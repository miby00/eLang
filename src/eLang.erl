%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2016, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2016 11:56
%%%-------------------------------------------------------------------
-module(eLang).
-author("Mikael Bylund <mikael.bylund@gmail.com").

-behaviour(gen_server).

%% API
-export([start_link/1,
         tr/1,
         tr/2,
         trDB/1,
         trDB/2,
         tu/1,
         tu/2,
         tuDB/1,
         tuDB/2,
         setLanguage/2,
         setWXEnv/1,
         setLANGEnv/1,
         getLanguage/0,
         initiateGUI/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eLang/include/eLang.hrl").
-include_lib("wx/include/wx.hrl").

-define(SERVER, ?MODULE).
-define(eLang, "eLangLanguage").

-record(state, {language = ?defLang, callback}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(CallBackModule :: atom()) ->   {ok, Pid :: pid()} |
                                                ignore |
                                                {error, Reason :: term()}.
start_link(CallBackModule) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CallBackModule], []).

-spec tr(LanguageString :: string()) -> Translation :: string().
tr(LanguageString) ->
    case get(?eLang) of
        undefined ->
            gen_server:call(?MODULE, {translate, LanguageString});
        Lang ->
            tr(Lang, LanguageString)
    end.

-spec tr(Language :: atom(), Text :: string()) -> Translation :: string().
tr(Language, Text) ->
    {D1, D2, _D3} = proplists:get_value(Language, ?languages, {#{}, #{}, #{}}),
    maps:get(Text, D1, maps:get(Text, D2, toStr(Text))).

-spec tu(LanguageString :: string()) -> Translation :: string().
tu(LanguageString) ->
    toUtf8(tr(LanguageString)).

-spec tu(Language :: atom(), Text :: string()) -> Translation :: string().
tu(Language, Text) ->
    toUtf8(tr(Language, Text)).

-spec trDB(LanguageString :: string()) -> atom().
trDB(LanguageString) ->
    case get(?eLang) of
        undefined ->
            gen_server:call(?MODULE, {trDB, LanguageString});
        Lang ->
            trDB(Lang, LanguageString)
    end.

-spec trDB(LanguageString :: string(), Text :: string()) -> atom().
trDB(LANG, Text) ->
    {_D1, _D2, D3} = proplists:get_value(LANG, ?languages, {#{}, #{}, #{}}),
    maps:get(Text, D3, Text).

-spec tuDB(LanguageString :: string()) -> atom().
tuDB(LanguageString) ->
    trDB(utf8ToLatin1(LanguageString)).

-spec tuDB(LanguageString :: string(), Text :: string()) -> atom().
tuDB(LANG, Text) ->
    trDB(LANG, utf8ToLatin1(Text)).

-spec setLanguage(LANG :: atom(), GUIState :: term()) -> ok.
setLanguage(LANG, GUIState) ->
    io:format("setLanguage: ~p~n", [LANG]),
    case proplists:get_value(LANG, ?languages) of
        undefined ->
            ok;
        _ ->
            gen_server:cast(?MODULE, {setLanguage, LANG, GUIState})
    end.

-spec getLanguage() -> atom().
getLanguage() ->
    case get(?eLang) of
        undefined ->
            gen_server:call(?MODULE, getLanguage);
        Lang ->
            Lang
    end.

-spec setWXEnv(WXEnv :: term()) -> ok.
setWXEnv(WXEnv) ->
    gen_server:cast(?MODULE, {setWXEnv, WXEnv}).

-spec setLANGEnv(LANG :: atom()) -> ok.
setLANGEnv(LANG) ->
    put(?eLang, LANG).

-spec initiateGUI(GUIState :: term()) -> ok.
initiateGUI(GUIState) ->
    gen_server:cast(?MODULE, {initiateGUI, GUIState}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
             {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term()} | ignore).
init([CallBackModule]) ->
    {ok, #state{callback = CallBackModule}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_call({translate, LangString}, _From, State) ->
    Translation = tr(State#state.language, LangString),
    {reply, Translation, State};
handle_call({trDB, LangString}, _From, State) ->
    DBName = trDB(State#state.language, LangString),
    {reply, DBName, State};
handle_call(getLanguage, _From, State) ->
    {reply, State#state.language, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({setLanguage, Language, _GUIState}, State = #state{language = Language}) ->
    {noreply, State};
handle_cast({setLanguage, Language, GUIState}, State = #state{callback = CBModule}) ->
    case proplists:get_value(Language, ?languages) of
        undefined ->
            {noreply, State};
        _ ->
            setLANGEnv(Language),
            CBModule:eLangLanguageChanged(Language, GUIState),
            {noreply, State#state{language = Language}}
    end;
handle_cast({setWXEnv, WXEnv}, State) ->
    wx:set_env(WXEnv),
    {noreply, State};
handle_cast({initiateGUI, GUIState},
            State = #state{language = Language, callback = CallBackModule}) ->
    {D1, _D2, _D3} = proplists:get_value(Language, ?languages, {#{}, #{}, #{}}),
    Components = maps:keys(D1),
    doInitiateGUI(Language, Components, CallBackModule, GUIState),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

doInitiateGUI(_Language, _Components, undefined, _GUIState) ->
    ok;
doInitiateGUI(_Language, [], _CBModule, _GUIState) ->
    ok;
doInitiateGUI(Language, Components, CBModule, GUIState) ->
    [translate(Language, Component, CBModule, GUIState) || Component <- Components],
    CBModule:eLangWxInitiateComplete(GUIState).

translate(Language, Component, CBModule, GUIState) ->
    TText = tr(Language, Component),
    try CBModule:eLangGetWXObject(Component, GUIState) of
        {Object, Type}      -> setTranslation(Type, Object, Component, TText);
        {Object, Type, Pos} -> setTranslation(Type, Object, Pos,       TText);
        _ ->
            ok
        catch
        Error:Reason ->
            io:format("~p:~p for component ~p~n", [Error, Reason, Component]),
            ok
    end.

setTranslation(wxMenu, Object, Data, Text) when is_list(Text) ->
    wxMenuBar:setLabelTop(Object, Data, Text);
setTranslation(wxMenuItem, Object, Data, Text) when is_list(Text) ->
    ID = wxXmlResource:getXRCID(Data),
    wxMenuBar:setLabel(Object, ID, Text);
setTranslation(tool, Object, Data, Text) when is_list(Text) ->
    ID       = wxXmlResource:getXRCID(Data),
    WXObject = wxToolBar:findById(Object, ID),
    wxToolBar:setLabel(WXObject, Text);
setTranslation(tool, Object, Data, Map = #{label := Label}) ->
    ID       = wxXmlResource:getXRCID(Data),
    WXObject = wxToolBar:findById(Object, ID),
    wxToolBar:setLabel(WXObject, Label),
    setTranslation(tool, Object, Data, maps:remove(label, Map));
setTranslation(tool, Object, Data, Map = #{tooltip := ToolTip}) ->
    ID = wxXmlResource:getXRCID(Data),
    wxToolBar:setToolShortHelp(Object, ID, ToolTip),
    setTranslation(tool, Object, Data, maps:remove(tooltip, Map));
setTranslation(tool, Object, Data, Map = #{longhelp := LHelp}) ->
    ID = wxXmlResource:getXRCID(Data),
    wxToolBar:setToolLongHelp(Object, ID, LHelp),
    setTranslation(tool, Object, Data, maps:remove(longhelp, Map));
setTranslation(wxStaticBoxSizer, Object, _Data, Text) when is_list(Text) ->
    SB = wxStaticBoxSizer:getStaticBox(Object),
    wxStaticBox:setLabel(SB, Text);
setTranslation(Type, Object, _Data, Text) when is_list(Text) ->
    Type:setLabel(Object, Text);
setTranslation(wxChoice, Object, Data, Map = #{contents := Items}) ->
    wxChoice:clear(Object),
    wxChoice:appendStrings(Object, Items),
    setTranslation(wxChoice, Object, Data, maps:remove(contents, Map));
setTranslation(wxCheckListBox, Object, Data, Map = #{contents := Items}) ->
    wxCheckListBox:clear(Object),
    wxCheckListBox:appendStrings(Object, Items),
    setTranslation(wxCheckListBox, Object, Data, maps:remove(contents, Map));
setTranslation(Type, Object, Data, Map = #{title := Title}) ->
    Type:setTitle(Object, Title),
    setTranslation(Type, Object, Data, maps:remove(title, Map));
setTranslation(Type, Object, Data, Map = #{tooltip := ToolTip}) ->
    Type:setToolTip(Object, ToolTip),
    setTranslation(Type, Object, Data, maps:remove(tooltip, Map));
setTranslation(_Type, _Object, _Data, #{}) ->
    ok.

toStr(undefined) -> "";
toStr(Value) when is_atom(Value) ->
    atom_to_list(Value);
toStr(Value) ->
    Value.

toUtf8(Text) when is_list(Text) ->
    toUtf8(list_to_binary(Text));
toUtf8(BinText) when is_binary(BinText) ->
    binary_to_list(unicode:characters_to_binary(BinText, latin1, utf8));
toUtf8(Value) ->
    Value.

utf8ToLatin1(Text) when is_list(Text) ->
    utf8ToLatin1(list_to_binary(Text));
utf8ToLatin1(BinText) when is_binary(BinText) ->
    binary_to_list(unicode:characters_to_binary(BinText, utf8, latin1));
utf8ToLatin1(Value) ->
    Value.
