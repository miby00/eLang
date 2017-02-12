-module(eLang_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, [Module]) ->
    eLang_sup:start_link(Module).

stop(_State) ->
    ok.
