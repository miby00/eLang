%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2016, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2016 11:56
%%%-------------------------------------------------------------------
-author("Mikael Bylund <mikael.bylund@gmail.com").

-include("eLangSwedish.hrl").
-include("eLangEnglish.hrl").

-define(tr(Text),         eLang:tr(Text)).
-define(tr(Lang, Text),   eLang:tr(Lang, Text)).

-define(tu(Text),         eLang:tu(Text)).
-define(tu(Lang, Text),   eLang:tu(Lang, Text)).

-define(trDB(Text),       eLang:trDB(Text)).
-define(trDB(Lang, Text), eLang:trDB(Lang, Text)).

-define(tuDB(Text),       eLang:tuDB(Text)).
-define(tuDB(Lang, Text), eLang:tuDB(Lang, Text)).

-define(languages, [{swedish, {?swedishWX, ?swedishDef, ?swedishToDB}},
                    {english, {?englishWX, ?englishDef, ?englishToDB}}]).

-define(defLang, english).

