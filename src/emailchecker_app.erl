-module(emailchecker_app).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    emailchecker_sup:start_link().

stop(_State) ->
    ok.
