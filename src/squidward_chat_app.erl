-module(squidward_chat_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    squidward_chat_sup:start_link().

stop(_State) ->
    ok.
