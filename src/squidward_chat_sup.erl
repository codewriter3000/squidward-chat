-module(squidward_chat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    
    ChildSpecs = [
        #{id => squidward_chat_auth,
          start => {squidward_chat_auth, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [squidward_chat_auth]},
        #{id => squidward_chat_room,
          start => {squidward_chat_room, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [squidward_chat_room]},
        #{id => squidward_chat_server,
          start => {squidward_chat_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [squidward_chat_server]}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
