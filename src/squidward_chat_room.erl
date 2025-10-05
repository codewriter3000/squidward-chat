-module(squidward_chat_room).
-behaviour(gen_server).

%% API
-export([start_link/0, broadcast_message/3, get_recent_messages/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_MESSAGES, 100).

-record(state, {
    messages = [] :: list()
}).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

broadcast_message(FromUsername, Message, Timestamp) ->
    gen_server:cast(?MODULE, {broadcast, FromUsername, Message, Timestamp}).

get_recent_messages() ->
    gen_server:call(?MODULE, get_messages).

%% gen_server Callbacks

init([]) ->
    {ok, #state{}}.

handle_call(get_messages, _From, State = #state{messages = Messages}) ->
    {reply, lists:reverse(Messages), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({broadcast, FromUsername, Message, Timestamp}, State = #state{messages = Messages}) ->
    NewMessage = #{
        type => message,
        username => FromUsername,
        message => Message,
        timestamp => Timestamp
    },
    %% Keep only the last MAX_MESSAGES messages
    NewMessages = lists:sublist([NewMessage | Messages], ?MAX_MESSAGES),
    {noreply, State#state{messages = NewMessages}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
