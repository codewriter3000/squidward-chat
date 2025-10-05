-module(squidward_chat_auth).
-behaviour(gen_server).

%% API
-export([start_link/0, register_user/2, login_user/2, verify_token/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    users = #{} :: map(),
    tokens = #{} :: map()
}).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_user(Username, Password) ->
    gen_server:call(?MODULE, {register, Username, Password}).

login_user(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

verify_token(Token) ->
    gen_server:call(?MODULE, {verify_token, Token}).

%% gen_server Callbacks

init([]) ->
    {ok, #state{}}.

handle_call({register, Username, Password}, _From, State = #state{users = Users}) ->
    case maps:is_key(Username, Users) of
        true ->
            {reply, {error, user_exists}, State};
        false ->
            %% In production, password should be hashed (e.g., with bcrypt)
            %% This is a minimal implementation
            HashedPassword = crypto:hash(sha256, Password),
            NewUsers = maps:put(Username, HashedPassword, Users),
            {reply, {ok, registered}, State#state{users = NewUsers}}
    end;

handle_call({login, Username, Password}, _From, State = #state{users = Users, tokens = Tokens}) ->
    case maps:get(Username, Users, undefined) of
        undefined ->
            {reply, {error, invalid_credentials}, State};
        StoredHash ->
            HashedPassword = crypto:hash(sha256, Password),
            case HashedPassword =:= StoredHash of
                true ->
                    %% Generate a simple token (in production, use proper JWT)
                    Token = generate_token(Username),
                    NewTokens = maps:put(Token, Username, Tokens),
                    {reply, {ok, Token, Username}, State#state{tokens = NewTokens}};
                false ->
                    {reply, {error, invalid_credentials}, State}
            end
    end;

handle_call({verify_token, Token}, _From, State = #state{tokens = Tokens}) ->
    case maps:get(Token, Tokens, undefined) of
        undefined ->
            {reply, {error, invalid_token}, State};
        Username ->
            {reply, {ok, Username}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

generate_token(Username) ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    Data = io_lib:format("~s-~p-~p", [Username, Timestamp, Random]),
    base64:encode(crypto:hash(sha256, Data)).
