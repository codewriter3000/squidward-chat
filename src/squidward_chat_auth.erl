-module(squidward_chat_auth).
-behaviour(gen_server).

-export([start_link/0, exchange_oauth_code/2, verify_token/1, logout/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(OAUTH_BASE_URL, "http://10.0.0.2:8001").

-record(state, {
    sessions = #{} :: map()
}).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Called from request handler processes — performs the OAuth code exchange
%% then stores the resulting session in the gen_server state.
exchange_oauth_code(Code, RedirectUri) ->
    TokenUrl = ?OAUTH_BASE_URL ++ "/oauth/token",
    ClientId = application:get_env(squidward_chat, oauth_client_id, "squidward-chat"),
    ClientSecret = application:get_env(squidward_chat, oauth_client_secret, ""),
    FormBody = form_encode([
        {"grant_type", "authorization_code"},
        {"code", binary_to_list(Code)},
        {"redirect_uri", binary_to_list(RedirectUri)}
    ]),
    Credentials = binary_to_list(base64:encode(ClientId ++ ":" ++ ClientSecret)),
    AuthHeader = "Basic " ++ Credentials,
    case httpc:request(post,
            {TokenUrl,
             [{"Authorization", AuthHeader}],
             "application/x-www-form-urlencoded",
             FormBody},
            [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            TokenData = squidward_chat_json:decode(list_to_binary(ResponseBody)),
            case maps:get(<<"access_token">>, TokenData, undefined) of
                undefined ->
                    {error, no_access_token};
                AccessToken ->
                    case fetch_userinfo(AccessToken) of
                        {ok, Username} ->
                            SessionToken = generate_session_token(Username),
                            gen_server:call(?MODULE, {store_session, SessionToken, Username}),
                            {ok, SessionToken, Username};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {ok, {{_, StatusCode, _}, _Headers, ResponseBody}} ->
            io:format("OAuth token exchange failed (~p): ~s~n", [StatusCode, ResponseBody]),
            {error, token_exchange_failed};
        {error, Reason} ->
            io:format("OAuth httpc error: ~p~n", [Reason]),
            {error, Reason}
    end.

verify_token(Token) ->
    gen_server:call(?MODULE, {verify_session, Token}).

logout(Token) ->
    gen_server:call(?MODULE, {remove_session, Token}).

%% gen_server Callbacks

init([]) ->
    {ok, #state{}}.

handle_call({store_session, Token, Username}, _From, State = #state{sessions = Sessions}) ->
    NewSessions = maps:put(Token, Username, Sessions),
    {reply, ok, State#state{sessions = NewSessions}};

handle_call({verify_session, Token}, _From, State = #state{sessions = Sessions}) ->
    case maps:get(Token, Sessions, undefined) of
        undefined -> {reply, {error, invalid_token}, State};
        Username  -> {reply, {ok, Username}, State}
    end;

handle_call({remove_session, Token}, _From, State = #state{sessions = Sessions}) ->
    NewSessions = maps:remove(Token, Sessions),
    {reply, ok, State#state{sessions = NewSessions}};

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

fetch_userinfo(AccessToken) ->
    UserinfoUrl = ?OAUTH_BASE_URL ++ "/oauth/userinfo",
    BearerHeader = "Bearer " ++ binary_to_list(AccessToken),
    case httpc:request(get,
            {UserinfoUrl, [{"Authorization", BearerHeader}]},
            [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            UserData = squidward_chat_json:decode(list_to_binary(ResponseBody)),
            Username = maps:get(<<"username">>, UserData,
                        maps:get(<<"preferred_username">>, UserData,
                            maps:get(<<"sub">>, UserData, undefined))),
            case Username of
                undefined -> {error, no_username_in_userinfo};
                _         -> {ok, Username}
            end;
        {ok, {{_, StatusCode, _}, _Headers, ResponseBody}} ->
            io:format("Userinfo request failed (~p): ~s~n", [StatusCode, ResponseBody]),
            {error, userinfo_failed};
        {error, Reason} ->
            {error, Reason}
    end.

generate_session_token(Username) ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    Data = io_lib:format("~s-~p-~p", [Username, Timestamp, Random]),
    base64:encode(crypto:hash(sha256, Data)).

form_encode(Params) ->
    Parts = [percent_encode(K) ++ "=" ++ percent_encode(V) || {K, V} <- Params],
    string:join(Parts, "&").

percent_encode(S) ->
    lists:flatmap(fun encode_char/1, S).

encode_char(C) when C >= $a, C =< $z -> [C];
encode_char(C) when C >= $A, C =< $Z -> [C];
encode_char(C) when C >= $0, C =< $9 -> [C];
encode_char($-) -> [$-];
encode_char($_) -> [$_];
encode_char($.) -> [$.];
encode_char($~) -> [$~];
encode_char(C) ->
    Hex = integer_to_list(C, 16),
    case Hex of
        [D]      -> [$%, $0, D];
        [D1, D2] -> [$%, D1, D2]
    end.
