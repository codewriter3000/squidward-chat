-module(squidward_chat_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PORT, 8080).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [
        binary,
        {packet, http_bin},
        {active, false},
        {reuseaddr, true}
    ]),
    io:format("Server listening on port ~p~n", [?PORT]),
    spawn_link(fun() -> accept_loop(ListenSocket) end),
    {ok, #{listen_socket => ListenSocket}}.

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_loop(ListenSocket);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            timer:sleep(100),
            accept_loop(ListenSocket)
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            Headers = receive_headers(Socket, []),
            Body = receive_body(Socket, Headers),
            Response = handle_request(Method, Path, Headers, Body),
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);
        {error, _Reason} ->
            gen_tcp:close(Socket)
    end.

receive_headers(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_header, _, Name, _, Value}} ->
            receive_headers(Socket, [{Name, Value} | Acc]);
        {ok, http_eoh} ->
            lists:reverse(Acc);
        _ ->
            lists:reverse(Acc)
    end.

receive_body(Socket, Headers) ->
    case proplists:get_value('Content-Length', Headers) of
        undefined -> <<>>;
        LengthBin ->
            Length = binary_to_integer(LengthBin),
            inet:setopts(Socket, [{packet, raw}]),
            case gen_tcp:recv(Socket, Length, 5000) of
                {ok, Data} -> Data;
                _ -> <<>>
            end
    end.

handle_request('POST', <<"/api/register">>, _Headers, Body) ->
    try
        Data = squidward_chat_json:decode(Body),
        Username = maps:get(<<"username">>, Data),
        Password = maps:get(<<"password">>, Data),
        case squidward_chat_auth:register_user(Username, Password) of
            {ok, registered} ->
                Resp = squidward_chat_json:encode(#{
                    success => true,
                    message => <<"User registered successfully">>
                }),
                http_response(200, "application/json", Resp);
            {error, user_exists} ->
                Resp = squidward_chat_json:encode(#{
                    success => false,
                    message => <<"Username already exists">>
                }),
                http_response(400, "application/json", Resp)
        end
    catch
        _:_ ->
            ErrResp = squidward_chat_json:encode(#{
                success => false,
                message => <<"Invalid request">>
            }),
            http_response(400, "application/json", ErrResp)
    end;

handle_request('POST', <<"/api/login">>, _Headers, Body) ->
    try
        Data = squidward_chat_json:decode(Body),
        Username = maps:get(<<"username">>, Data),
        Password = maps:get(<<"password">>, Data),
        case squidward_chat_auth:login_user(Username, Password) of
            {ok, Token, Username} ->
                Resp = squidward_chat_json:encode(#{
                    success => true,
                    token => Token,
                    username => Username
                }),
                http_response(200, "application/json", Resp);
            {error, invalid_credentials} ->
                Resp = squidward_chat_json:encode(#{
                    success => false,
                    message => <<"Invalid username or password">>
                }),
                http_response(401, "application/json", Resp)
        end
    catch
        _:_ ->
            ErrResp = squidward_chat_json:encode(#{
                success => false,
                message => <<"Invalid request">>
            }),
            http_response(400, "application/json", ErrResp)
    end;

handle_request('POST', <<"/api/messages/send">>, Headers, Body) ->
    case get_auth_token(Headers) of
        undefined ->
            Resp = squidward_chat_json:encode(#{success => false, message => <<"Unauthorized">>}),
            http_response(401, "application/json", Resp);
        Token ->
            case squidward_chat_auth:verify_token(Token) of
                {ok, Username} ->
                    try
                        Data = squidward_chat_json:decode(Body),
                        Message = maps:get(<<"message">>, Data),
                        Timestamp = erlang:system_time(millisecond),
                        squidward_chat_room:broadcast_message(Username, Message, Timestamp),
                        Resp = squidward_chat_json:encode(#{success => true}),
                        http_response(200, "application/json", Resp)
                    catch
                        _:_ ->
                            ErrResp = squidward_chat_json:encode(#{success => false, message => <<"Invalid request">>}),
                            http_response(400, "application/json", ErrResp)
                    end;
                {error, _} ->
                    Resp = squidward_chat_json:encode(#{success => false, message => <<"Invalid token">>}),
                    http_response(401, "application/json", Resp)
            end
    end;

handle_request('GET', <<"/api/messages">>, Headers, _Body) ->
    case get_auth_token(Headers) of
        undefined ->
            Resp = squidward_chat_json:encode(#{success => false, message => <<"Unauthorized">>}),
            http_response(401, "application/json", Resp);
        Token ->
            case squidward_chat_auth:verify_token(Token) of
                {ok, _Username} ->
                    Messages = squidward_chat_room:get_recent_messages(),
                    Resp = squidward_chat_json:encode(#{success => true, messages => Messages}),
                    http_response(200, "application/json", Resp);
                {error, _} ->
                    Resp = squidward_chat_json:encode(#{success => false, message => <<"Invalid token">>}),
                    http_response(401, "application/json", Resp)
            end
    end;

handle_request('GET', Path, _Headers, _Body) ->
    serve_static_file(Path);

handle_request(_, _, _, _) ->
    http_response(404, "text/plain", "Not Found").

get_auth_token(Headers) ->
    case proplists:get_value('Authorization', Headers) of
        undefined -> undefined;
        <<"Bearer ", Token/binary>> -> Token;
        _ -> undefined
    end.

http_response(Code, ContentType, Body) when is_list(Body) ->
    http_response(Code, ContentType, list_to_binary(Body));
http_response(Code, ContentType, Body) ->
    StatusText = http_status_text(Code),
    [
        <<"HTTP/1.1 ">>, integer_to_binary(Code), <<" ">>, StatusText, <<"\r\n">>,
        <<"Content-Type: ">>, ContentType, <<"\r\n">>,
        <<"Content-Length: ">>, integer_to_binary(byte_size(Body)), <<"\r\n">>,
        <<"Access-Control-Allow-Origin: *\r\n">>,
        <<"Connection: close\r\n">>,
        <<"\r\n">>,
        Body
    ].

http_status_text(200) -> <<"OK">>;
http_status_text(400) -> <<"Bad Request">>;
http_status_text(401) -> <<"Unauthorized">>;
http_status_text(404) -> <<"Not Found">>;
http_status_text(500) -> <<"Internal Server Error">>;
http_status_text(_) -> <<"Unknown">>.

serve_static_file(<<"/", Path/binary>>) ->
    serve_static_file(Path);
serve_static_file(<<"">>) ->
    serve_static_file(<<"index.html">>);
serve_static_file(Path) ->
    PrivDir = code:priv_dir(squidward_chat),
    FilePath = filename:join([PrivDir, "static", binary_to_list(Path)]),
    case file:read_file(FilePath) of
        {ok, Content} ->
            ContentType = get_content_type(Path),
            http_response(200, ContentType, Content);
        {error, _} ->
            http_response(404, "text/plain", "File Not Found")
    end.

get_content_type(Path) ->
    case filename:extension(binary_to_list(Path)) of
        ".html" -> "text/html";
        ".css" -> "text/css";
        ".js" -> "application/javascript";
        ".json" -> "application/json";
        ".png" -> "image/png";
        ".jpg" -> "image/jpeg";
        ".jpeg" -> "image/jpeg";
        ".gif" -> "image/gif";
        ".svg" -> "image/svg+xml";
        _ -> "text/plain"
    end.

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
