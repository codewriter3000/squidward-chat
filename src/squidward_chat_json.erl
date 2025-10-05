-module(squidward_chat_json).
-export([encode/1, decode/1]).

%% Simple JSON encoder for basic maps
encode(Map) when is_map(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        Key = encode_key(K),
        Value = encode_value(V),
        [io_lib:format("~s:~s", [Key, Value]) | Acc]
    end, [], Map),
    ["{", string:join(lists:reverse(Pairs), ","), "}"].

encode_key(K) when is_atom(K) -> encode_string(atom_to_binary(K, utf8));
encode_key(K) when is_binary(K) -> encode_string(K);
encode_key(K) when is_list(K) -> encode_string(list_to_binary(K)).

encode_value(V) when is_binary(V) -> encode_string(V);
encode_value(true) -> "true";
encode_value(false) -> "false";
encode_value(null) -> "null";
encode_value(V) when is_atom(V) -> encode_string(atom_to_binary(V, utf8));
encode_value(V) when is_integer(V) -> integer_to_list(V);
encode_value(V) when is_float(V) -> float_to_list(V);
encode_value(V) when is_list(V) -> 
    case io_lib:printable_unicode_list(V) of
        true -> encode_string(list_to_binary(V));
        false -> 
            %% Assume it's a list of values (array)
            Values = [encode_value(Item) || Item <- V],
            ["[", string:join(Values, ","), "]"]
    end;
encode_value(V) when is_map(V) ->
    encode(V).

encode_string(Bin) when is_binary(Bin) ->
    [$", escape_string(binary_to_list(Bin)), $"].

escape_string([]) -> [];
escape_string([$" | Rest]) -> [$\\, $" | escape_string(Rest)];
escape_string([$\\ | Rest]) -> [$\\, $\\ | escape_string(Rest)];
escape_string([C | Rest]) -> [C | escape_string(Rest)].

%% Simple JSON decoder
decode(Binary) when is_binary(Binary) ->
    decode(binary_to_list(Binary));
decode(String) when is_list(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    parse_json(Tokens).

parse_json([{'{', _} | Rest]) ->
    {Map, _} = parse_object(Rest, #{}),
    Map;
parse_json(_) ->
    #{}.

parse_object([{'}', _} | Rest], Acc) ->
    {Acc, Rest};
parse_object([{atom, _, Key} | [{':' , _} | Rest]], Acc) ->
    {Value, Rest2} = parse_value(Rest),
    NewAcc = maps:put(atom_to_binary(Key, utf8), Value, Acc),
    case Rest2 of
        [{',', _} | Rest3] -> parse_object(Rest3, NewAcc);
        _ -> parse_object(Rest2, NewAcc)
    end;
parse_object([{string, _, Key} | [{':' , _} | Rest]], Acc) ->
    {Value, Rest2} = parse_value(Rest),
    NewAcc = maps:put(list_to_binary(Key), Value, Acc),
    case Rest2 of
        [{',', _} | Rest3] -> parse_object(Rest3, NewAcc);
        _ -> parse_object(Rest2, NewAcc)
    end;
parse_object([_ | Rest], Acc) ->
    parse_object(Rest, Acc).

parse_value([{string, _, Value} | Rest]) ->
    {list_to_binary(Value), Rest};
parse_value([{integer, _, Value} | Rest]) ->
    {Value, Rest};
parse_value([{atom, _, true} | Rest]) ->
    {true, Rest};
parse_value([{atom, _, false} | Rest]) ->
    {false, Rest};
parse_value([{atom, _, null} | Rest]) ->
    {null, Rest};
parse_value([_ | Rest]) ->
    {null, Rest}.
