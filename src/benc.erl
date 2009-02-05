%%%-------------------------------------------------------------------
%%% Description: Parse bencoded files (.torrent)
%%%
%%%              Hashes consist of {Key, Value, Hash}, because the
%%%              SHA1 hash of the value of the info element is used as
%%%              the BitTorrent info_hash.
%%%-------------------------------------------------------------------
-module(benc).
-export([parse_file/1, parse/1]).
-export([to_binary/1]).

%%% API %%%

parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    parse(Data).

parse(Data) ->
    crypto:start(),

    {Value, <<>>} = parse_value(Data),
    Value.

%%% parse_value - where data is being typed %%%

parse_value(<<$d, Remaining/binary>>) ->
    parse_dict([], Remaining);

parse_value(<<$l, Remaining/binary>>) ->
    parse_list([], Remaining);

parse_value(<<$i, Remaining/binary>>) ->
    parse_int("", Remaining);

parse_value(<<Z, Remaining/binary>>) when Z >= $0, Z =< $9 ->
    parse_str_len([Z], Remaining).

%%% parse_dict %%%

parse_dict(Dict, <<$e, Remaining/binary>>) ->
    {lists:reverse(Dict), Remaining};

parse_dict(Dict, Remaining) ->
    {Key, Remaining2} = parse_value(Remaining),
    {Value, Remaining3} = parse_value(Remaining2),
    {Remaining2only, _} = split_binary(Remaining2, size(Remaining2) - size(Remaining3)),
    parse_dict([{Key, Value} | Dict], Remaining3).

%%% parse_list %%%

parse_list(List, <<$e, Remaining/binary>>) ->
    {lists:reverse(List), Remaining};

parse_list(List, Remaining) ->
    {Value, Remaining2} = parse_value(Remaining),
    parse_list([Value | List], Remaining2).

%%% parse_int %%%

parse_int(IntS, <<$e, Remaining/binary>>) ->
    {Int, []} = string:to_integer(lists:reverse(IntS)),
    {Int, Remaining};

parse_int("", <<$-, Remaining/binary>>) ->
    parse_int([$-], Remaining);

parse_int(IntS, <<Z, Remaining/binary>>) when Z >= $0, Z =< $9 ->
    parse_int([Z | IntS], Remaining).

%%% parse_str* %%%

parse_str_len(LenS, <<Z, Remaining/binary>>) when Z >= $0, Z =< $9 ->
    parse_str_len([Z | LenS], Remaining);

parse_str_len(LenS, <<$:, Remaining/binary>>) ->
    {Len, []} = string:to_integer(lists:reverse(LenS)),
    parse_str(Len, "", Remaining).

parse_str(0, Str, Remaining) ->
    {list_to_binary(lists:reverse(Str)), Remaining};

parse_str(Len, Str, <<C, Remaining/binary>>) ->
    parse_str(Len - 1, [C | Str], Remaining).

%%%%%%%%%%%%%%%%%%%
%% Serialization
%%%%%%%%%%%%%%%%%%%

to_binary([{_, _} | _] = Dict) ->
    Elements = [[to_binary(K), to_binary(V)] || {K, V} <- Dict],
    Bin = list_to_binary(Elements),
    list_to_binary(["d", Bin, "e"]);

to_binary(List) when is_list(List) ->
    Elements = [to_binary(E) || E <- List],
    Bin = list_to_binary(Elements),
    list_to_binary(["l", Bin, "e"]);

to_binary(I) when is_integer(I) ->
    Bin = list_to_binary(integer_to_list(I)),
    list_to_binary(["i", Bin, "e"]);

to_binary(<<String/binary>>) ->
    Length = list_to_binary(integer_to_list(size(String))),
    list_to_binary([Length, $:, String]).

     
