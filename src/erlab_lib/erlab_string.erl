-module(erlab_string).
-export([tokens/2]).
-export([tokens_benchmark/0]).


-spec tokens(String, SeparatorList) -> Tokens when
      String :: string(),
      SeparatorList :: string(),
      Tokens :: [Token :: nonempty_string()].

tokens(String, SepList) ->
    case erlang:length(SepList) < 999 of
    true ->
        string:tokens(String, SepList);
    false ->
        SepMaps = maps:from_list([{K,undef} || K <- SepList]),
        tokens_multiple_1(lists:reverse(String), SepMaps, [])
    end.

tokens_multiple_1([C|S], SepMaps, Toks) ->
    case maps:is_key(C, SepMaps) of
    true -> tokens_multiple_1(S, SepMaps, Toks);
    false -> tokens_multiple_2(S, SepMaps, Toks, [C])
    end;
tokens_multiple_1([], _SepMaps, Toks) ->
    Toks.

tokens_multiple_2([C|S], SepMaps, Toks, Tok) ->
    case maps:is_key(C, SepMaps) of
    true -> tokens_multiple_1(S, SepMaps, [Tok|Toks]);
    false -> tokens_multiple_2(S, SepMaps, Toks, [C|Tok])
    end;
tokens_multiple_2([], _SepMaps, Toks, Tok) ->
    [Tok|Toks].



%%%============================================================================
%%% Test functions
%%%============================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tokens_test() ->
    ?assertEqual(tokens_test_erlab(), tokens_test_stdlib()).

-endif.


tokens_benchmark() ->
    {Terlab,_} = timer:tc(fun() -> lists:foreach(fun(_) ->
        tokens_test_erlab()
    end, lists:seq(1, 10000)) end),
    {Tstdlib,_} = timer:tc(fun() -> lists:foreach(fun(_) ->
        tokens_test_stdlib()
    end, lists:seq(1, 10000)) end),
    io:format("Terlab : ~w~n", [Terlab]),
    io:format("Tstdlib: ~w~n", [Tstdlib]).


tokens_test_erlab() ->
    [?MODULE:tokens(String, SepList) || [String, SepList]
                                <- tokens_test_cases()].


tokens_test_stdlib() ->
    [string:tokens(String, SepList) || [String, SepList]
                                <- tokens_test_cases()].


tokens_test_cases() ->
[
     ["1,2 34,45;5,;6;,7", ";,"]
    ,["", ""]
    ,["abc", "abc"]
    ,["abc", ""]
    ,[lists:duplicate(2000, $a), lists:duplicate(1000, $b)]
    ,[lists:duplicate(2000, $a), lists:seq(1, 1000)]
].
