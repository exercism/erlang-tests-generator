-module('tgen_queen-attack').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := Exp, property := Prop = <<"canAttack">>, input := #{white_queen := #{position := #{row := WRow, column := WCol}}, black_queen := #{position := #{row := BRow, column := BCol}}}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Assert = "_" ++ case Exp of
        true -> "assert";
        false -> "assertNot"
    end,

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro(Assert, [
                tgs:call_fun("queen_attack:" ++ Property, [
                    tgs:value({WCol, WRow}), tgs:value({BCol, BRow})])])])]),

    {ok, Fn, [{Property, ["WhiteQueen", "BlackQueen"]}]};
generate_test(_, _) ->
    ignore.
