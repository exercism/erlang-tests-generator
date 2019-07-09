-module('tgen_allergies').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 2.

generate_test(N, #{description := Desc, expected := Exp, property := <<"list">>, input := #{score := Score}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = "allergies",

    Exp1=
    lists:sort(
        lists:map(
            fun
                (Substance) -> binary_to_atom(string:lowercase(Substance), latin1)
            end,
            Exp
        )
    ),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:value(binary_to_list(Desc)),
            tgs:call_macro("_assertMatch", [
                tgs:value(Exp1),
                tgs:call_fun("lists:sort", [
                    tgs:call_fun("allergies:" ++ Property, [
                        tgs:value(Score)])])])])]),

    {ok, Fn, [{Property, ["Score"]}]};

generate_test(N, #{description := Desc, expected := Exp, property := <<"allergicTo">>, input := #{item := Item, score := Score}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = "is_allergic_to",

    Assert = "_" ++ case Exp of
        true  -> "assert";
        false -> "assertNot"
    end,

    Fn=
    tgs:simple_fun(
        TestName ++ "_", [
            tgs:assign(tgs:var("Score"), tgs:value(Score)),
            erl_syntax:tuple([
                tgs:string(Desc),
                tgs:call_macro(Assert, [
                    tgs:call_fun("allergies:" ++ Property, [
                        tgs:value(binary_to_atom(string:lowercase(Item), latin1)),
                        tgs:var("Score")
                    ])
                ])
            ])
        ]
    ),

    {ok, Fn, [{Property, ["Substance", "Score"]}]}.
