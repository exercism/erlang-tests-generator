-module('tgen_pythagorean-triplet').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := Exp, property := Prop, input := #{n := Limit}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Exp1 = [{A, B, C} || [A, B, C] <- Exp],
    Exp2 = lists:sort(Exp1),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(Exp2),
                tgs:call_fun("lists:sort", [
                    tgs:call_fun("pythagorean_triplet:" ++ Property, [
                        tgs:value(Limit)
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Limit"]}]}.
