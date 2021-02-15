-module('tgen_pascals-triangle').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{count := Count}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value(Exp),
                tgs:call_fun("pascals_triangle:" ++ Property, [
                    tgs:value(Count)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Count"]}]}.
