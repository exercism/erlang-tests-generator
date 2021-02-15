-module('tgen_collatz-conjecture').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 2.

generate_test(N, #{
    description := Desc,
    expected := #{error := _},
    property := Prop,
    input := #{number := Num}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertError", [
                tgs:value(badarg),
                tgs:call_fun("collatz_conjecture:" ++ Property, [
                    tgs:value(Num)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Prop, ["N"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{number := Num}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value(Exp),
                tgs:call_fun("collatz_conjecture:" ++ Property, [
                    tgs:value(Num)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Prop, ["N"]}]}.
