-module('tgen_largest-series-product').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{
    description := Desc,
    expected := #{error := _},
    property := Prop,
    input := #{digits := Digits, span := Span}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertError", [
                tgs:raw("_"),
                tgs:call_fun("largest_series_product:" ++ Property, [
                    tgs:value(binary_to_list(Digits)),
                    tgs:value(Span)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Digits", "Span"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{digits := Digits, span := Span}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(Exp),
                tgs:call_fun("largest_series_product:" ++ Property, [
                    tgs:value(binary_to_list(Digits)),
                    tgs:value(Span)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Digits", "Span"]}]}.
