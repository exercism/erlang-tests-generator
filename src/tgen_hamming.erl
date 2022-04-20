-module('tgen_hamming').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{
    description := Desc,
    expected := #{error := _Message},
    property := Prop,
    input := #{strand1 := S1, strand2 := S2}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value({error, badarg}),
                tgs:call_fun("hamming:" ++ Property, [
                    tgs:value(binary_to_list(S1)),
                    tgs:value(binary_to_list(S2))
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Strand1", "Strand2"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{strand1 := S1, strand2 := S2}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value(Exp),
                tgs:call_fun("hamming:" ++ Property, [
                    tgs:value(binary_to_list(S1)),
                    tgs:value(binary_to_list(S2))
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Strand1", "Strand2"]}]}.
