-module('tgen_forth').

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
    input := #{instructions := Instructions}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertError", [
                tgs:raw("_"),
                tgs:call_fun("forth:" ++ Property, [
                    tgs:value([binary_to_list(I) || I <- Instructions])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Instructions"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{instructions := Instructions}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(Exp),
                tgs:call_fun("forth:" ++ Property, [
                    tgs:value([binary_to_list(I) || I <- Instructions])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Instructions"]}]};
generate_test(N, #{
    description := Desc,
    expected := [ExpFirst, ExpSecond],
    property := <<"evaluateBoth">> = Prop,
    input := #{instructionsFirst := InstructionsFirst, instructionsSecond := InstructionsSecond}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    First = tgs:call_macro("_assertEqual", [
        tgs:value(ExpFirst),
        tgs:call_fun("forth:" ++ Property, [
            tgs:value([binary_to_list(I) || I <- InstructionsFirst])
        ])
    ]),
    Second = tgs:call_macro("_assertEqual", [
        tgs:value(ExpSecond),
        tgs:call_fun("forth:" ++ Property, [
            tgs:value([binary_to_list(I) || I <- InstructionsSecond])
        ])
    ]),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:list([
            erl_syntax:tuple([tgs:string(<<Desc/binary, <<" (first)">>/binary>>), First]),
            erl_syntax:tuple([tgs:string(<<Desc/binary, <<" (second)">>/binary>>), Second])
        ])
    ]),

    {ok, Fn, [{Property, ["Instructions"]}]}.
