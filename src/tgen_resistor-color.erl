-module('tgen_resistor-color').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := Exp, property := Prop = <<"colorCode">>, input := #{color := Color}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(Exp),
                tgs:call_fun("resistor_color:" ++ Property, [
                    tgs:value(binary_to_atom(Color, latin1))])])])]),

    {ok, Fn, [{Property, ["Color"]}]};
generate_test(N, #{description := Desc, expected := Exp, property := Prop = <<"colors">>, input := #{}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value([binary_to_atom(C, latin1) || C <- Exp]),
                tgs:call_fun("resistor_color:" ++ Property, [])])])]),

    {ok, Fn, [{Property, []}]}.
