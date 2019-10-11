-module('tgen_nth-prime').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := #{error := _}, property := Prop, input := #{number := Number}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertError", [
                tgs:var("_"),
                tgs:call_fun("nth_prime:" ++ Property, [
                    tgs:value(Number)])])])]),

    {ok, Fn, [{Property, ["N"]}]};
generate_test(N, #{description := Desc, expected := Exp, property := Prop, input := #{number := Number}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(Exp),
                tgs:call_fun("nth_prime:" ++ Property, [
                    tgs:value(Number)])])])]),

    {ok, Fn, [{Property, ["N"]}]}.
