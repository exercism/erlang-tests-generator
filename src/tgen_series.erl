-module('tgen_series').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := #{error := _}, property := Prop, input := #{series := Series, sliceLength := SliceLength}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertError", [
                tgs:var("_"),
                tgs:call_fun("series:" ++ Property, [
                    tgs:value(SliceLength), tgs:value(binary_to_list(Series))])])])]),

    {ok, Fn, [{Property, ["SliceLength", "Series"]}]};

generate_test(N, #{description := Desc, expected := Exp, property := Prop, input := #{series := Series, sliceLength := SliceLength}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value([binary_to_list(E) || E <- Exp]),
                tgs:call_fun("series:" ++ Property, [
                    tgs:value(SliceLength), tgs:value(binary_to_list(Series))])])])]),

    {ok, Fn, [{Property, ["SliceLength", "Series"]}]}.
