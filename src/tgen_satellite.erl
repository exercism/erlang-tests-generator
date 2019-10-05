-module('tgen_satellite').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := #{error := _}, property := Prop, input := #{preorder := PreOrder, inorder := InOrder}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    PreOrder1 = normalize_input(PreOrder),
    InOrder1 = normalize_input(InOrder),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertError", [
                tgs:var("_"),
                tgs:call_fun("satellite:" ++ Property, [
                    tgs:value(PreOrder1), tgs:value(InOrder1)])])])]),

    {ok, Fn, [{Property, ["PreOrder", "InOrder"]}]};
generate_test(N, #{description := Desc, expected := Exp, property := Prop, input := #{preorder := PreOrder, inorder := InOrder}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    PreOrder1 = normalize_input(PreOrder),
    InOrder1 = normalize_input(InOrder),
    Exp1=normalize_exp(Exp),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
		tgs:value(Exp1),
                tgs:call_fun("satellite:" ++ Property, [
                    tgs:value(PreOrder1), tgs:value(InOrder1)])])])]),

    {ok, Fn, [{Property, ["PreOrder", "InOrder"]}]}.

normalize_input(Input) ->
    [binary_to_list(I) || I <- Input].

normalize_exp(Exp) when is_binary(Exp) ->
    binary_to_list(Exp);
normalize_exp(Exp) when is_map(Exp) ->
    maps:fold(
        fun
            (K, V, Acc) when is_binary(K) ->
                Acc#{binary_to_atom(K, latin1) => normalize_exp(V)};
            (K, V, Acc) when is_atom(K) ->
                Acc#{K => normalize_exp(V)}
        end,
        #{},
        Exp
    );
normalize_exp(Exp) ->
    Exp.
