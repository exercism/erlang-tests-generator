-module('tgen_book-store').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := Exp, property := Prop, input := #{basket := Basket}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
		tgs:value(Exp),
                tgs:call_fun("book_store:" ++ Property, [
                    tgs:value(Basket)])])])]),

    {ok, Fn, [{Property, ["Basket"]}]}.
