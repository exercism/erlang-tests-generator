-module('tgen_rail-fence-cipher').

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
    input := #{msg := Message, rails := Rails}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value(binary_to_list(Exp)),
                tgs:call_fun("rail_fence_cipher:" ++ Property, [
                    tgs:value(binary_to_list(Message)),
                    tgs:value(Rails)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Message", "Rails"]}]}.
