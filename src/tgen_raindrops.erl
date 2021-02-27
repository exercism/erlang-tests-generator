-module(tgen_raindrops).

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
    input := #{number := Num}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(binary_to_list(Exp)),
                tgs:call_fun("raindrops:" ++ Property, [
                    tgs:value(Num)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Prop, ["Number"]}]}.
