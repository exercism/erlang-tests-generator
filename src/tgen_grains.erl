-module('tgen_grains').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{
    description := Desc,
    expected := #{error := Message},
    property := Prop = <<"square">>,
    input := #{square := Square}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value({error, binary_to_list(Message)}),
                tgs:call_fun("grains:square", [
                    tgs:value(Square)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Square"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop = <<"square">>,
    input := #{square := Square}
}) ->
    TestName = tgen:to_test_name(N, <<"square_", Desc/binary>>),
    Descript = <<"Square ", Desc/binary>>,
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Descript),
            tgs:call_macro("_assertMatch", [
                tgs:value(Exp),
                tgs:call_fun("grains:square", [
                    tgs:value(Square)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Square"]}]};
generate_test(N, #{description := Desc, expected := Exp, property := Prop = <<"total">>}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value(Exp),
                tgs:call_fun("grains:total", [])
            ])
        ])
    ]),

    {ok, Fn, [{Property, []}]};
generate_test(_, _) ->
    ignore.
