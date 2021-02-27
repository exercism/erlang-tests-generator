-module('tgen_custom-set').

-behaviour(tgen).

-export([
    revision/0,
    prepare_tests/1,
    generate_test/2
]).

revision() -> 1.

prepare_tests(Cases) ->
    %% pull up the equality tests because they are needed in other tests
    {Equality, Other} = lists:partition(
        fun
            (#{property := <<"equal">>}) -> true;
            (_) -> false
        end,
        Cases
    ),
    Equality ++ Other.

generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{set1 := Set1, set2 := Set2}
}) when is_list(Exp) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assert", [
                tgs:call_fun("custom_set:equal", [
                    tgs:call_fun("custom_set:from_list", [
                        tgs:value(Exp)
                    ]),
                    tgs:call_fun("custom_set:" ++ Property, [
                        tgs:call_fun("custom_set:from_list", [tgs:value(Set1)]),
                        tgs:call_fun("custom_set:from_list", [tgs:value(Set2)])
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Set1", "Set2"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{set1 := Set1, set2 := Set2}
}) when Exp =:= true; Exp =:= false ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Assert =
        "_" ++
            case Exp of
                true -> "assert";
                false -> "assertNot"
            end,

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro(Assert, [
                tgs:call_fun("custom_set:" ++ Property, [
                    tgs:call_fun("custom_set:from_list", [tgs:value(Set1)]),
                    tgs:call_fun("custom_set:from_list", [tgs:value(Set2)])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Set1", "Set2"]}, {"from_list", ["List"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{set := Set, element := Elem}
}) when is_list(Exp) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assert", [
                tgs:call_fun("custom_set:equal", [
                    tgs:call_fun("custom_set:from_list", [
                        tgs:value(Exp)
                    ]),
                    tgs:call_fun("custom_set:" ++ Property, [
                        tgs:value(Elem),
                        tgs:call_fun("custom_set:from_list", [
                            erl_syntax:abstract(Set)
                        ])
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Elem", "Set"]}, {"from_list", ["List"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{set := Set, element := Elem}
}) when Exp =:= true; Exp =:= false ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Assert =
        "_" ++
            case Exp of
                true -> "assert";
                false -> "assertNot"
            end,

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro(Assert, [
                tgs:call_fun("custom_set:" ++ Property, [
                    tgs:value(Elem),
                    tgs:call_fun("custom_set:from_list", [
                        erl_syntax:abstract(Set)
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Elem", "Set"]}, {"from_list", ["List"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := <<"empty">>,
    input := #{set := Set}
}) when Exp =:= true; Exp =:= false ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(<<"empty">>),

    Assert =
        "_" ++
            case Exp of
                true -> "assert";
                false -> "assertNot"
            end,

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro(Assert, [
                tgs:call_fun("custom_set:" ++ Property, [
                    tgs:call_fun("custom_set:from_list", [
                        tgs:value(Set)
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Set"]}, {"from_list", ["List"]}]}.
