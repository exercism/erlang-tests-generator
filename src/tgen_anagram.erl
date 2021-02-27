-module('tgen_anagram').

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
    input := #{subject := Subject, candidates := Candidates}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Candidates1 = [binary_to_list(V) || V <- Candidates],
    Exp1 = lists:sort([binary_to_list(V) || V <- Exp]),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertMatch", [
                tgs:value(Exp1),
                tgs:call_fun("lists:sort", [
                    tgs:call_fun("anagram:" ++ Property, [
                        tgs:value(binary_to_list(Subject)),
                        tgs:value(Candidates1)
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Subject", "Candidates"]}]}.
