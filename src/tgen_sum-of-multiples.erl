-module('tgen_sum-of-multiples').

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
    input := #{factors := Factors, limit := Limit}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(Exp),
                tgs:call_fun("sum_of_multiples:" ++ Property, [
                    tgs:raw(format_factors(Factors)),
                    tgs:value(Limit)
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Factors", "Limit"]}]}.

format_factors(Factors) ->
    Factors1 = [io_lib:format("~B", [F]) || F <- Factors],
    lists:flatten([$[, lists:join(", ", Factors1), $]]).
