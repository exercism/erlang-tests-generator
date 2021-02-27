-module('tgen_space-age').

-behaviour(tgen).

-export([
    revision/0,
    prepare_test_module/0,
    generate_test/2
]).

revision() -> 1.

prepare_test_module() ->
    {
        ok,
        [
            tgs:raw(
                io_lib:format(
                    "-define(equalFloat(Desc, A, B), {Desc, ?_assertEqual(B, round(A,2))}).",
                    []
                )
            ),
            tgs:raw(
                io_lib:format(
                    "round(Number, Precision) ->~n"
                    "    P = math:pow(10, Precision),~n"
                    "    round(Number * P) / P.",
                    []
                )
            )
        ]
    }.

generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{planet := Planet, seconds := Seconds}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        tgs:call_macro("equalFloat", [
            tgs:string(Desc),
            tgs:call_fun("space_age:" ++ Property, [
                tgs:value(planet_b2a(Planet)),
                tgs:value(Seconds)
            ]),
            tgs:value(Exp)
        ])
    ]),

    {ok, Fn, [{Property, ["Planet", "Seconds"]}]}.

planet_b2a(<<"Mercury">>) -> mercury;
planet_b2a(<<"Venus">>) -> venus;
planet_b2a(<<"Earth">>) -> earth;
planet_b2a(<<"Mars">>) -> mars;
planet_b2a(<<"Jupiter">>) -> jupiter;
planet_b2a(<<"Saturn">>) -> saturn;
planet_b2a(<<"Uranus">>) -> uranus;
planet_b2a(<<"Neptune">>) -> neptune.
