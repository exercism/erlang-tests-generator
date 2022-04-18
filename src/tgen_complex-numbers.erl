-module('tgen_complex-numbers').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{description := Desc, expected := Exp, property := Prop, input := #{z := Z}}) when
    is_number(Exp)
->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Cplx = lists:map(fun to_num/1, Z),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assert", [
                erl_syntax:infix_expr(
                    tgs:value(Exp),
                    erl_syntax:operator("=="),
                    tgs:call_fun("complex_numbers:" ++ Property, [
                        tgs:call_fun("complex_numbers:new", lists:map(fun tgs:value/1, Cplx))
                    ])
                )
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Z"]}]};
generate_test(N, #{description := Desc, expected := Exp, property := Prop, input := #{z := Z}}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Cplx = lists:map(fun to_num/1, Z),
    Expected = lists:map(fun to_num/1, Exp),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assert", [
                tgs:call_fun("complex_numbers:equal", [
                    tgs:call_fun("complex_numbers:new", lists:map(fun tgs:value/1, Expected)),
                    tgs:call_fun("complex_numbers:" ++ Property, [
                        tgs:call_fun("complex_numbers:new", lists:map(fun tgs:value/1, Cplx))
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Z"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := <<"div">>,
    input := #{z1 := Z1, z2 := Z2}
}) ->
    generate_test(N, #{
        description => Desc,
        expected => Exp,
        property => <<"divide">>,
        input => #{z1 => Z1, z2 => Z2}
    });
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{z1 := Z1, z2 := Z2}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Cplx1 = to_cplx(Z1),
    Cplx2 = to_cplx(Z2),
    Expected = to_cplx(Exp),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assert", [
                tgs:call_fun("complex_numbers:equal", [
                    tgs:call_fun("complex_numbers:new", lists:map(fun tgs:value/1, Expected)),
                    tgs:call_fun("complex_numbers:" ++ Property, [
                        tgs:call_fun("complex_numbers:new", lists:map(fun tgs:value/1, Cplx1)),
                        tgs:call_fun("complex_numbers:new", lists:map(fun tgs:value/1, Cplx2))
                    ])
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Z1", "Z2"]}, {"equal", ["Z1", "Z2"]}, {"new", ["R", "I"]}]}.

to_num(N) when is_number(N) -> N;
to_num(<<"ln(2)">>) -> math:log(2);
to_num(<<"pi">>) -> math:pi();
to_num(<<"e">>) -> math:exp(1).

to_cplx(L) when is_list(L) -> lists:map(fun to_num/1, L);
to_cplx(N) when is_integer(N) -> [to_num(N), 0].