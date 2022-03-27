-module('tgen_accumulate').
-behaviour(tgen).

-export([revision/0, generate_test/2]).

revision() -> 1.

generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := Prop,
    input := #{list := Input, accumulator := Acc},
    uuid := _UUID
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        tgs:assign(tgs:var("Expected"), to_list(Exp)),
        tgs:assign(tgs:var("Input"), to_list(Input)),
        tgs:assign(tgs:var("Acc"), accumulator(Acc)),
        erl_syntax:tuple([
            tgs:value(binary_to_list(Desc)),
            tgs:call_macro("_assertMatch", [
                tgs:var("Expected"),
                tgs:call_fun("accumulate:" ++ Property, [
                    tgs:var("Acc"),
                    tgs:var("Input")
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, []}]}.

accumulator(<<"(x) => x * x">>) ->
    to_expr_list("fun(X) -> X * X end.");
accumulator(<<"(x) => upcase(x)">>) ->
    to_expr_list("fun(X) -> string:uppercase(X) end.");
accumulator(<<"(x) => reverse(x)">>) ->
    to_expr_list("fun(X) -> string:reverse(X) end.");
accumulator(<<"(x) => accumulate([\"1\", \"2\", \"3\"], (y) => x + y)">>) ->
    to_expr_list(
        "fun(X) -> accumulate:accumulate(fun(Y) -> X ++ Y end, [\"1\", \"2\", \"3\"]) end."
    ).

to_expr_list(Code) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(Code),
    {ok, ExprList} = erl_parse:parse_exprs(Tokens),
    hd(ExprList).

value(X) when is_list(X) -> to_list(X);
value(X) when is_integer(X) -> tgs:value(X);
value(X) -> tgs:string(X).

to_list(L) -> erl_syntax:list([value(X) || X <- L]).
