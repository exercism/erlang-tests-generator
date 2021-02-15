-module('tgen_connect').

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
    input := #{board := Board}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        tgs:raw("Input=" ++ format_lines(Board)),
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:value(exp2atom(Exp)),
                tgs:call_fun("connect:" ++ Property, [
                    tgs:raw("Input")
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Board"]}]}.

%% align input/expectation lines to make a nice board
format_lines(Lines) ->
    Fmt0 = ["        \"~s\"" || _ <- Lines],
    Fmt1 = lists:join(",~n", Fmt0),
    Fmt2 = io_lib:format("[~n~s~n    ]", [Fmt1]),
    Fmt3 = lists:flatten(Fmt2),
    lists:flatten(io_lib:format(Fmt3, Lines)).

exp2atom(<<"X">>) -> x;
exp2atom(<<"O">>) -> o;
exp2atom(<<>>) -> undefined.
