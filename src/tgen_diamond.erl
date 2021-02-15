-module('tgen_diamond').

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
    input := #{letter := Letter}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        tgs:raw("Expected=" ++ format_lines(Exp)),
        erl_syntax:tuple([
            tgs:string(Desc),
            tgs:call_macro("_assertEqual", [
                tgs:raw("Expected"),
                tgs:call_fun("diamond:" ++ Property, [
                    tgs:value(binary_to_list(Letter))
                ])
            ])
        ])
    ]),

    {ok, Fn, [{Property, ["Letter"]}]}.

format_lines(Lines) ->
    Fmt0 = ["        \"~s\"" || _ <- Lines],
    Fmt1 = lists:join(",~n", Fmt0),
    Fmt2 = io_lib:format("[~n~s~n    ]", [Fmt1]),
    Fmt3 = lists:flatten(Fmt2),
    lists:flatten(io_lib:format(Fmt3, Lines)).
