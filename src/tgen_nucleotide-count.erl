-module('tgen_nucleotide-count').

-behaviour(tgen).

-export([
    revision/0,
    generate_test/2
]).

revision() -> 1.

generate_test(N, #{
    description := Desc,
    expected := #{error := _},
    property := <<"nucleotideCounts">>,
    input := #{strand := Strand}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property1 = tgen:to_property_name(<<"nucleotide_counts">>),
    Property2 = tgen:to_property_name(<<"count">>),

    Asserts0 = lists:map(
        fun(Nuc) ->
            erl_syntax:tuple([
                tgs:string(unicode:characters_to_list([Desc, ", ", Nuc])),
                tgs:call_macro("_assertError", [
                    tgs:raw("_"),
                    tgs:call_fun("nucleotide_count:" ++ Property2, [
                        tgs:var("Strand"),
                        tgs:value(Nuc)
                    ])
                ])
            ])
        end,
        ["A", "C", "G", "T"]
    ),
    Asserts1 = [
        erl_syntax:tuple([
            tgs:string(unicode:characters_to_list([Desc, ", all"])),
            tgs:call_macro("_assertError", [
                tgs:raw("_"),
                tgs:call_fun("nucleotide_count:" ++ Property1, [
                    tgs:var("Strand")
                ])
            ])
        ])
        | Asserts0
    ],

    Fn = tgs:simple_fun(
        TestName ++ "_",
        [
            tgs:assign(tgs:var("Strand"), tgs:value(binary_to_list(Strand)))
        ] ++
            [erl_syntax:list(Asserts1)]
    ),

    {ok, Fn, [{Property1, ["Strand"]}, {Property2, ["Strand", "Nucleotide"]}]};
generate_test(N, #{
    description := Desc,
    expected := Exp,
    property := <<"nucleotideCounts">>,
    input := #{strand := Strand}
}) ->
    TestName = tgen:to_test_name(N, Desc),
    Property1 = tgen:to_property_name(<<"nucleotide_counts">>),
    Property2 = tgen:to_property_name(<<"count">>),

    Exp1 = [{binary_to_list(K), V} || {K, V} <- maps:to_list(Exp)],

    Asserts0 = lists:map(
        fun({K, V}) ->
            erl_syntax:tuple([
                tgs:string(unicode:characters_to_list([Desc, ", ", K])),
                tgs:call_macro("_assertEqual", [
                    tgs:value(V),
                    tgs:call_fun("nucleotide_count:" ++ Property2, [
                        tgs:var("Strand"),
                        tgs:value(K)
                    ])
                ])
            ])
        end,
        Exp1
    ),
    Asserts1 = [
        erl_syntax:tuple([
            tgs:string(unicode:characters_to_list([Desc, ", all"])),
            tgs:call_macro("_assertEqual", [
                tgs:call_fun("lists:sort", [
                    tgs:value(Exp1)
                ]),
                tgs:call_fun("lists:sort", [
                    tgs:call_fun("nucleotide_count:" ++ Property1, [
                        tgs:var("Strand")
                    ])
                ])
            ])
        ])
        | Asserts0
    ],

    Fn = tgs:simple_fun(
        TestName ++ "_",
        [
            tgs:assign(tgs:var("Strand"), tgs:value(binary_to_list(Strand)))
        ] ++
            [erl_syntax:list(Asserts1)]
    ),

    {ok, Fn, [{Property1, ["Strand"]}, {Property2, ["Strand", "Nucleotide"]}]}.
