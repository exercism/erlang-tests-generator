-module(tgen).

-export([
    check/1,
    generate/1,
    to_test_name/1, to_test_name/2,
    to_property_name/1
]).

-export_type([
    tgen/0,
    exercise_json/0
]).

-include("tgen.hrl").

-type tgen() :: #tgen{}.

% -type canonical_data() :: #{
%     exercise := binary(),
%     version  := binary(),
%     cases    := exercise_json()
% }.

-type exercise_json() :: #{
    description := binary() | string(),
    expected := jsx:json_term(),
    property := binary(),
    input := map(),
    binary() => jsx:json_term()
}.

-type fun_name() :: string() | binary().
-type arg_name() :: string() | binary().
-type args() :: [arg_name()].

-callback prepare_test_module() -> {ok, [erl_syntax:syntax_tree()]}.
-callback prepare_tests([exercise_json()]) -> [exercise_json()].
-callback generate_test(non_neg_integer(), exercise_json()) ->
    {ok, erl_syntax:syntax_tree() | [erl_syntax:syntax_tree()], [{fun_name(), args()}]}
    | ignore.
-callback revision() -> pos_integer().

-optional_callbacks([prepare_test_module/0]).
-optional_callbacks([prepare_tests/1]).

-spec check(string()) -> {true, atom()} | false.
check(Name) ->
    Module = list_to_atom("tgen_" ++ Name),
    try Module:available() of
        true -> {true, Module};
        false -> false
    catch
        _:_ -> false
    end.

-spec generate(tgen()) -> ok.
generate(Generator = #tgen{}) ->
    io:format("Generating ~s", [Generator#tgen.name]),
    case file:read_file(Generator#tgen.path) of
        {ok, Content} ->
            Files = process_json(Generator, Content),
            io:format(", finished~n"),
            Files;
        {error, Reason} ->
            io:format(", failed (~p)~n", [Reason]),
            {error, Reason, Generator#tgen.path}
    end.

process_json(G = #tgen{name = GName}, Content) when is_list(GName) ->
    process_json(G#tgen{name = list_to_binary(GName)}, Content);
process_json(#tgen{name = GName, module = Module, sha = SHA, dest = Dest}, Content) ->
    TOMLPath = filename:join([Dest, ".meta", "tests.toml"]),
    {ok, #{} = CTests} = tomerl:read_file(TOMLPath),
    case jsx:decode(Content, [return_maps, {labels, attempt_atom}]) of
        _JSON = #{exercise := GName, cases := Cases0} ->
            Cases1 = flatten_cases(Cases0),
            Cases2 = lists:filter(
                fun(#{uuid := UUID}) ->
                    case tomerl:get(CTests, [UUID, include]) of
                        {ok, V} -> V;
                        {error, not_found} -> throw({uuid_not_found, UUID})
                    end
                end,
                Cases1
            ),
            Cases3 = prepare_tests(Module, Cases2),
            % io:format("Parsed JSON: ~p~n", [JSON]),
            {_, TestImpls0, Props} = lists:foldl(
                fun(Spec, {N, Tests, OldProperties}) ->
                    case Module:generate_test(N, Spec) of
                        {ok, Test, Properties} ->
                            {N + 1, [Test | Tests], combine(OldProperties, Properties)};
                        ignore ->
                            {N, Tests, OldProperties}
                    end
                end,
                {1, [], []},
                Cases3
            ),
            TestImpls1 = lists:reverse(TestImpls0),
            {TestModuleName, TestModuleContent} = generate_test_module(
                SHA,
                Module,
                binary_to_list(GName),
                TestImpls1
            ),
            {StubModuleName, StubModuleContent} = generate_stub_module(
                Module,
                binary_to_list(GName),
                Props
            ),

            [
                #{
                    exercise => GName,
                    name => TestModuleName,
                    folder => "test",
                    content => io_lib:format("~s", [TestModuleContent])
                },
                #{
                    exercise => GName,
                    name => StubModuleName,
                    folder => "src",
                    content => io_lib:format("~s", [StubModuleContent])
                }
            ];
        JSON = #{exercise := GName} ->
            io:format("Unknown JSON for ~s: ~p~n", [GName, JSON]);
        #{exercise := Name} ->
            io:format("Name in JSON (~p) and name for generator (~p) do not line up", [Name, GName])
    end.

-spec to_test_name(string() | binary()) -> string().
to_test_name(Name) when is_binary(Name) ->
    to_test_name(binary_to_list(Name));
to_test_name(Name) when is_list(Name) ->
    slugify(Name) ++ "_test".

-spec to_test_name(non_neg_integer(), string() | binary()) -> string().
to_test_name(N, Name) when is_binary(Name) ->
    to_test_name(N, binary_to_list(Name));
to_test_name(N, Name) when is_list(Name) ->
    lists:flatten([integer_to_list(N), $_, slugify(Name), "_test"]).

-spec to_property_name(string() | binary()) -> string().
to_property_name(Name) when is_binary(Name) ->
    to_property_name(binary_to_list(Name));
to_property_name(Name) when is_list(Name) ->
    slugify(Name).

% slugify(Name) when is_binary(Name) -> list_to_binary(slugify(binary_to_list(Name)));
slugify(Name) when is_list(Name) -> slugify(Name, false, []).

slugify([], _, Acc) ->
    lists:reverse(Acc);
slugify([$_ | Name], _, Acc) ->
    slugify(Name, false, [$_ | Acc]);
slugify([$- | Name], _, Acc) ->
    slugify(Name, false, [$_ | Acc]);
slugify([$\s | Name], _, Acc) ->
    slugify(Name, false, [$_ | Acc]);
slugify([C | Name], _, Acc) when C >= $a andalso C =< $z ->
    slugify(Name, true, [C | Acc]);
slugify([C | Name], _, Acc) when C >= $0 andalso C =< $9 ->
    slugify(Name, true, [C | Acc]);
slugify([C | Name], false, Acc) when C >= $A andalso C =< $Z ->
    slugify(Name, false, [C - $A + $a | Acc]);
slugify([C | Name], true, Acc) when C >= $A andalso C =< $Z ->
    slugify(Name, false, [C - $A + $a, $_ | Acc]);
slugify([_ | Name], AllowSnail, Acc) ->
    slugify(Name, AllowSnail, Acc).

generate_stub_module(_Module, ModuleName, Props) ->
    SluggedModName = slugify(ModuleName),

    Funs = lists:map(
        fun
            ({Name, []}) ->
                tgs:simple_fun(Name, [tgs:atom(undefined)]);
            ({Name, Args}) when is_list(Args) ->
                UnderscoredArgs = lists:map(fun(Arg) -> [$_ | Arg] end, Args),
                tgs:simple_fun(Name, UnderscoredArgs, [tgs:atom(undefined)])
        end,
        Props
    ),

    Abstract =
        [
            tgs:module(SluggedModName),
            nl,
            tgs:export(Props),
            nl,
            nl
        ] ++ inter(nl, Funs),

    {SluggedModName,
        lists:flatten(
            lists:map(
                fun
                    (nl) -> io_lib:format("~n", []);
                    (Tree) -> io_lib:format("~s~n", [erl_prettypr:format(Tree)])
                end,
                Abstract
            )
        )}.

prepare_test_module(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    case erlang:function_exported(Module, prepare_test_module, 0) of
        true ->
            {ok, Tree} = Module:prepare_test_module(),
            inter(nl, lists:flatten(Tree));
        false ->
            []
    end.

prepare_tests(Module, Tests) ->
    {module, Module} = code:ensure_loaded(Module),
    case erlang:function_exported(Module, prepare_tests, 1) of
        true -> Module:prepare_tests(Tests);
        false -> Tests
    end.

generate_test_module(SHA, Module, ModuleName, Tests) ->
    SluggedModName = slugify(ModuleName),
    Revision = integer_to_list(Module:revision()),
    {ok, GeneratorVsn} = application:get_key(testgen, vsn),

    Abstract = [
        erl_syntax:comment(
            [
                "% Generated with 'testgen v" ++ GeneratorVsn ++ "'",
                "% Revision " ++ Revision ++ " of the exercises generator was used",
                "% https://github.com/exercism/problem-specifications/raw/" ++
                    SHA ++ "/exercises/" ++ ModuleName ++ "/canonical-data.json",
                "% This file is automatically generated from the exercises canonical data."
            ]
        ),
        tgs:module(SluggedModName ++ "_tests"),
        nl,
        tgs:include_lib("erl_exercism/include/exercism.hrl"),
        tgs:include_lib("eunit/include/eunit.hrl")
    ],

    Preamble = prepare_test_module(Module),

    Body = inter(nl, lists:flatten(Tests)),

    Content = Abstract ++ [nl, nl] ++ Preamble ++ [nl, nl] ++ Body,

    {SluggedModName ++ "_tests",
        lists:flatten(
            lists:map(
                fun
                    (nl) -> io_lib:format("~n", []);
                    (Tree) -> io_lib:format("~s~n", [erl_prettypr:format(Tree)])
                end,
                Content
            )
        )}.

inter(_, []) -> [];
inter(_, [X]) -> [X];
inter(Delim, [X | Xs]) -> [X, Delim | inter(Delim, Xs)].

combine(List, []) ->
    List;
combine(List, [{Name, Arity} | Xs]) when is_list(Name) ->
    combine(List, [{list_to_binary(Name), Arity} | Xs]);
combine(List, [{Name, _} = X | Xs]) when is_binary(Name) ->
    List1 = insert(List, X),
    combine(List1, Xs).

insert([], X) -> [X];
insert([X | _] = Xs, X) -> Xs;
insert([X | XS], Y) when X < Y -> [X | insert(XS, Y)];
insert([X | XS], Y) when X > Y -> [Y, X | XS].

flatten_cases(Cases) ->
    flatten_cases(Cases, []).
flatten_cases([], Acc) ->
    lists:flatten(lists:reverse(Acc));
flatten_cases([#{cases := Nested} | Cases], Acc) ->
    flatten_cases(Cases, [flatten_cases(Nested) | Acc]);
flatten_cases([Case | Cases], Acc) ->
    flatten_cases(Cases, [Case | Acc]).
