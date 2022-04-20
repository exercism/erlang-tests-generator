-module('tgen_hello-world').

-behaviour(tgen).

-export([
    revision/0,
    prepare_test_module/0,
    generate_test/2,
    fun_body/2
]).

revision() -> 1.

prepare_test_module() ->
    AssertStringEqual = tgs:raw(
        "-define(assertStringEqual(Expect, Expr),\n"
        "        begin ((fun () ->\n"
        "            __X = (Expect),\n"
        "            __Y = (Expr),\n"
        "            case string:equal(__X, __Y) of\n"
        "                true -> ok;\n"
        "                false -> erlang:error({assertStringEqual,\n"
        "                    [{module, ?MODULE},\n"
        "                     {line, ?LINE},\n"
        "                     {expression, (??Expr)},\n"
        "                     {expected, unicode:characters_to_list(__X)},\n"
        "                     {value, unicode:characters_to_list(__Y)}]})\n"
        "            end\n"
        "        end)())\n"
        "    end)."
    ),

    UnderscoreAssertStringEqual = tgs:raw(
        "-define(_assertStringEqual(Expect, Expr), ?_test(?assertStringEqual(Expect, Expr)))."
    ),

    {ok, [AssertStringEqual, UnderscoreAssertStringEqual]}.

generate_test(N, #{description := Desc, expected := Exp, property := Prop}) ->
    TestName = tgen:to_test_name(N, Desc),
    Expected = binary_to_list(Exp),
    Property = tgen:to_property_name(Prop),

    Fn = tgs:simple_fun(TestName ++ "_", [
        erl_syntax:tuple([
            tgs:value(binary_to_list(Desc)),
            tgs:call_macro("_assertStringEqual", [
                tgs:value(Expected),
                tgs:call_fun("hello_world:" ++ Property, [])
            ])
        ])
    ]),

    {ok, Fn, [{Prop, []}]}.

fun_body(<<"hello">>, 0) -> tgs:raw("\"Goodbye, Mars!\"").
