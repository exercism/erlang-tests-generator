testgen
=====

A generator for the exercises testsuite

Build
-----

    $ make testgen

Run
---

    $ make

Implement a Testgenerator
-------------------------

1. Create a module that is named as the exercise, but prefixed with `tgen_`.
2. Make it implement the `tgen` behaviour.

### Callbacks

#### `tgen:revision/0`

Spec: `() -> pos_integer()`

When you initially create the module make this return `1`, after that every PullRequest changing the module shall also increment the number by `1`.

It is the monotonically increasing version of this module.

#### `tgen:generate_test/2`

Spec: `(non_neg_integer(), exercise_json()) -> {ok, erl_syntax:syntax_tree() | [erl_syntax:syntax_tree()], [{string() | binary(), non_neg_integer()}]} } | ignore`

The first argument, usually called `N` has to be considered opaque, it has to be passed to `tgen:to_test_name/2` as first argument to generate the testname.

The second argument is the current JSON blob. Please refer to the actual JSON specification of the exercise.

#### `tgen:prepare_test_module/0` (Optional)

Spec: `prepare_test_module() -> {ok, [erl_syntax:syntax_tree()]}`

Can inject arbitrary code into the test module, eg. helper functions, macros, etc.

#### `tgen:prepare_tests/1` (Optional)

Spec: `prepare_tests([exercise_json()]) -> [exercise_json()]`

This callback is called with a list of the JSON as it got parsed, it can be used to enrich the JSON with additional data or to filter some tests out, or even to change order of tests.
