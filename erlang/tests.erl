-module(tests).
-export([main/0]).
-import(interpreter, [execute/3]).
-import(common, [readlines/1]).
-import(ets,[new/2, insert/2, first/1, delete_all_objects/1, delete/1]).
-import(lists, [nth/2, any/2]).

main() ->
    io:format("Running tests for brainfuck implementation in erlang:~n"),
    ets:new(input, [bag, named_table]),
    ets:new(output, [bag, named_table]),
    Results = [ hello_world_short(), hello_world(), echo(), rot13(), wc() ],
    IsFailure = fun(X) -> X == fail end,
    AnyFailure = lists:any(IsFailure, Results),
    if
        AnyFailure ->
            io:format("~n=> TEST FAILED <= ~n", []),
            ErrorCode = 1;
        true ->
            io:format("~n=> ALL TESTS PASSED <= ~n", []),
            ErrorCode = 0
    end,
    halt(ErrorCode).

hello_world() ->
    io:format("* Hello world... "),
    ets:delete_all_objects(input),
    ets:delete_all_objects(output),
    ets:insert(input, { 1 }),
    ets:insert(output, { [] }),
    Program = common:readlines("../samples/hello-world.bf"),
    Input = "",
    interpreter:execute(
        Program,
        fun() -> ets_stdin(Input) end,
        fun(Ch) -> ets_stdout(Ch) end),
    ResultOutput = ets:first(output),
    Expected = "Hello World!" ++ [10],
    if
        ResultOutput == Expected -> Result = ok;
        true -> Result = fail
    end,
    io:format("~w~n", [Result]),
    Result.

hello_world_short() ->
    io:format("* Hello world short... "),
    ets:delete_all_objects(input),
    ets:delete_all_objects(output),
    ets:insert(input, { 1 }),
    ets:insert(output, { [] }),
    Program = common:readlines("../samples/hello-world-short.bf"),
    Input = "",
    interpreter:execute(
        Program,
        fun() -> ets_stdin(Input) end,
        fun(Ch) -> ets_stdout(Ch) end),
    ResultOutput = ets:first(output),
    Expected = "Hello World!" ++ [10],
    if
        ResultOutput == Expected -> Result = ok;
        true -> Result = fail
    end,
    io:format("~w~n", [Result]),
    Result.


echo() ->
    io:format("* echo... "),
    ets:delete_all_objects(input),
    ets:delete_all_objects(output),
    ets:insert(input, { 1 }),
    ets:insert(output, { "" }),
    Program = common:readlines("../samples/echo-single-char.bf"),
    Input = "a",
    interpreter:execute(
        Program,
        fun() -> ets_stdin(Input) end,
        fun(Ch) -> ets_stdout(Ch) end),
    ResultOutput = ets:first(output),
    Expected = "a",
    if
        ResultOutput == Expected -> Result = ok;
        true -> Result = fail
    end,
    io:format("~w~n", [Result]),
    Result.

rot13() ->
    io:format("* ROT13... "),
    ets:delete_all_objects(input),
    ets:delete_all_objects(output),
    ets:insert(input, { 1 }),
    ets:insert(output, { "" }),
    Program = common:readlines("../samples/rot13.bf"),
    Input = "stefano" ++ [0],
    interpreter:execute(
        Program,
        fun() -> ets_stdin(Input) end,
        fun(Ch) -> ets_stdout(Ch) end),
    ResultOutput = ets:first(output),
    Expected = "fgrsnab",
    if
        ResultOutput == Expected -> Result = ok;
        true -> Result = fail
    end,
    io:format("~w~n", [Result]),
    Result.

wc() ->
    io:format("* wc... "),
    ets:delete_all_objects(input),
    ets:delete_all_objects(output),
    ets:insert(input, { 1 }),
    ets:insert(output, { "" }),
    Program = common:readlines("../samples/wc.bf"),
    Input = "Hello world" ++ [10, 13] ++ "this is me" ++ [0],
    interpreter:execute(
        Program,
        fun() -> ets_stdin(Input) end,
        fun(Ch) -> ets_stdout(Ch) end),
    ResultOutput = ets:first(output),
    Expected = [9] ++ "1" ++ [9] ++ "5" ++ [9] ++ "23" ++ [10],
    if
        ResultOutput == Expected -> Result = ok;
        true -> Result = fail
    end,
    io:format("~w~n", [Result]),
    Result.

ets_stdin(Input) ->
    CurrentIndex = ets:first(input),
    Result = lists:nth(CurrentIndex, Input),
    ets:delete_all_objects(input),
    ets:insert(input, { CurrentIndex + 1 }),
    Result.

ets_stdout(Ch) ->
    Current = ets:first(output),
    ets:delete_all_objects(output),
    ets:insert(output, { Current ++ [Ch] }),
    ok.