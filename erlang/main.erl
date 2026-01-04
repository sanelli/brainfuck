-module(main).
-export([main/1]).
-import(interpreter, [execute/2]).
-import(common, [readlines/1]).
-import(ets,[new/2, insert/2, first/1, delete_all_objects/1]).
-import(lists, [nth/2]).

main(Args) ->
    Filename = lists:nth(1, Args),
    Input = lists:nth(2, Args),
    Program = common:readlines(Filename),
    ets:new(bf, [bag, named_table]),
    ets:insert(bf, {1}),
    interpreter:execute(Program, fun() -> command_line_stdin(Input) end),
    halt().

command_line_stdin(Input) ->
    CurrentIndex = ets:first(bf),
    Result = lists:nth(CurrentIndex, Input),
    ets:delete_all_objects(bf),
    ets:insert(bf, { CurrentIndex + 1 }),
    Result.