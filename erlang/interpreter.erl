-module(interpreter).
-export([new/0, get_pointer/1, move/2, get_current_value/1]). % TODO: ONLY LEAVE HERE new, execute

-record(brainfuck, {pointer, memory}).

new() ->
    #brainfuck{pointer = 0, memory = create_memory(30000)}.

get_pointer(Interpreter) ->
    Interpreter#brainfuck.pointer.

create_memory(Counter) ->
    if Counter > 0 -> [0] ++ create_memory(Counter - 1);
       Counter =< 0 -> []
    end.

move(Interpreter, Direction) ->
    case Direction of
        left -> Interpreter#brainfuck{ pointer = Interpreter#brainfuck.pointer - 1 };
        right -> Interpreter#brainfuck{ pointer = Interpreter#brainfuck.pointer + 1 };
        none -> Interpreter
    end.

get_current_value(Interpreter) ->
    {_, CurrentValue} = lists:nth(Interpreter#brainfuck.pointer, lists:enumerate(Interpreter#brainfuck.memory)),
    CurrentValue.

sublist(List, Begin, End) when Index < ListLength
    -> [ lists:nth(Begin, List) ] ++ sublist(List, Begin + 1, End).
sublist(List, Begin, End) when Begin = End
    -> [ lists:nth(Index, Begin) ]
sublist(_List, Begin, End) when Begin > End
    -> [].

replace_element(List, Index, Element)
    -> sublist(List, 0, Index - 1) ++ [ Element ] ++ sublist(List, Index + 1, length(List) - 1)

execute_program(Interpreter, [CurrentCommand | RestOfProgram]) ->
    % TODO: EXECUTE CASE HERE
    execute_program(Interpreter, RestOfProgram).

execute_program(Interpreter, []) ->
    ok.