-module(interpreter).
-export([new/0, get_pointer/1, move/2]). % TODO: ONLY LEAVE HERE new, execute

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