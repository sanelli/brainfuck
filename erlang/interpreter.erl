-module(interpreter).
-export([execute/1]).

-import(lists, [nth/2, nthtail/2, droplast/1]).

execute(Program) ->
    JumpTable = create_jump_table(Program, 1, [], []),
    execute(1, create_memory(30000), JumpTable, Program, 1).

execute(Position, Memory, JumpTable, Program, ProgramCounter) ->
    CurrentCommand = lists:nth(ProgramCounter, Program),
    io:format("EXECUTE code: '~c', pc: ~w, ptr: ~w //  ~n", [CurrentCommand, ProgramCounter, Position]),
    case CurrentCommand of
        $> -> 
            NewProgramCounter = ProgramCounter + 1,
            NewPosition = Position + 1,
            NewMemory = Memory;
        $< -> 
            NewProgramCounter = ProgramCounter + 1,
            NewPosition = Position - 1,
            NewMemory = Memory;
        $+ -> 
            NewProgramCounter = ProgramCounter + 1,
            NewPosition = Position,
            NewValue = lists:nth(Position, Memory) + 1,
            NewMemory = replace_element(Memory, Position, NewValue),
            io:format("SET MEMORY[~w] = ~w~n", [Position, NewValue]);
        $- -> 
            NewProgramCounter = ProgramCounter + 1,
            NewPosition = Position,
            NewValue = lists:nth(Position, Memory) - 1,
            NewMemory = replace_element(Memory, Position, NewValue),
            io:format("SET MEMORY[~w] = ~w~n", [Position, NewValue]);
        $[ ->
            NewMemory = Memory,
            NewPosition = Position,
            CurrentValue = lists:nth(Position, Memory),
            io:format("TEST JUMP cv: '~w'~n", [CurrentValue]),
            if 
                CurrentValue == 0 -> NewProgramCounter = locate_jump_pair(JumpTable, ProgramCounter);
                CurrentValue /= 0 -> NewProgramCounter = ProgramCounter + 1
            end;
        $] ->
            NewMemory = Memory,
            NewPosition = Position,
            CurrentValue = lists:nth(Position, Memory),
            io:format("TEST JUMP cv: '~w'~n", [CurrentValue]),
            if 
                CurrentValue /= 0 -> NewProgramCounter = locate_jump_pair(JumpTable, ProgramCounter);
                CurrentValue == 0 -> NewProgramCounter = ProgramCounter + 1
            end;
        $. -> 
            NewProgramCounter = ProgramCounter + 1,
            NewMemory = Memory,
            NewPosition = Position,
            CurrentValue = lists:nth(Position, Memory),
            io:format("PRINT '~c'~n", [CurrentValue]),
            io:format("~c", [CurrentValue]);
        _ ->
            NewProgramCounter = ProgramCounter + 1,
            NewPosition = Position,
            NewMemory = Memory
    end,
    if
        NewProgramCounter =< length(Program) -> execute(NewPosition, NewMemory, JumpTable, Program, NewProgramCounter);
        NewProgramCounter > length(Program) -> ok
    end.

create_memory(Counter) ->
    if Counter > 0 -> [0] ++ create_memory(Counter - 1);
       Counter =< 0 -> []
    end.

elements_up_to([Head | Tail], Counter, Right) when Counter < Right
    -> [Head] ++ elements_up_to(Tail, Counter + 1, Right);
elements_up_to([Head | _], Counter, Right) when Counter == Right
    -> [Head];
elements_up_to(_, Counter, Right) when Counter > Right
    -> [].

replace_element(List, Index, Element)
    -> elements_up_to(List, 1, Index - 1) ++ [ Element ] ++ lists:nthtail(Index + 1, List).

create_jump_table([Head | RestOfProgram], ProgramPosition, Stack, JumpTable)
    -> case Head of
        $[ -> 
            NewStack = Stack ++ [ ProgramPosition ],
            NewJumpTable = JumpTable;
        $] -> 
            Pair = lists:nth(length(Stack), Stack),
            NewStack = lists:droplast(Stack),
            io:format("JT (~w m ~w) ~n", [Pair, ProgramPosition]),
            io:format("JT (~w m ~w) ~n", [ProgramPosition, Pair]),
            TempJumpTable = JumpTable ++ [{Pair, ProgramPosition}],
            NewJumpTable = TempJumpTable ++ [{ProgramPosition, Pair}];
        _ -> 
            NewStack = Stack,
            NewJumpTable = JumpTable
       end,
    create_jump_table(RestOfProgram, ProgramPosition + 1, NewStack, NewJumpTable);
create_jump_table([], _, _, JumpTable)
    -> JumpTable.

locate_jump_pair([{Left, Right} | RestOfJumpTable], Position)
    -> if 
            Left == Position -> 
                io:format("JUMP (~w -> ~w) ~n", [Position, Right]),
                Right;
            Right == Position ->
                io:format("JUMP (~w -> ~w) ~n", [Position, Left]),
                Left;
            (Left /= Position) and (Right /= Position) -> locate_jump_pair(RestOfJumpTable, Position)
        end;
locate_jump_pair([], _)
    -> error("Something went wrong when detecting the corresponding jump").