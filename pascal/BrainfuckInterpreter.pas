unit BrainfuckInterpreter;
interface
    uses BrainfuckUtility, BrainfuckTyping;

    type
        TJumpTableEntry = record
            EntryPoint  : TProgramCounter;
            ExitPoint   : TProgramCounter;
        end;

        TCin = function : byte;
        TCout = procedure (value: byte);

        TBrainfuckInterpreter = record
            Commands    : AnsiString;
            JumpTable   : array of TJumpTableEntry;
            Cin         : TCin;
            Cout        : TCout;
        end;

    procedure InitializeBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter; Commands: AnsiString; Cin: TCin; Cout: TCout);
    procedure RunBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter);

implementation
    procedure InitializeJumpTable(var Interpreter : TBrainfuckInterpreter);
    var
        Stack                   : TProgramCounterStack;
        ProgramCounter          : TProgramCounter;
        MatchingProgramCounter  : TProgramCounter;
    begin
        SetLength(Interpreter.JumpTable, 0);
        Clear(Stack);

        for ProgramCounter := 1 to Length(Interpreter.Commands) do
        begin
            if Interpreter.Commands[ProgramCounter] = '[' then Push(Stack, ProgramCounter)
            else if Interpreter.Commands[ProgramCounter] = ']' then begin
                if IsEmpty(Stack) then Panic('Missing matching [');
                MatchingProgramCounter := Pop(Stack);
                SetLength(Interpreter.JumpTable, Length(Interpreter.JumpTable) + 2);
                Interpreter.JumpTable[Length(Interpreter.JumpTable) - 1].EntryPoint := MatchingProgramCounter;
                Interpreter.JumpTable[Length(Interpreter.JumpTable) - 1].ExitPoint := ProgramCounter;
            end; 
        end;

        if not IsEmpty(Stack) then Panic('Missing matching ]');

    end;

    procedure InitializeBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter; Commands: AnsiString; Cin: TCin; Cout: TCout);
    begin
        Interpreter.Commands := Commands;
        Interpreter.Cin := Cin;
        Interpreter.Cout := Cout;
        InitializeJumpTable(Interpreter);
    end;

    function GetMatchingJump(var JumpTable: array of TJumpTableEntry; ProgramCounter: TProgramCounter) : TProgramCounter;
    var Idx : integer;
    begin
        GetMatchingJump := -1;
        Idx := 0;
        while (Idx < Length(JumpTable)) and (GetMatchingJump = -1) do
            if JumpTable[Idx].EntryPoint = ProgramCounter then GetMatchingJump := JumpTable[Idx].ExitPoint
            else if JumpTable[Idx].ExitPoint = ProgramCounter then GetMatchingJump := JumpTable[Idx].EntryPoint;
        if GetMatchingJump = -1 then Panic('Cannot find matching jump target');
    end;

    procedure RunBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter);
    var 
        Idx             : integer;
        ProgramCounter  : TProgramCounter;
        Command         : char;
        Ptr             : integer;
        Tape            : array[-30000..30000] of byte;
    begin

        for Idx := -30000 to 30000 do
            Tape[Idx] := 0;
        Ptr := 0;
        
        ProgramCounter := 1;
        while ProgramCounter <= Length(Interpreter.Commands) do
        begin
            Command := Interpreter.Commands[ProgramCounter];

            if Command = '>' then Inc(Ptr)
            else if Command = '<' then Dec(Ptr)
            else if Command = '+' then Inc(Tape[Ptr])
            else if Command = '-' then Dec(Tape[Ptr])
            else if Command = '.' then Interpreter.Cout(Tape[Ptr])
            else if Command = '.' then Tape[Ptr] := Interpreter.Cin()
            else if Command = '[' then
                begin
                    if tape[Ptr] = 0 then ProgramCounter := GetMatchingJump(Interpreter.JumpTable, ProgramCounter) - 1;
                end
            else if Command = ']' then
                begin
                    if tape[Ptr] <> 0 then ProgramCounter := GetMatchingJump(Interpreter.JumpTable, ProgramCounter) - 1;
                end;

            Inc(ProgramCounter);
        end;
    end;

end.