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
            Code        : AnsiString;
            JumpTable   : array of TJumpTableEntry;
            Cin         : TCin;
            Cout        : TCout;
        end;

    procedure InitializeBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter; Code: AnsiString; Cin: TCin; Cout: TCout; Debug: boolean);
    procedure RunBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter; Debug: boolean);

implementation
    procedure InitializeJumpTable(var Interpreter : TBrainfuckInterpreter; Debug: boolean);
    var
        Stack                   : TProgramCounterStack;
        ProgramCounter          : TProgramCounter;
        MatchingProgramCounter  : TProgramCounter;
    begin
        SetLength(Interpreter.JumpTable, 0);
        Clear(Stack);

        if Debug then WriteLn('[Debug] Computing jump table');
        for ProgramCounter := 1 to Length(Interpreter.Code) do
        begin
            if Interpreter.Code[ProgramCounter] = '[' then Push(Stack, ProgramCounter)
            else if Interpreter.Code[ProgramCounter] = ']' then begin
                if IsEmpty(Stack) then Panic('Missing matching [');
                MatchingProgramCounter := Pop(Stack);
                SetLength(Interpreter.JumpTable, Length(Interpreter.JumpTable) + 2);
                Interpreter.JumpTable[Length(Interpreter.JumpTable) - 1].EntryPoint := MatchingProgramCounter;
                Interpreter.JumpTable[Length(Interpreter.JumpTable) - 1].ExitPoint := ProgramCounter;

                if Debug then WriteLn('[Debug] Matching parenthesis at [',MatchingProgramCounter,',',ProgramCounter,']')
            end; 
        end;

        if not IsEmpty(Stack) then Panic('Missing matching ]');

    end;

    procedure InitializeBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter; Code: AnsiString; Cin: TCin; Cout: TCout; Debug: boolean);
    begin
        Interpreter.Code := Code;
        Interpreter.Cin := Cin;
        Interpreter.Cout := Cout;
        InitializeJumpTable(Interpreter, Debug);
    end;

    function GetMatchingJump(var JumpTable: array of TJumpTableEntry; ProgramCounter: TProgramCounter) : TProgramCounter;
    var Idx : integer;
    begin
        GetMatchingJump := -1;
        Idx := 0;
        while (Idx < Length(JumpTable)) and (GetMatchingJump = -1) do begin
            if JumpTable[Idx].EntryPoint = ProgramCounter then GetMatchingJump := JumpTable[Idx].ExitPoint
            else if JumpTable[Idx].ExitPoint = ProgramCounter then GetMatchingJump := JumpTable[Idx].EntryPoint;
            Inc(Idx);
        end;
        if GetMatchingJump = -1 then Panic('Cannot find matching jump target');
    end;

    procedure RunBrainfuckInterpreter(var Interpreter : TBrainfuckInterpreter; Debug: boolean);
    var 
        Idx             : integer;
        ProgramCounter  : TProgramCounter;
        Command         : char;
        Ptr             : integer;
        Tape            : array[-30000..30000] of byte;
    begin
        if Debug then WriteLn('[Debug] Reset tape');
        for Idx := -30000 to 30000 do
            Tape[Idx] := 0;
        Ptr := 0;
        
        if Debug then WriteLn('[Debug] Running program:');
        ProgramCounter := 1;
        while ProgramCounter <= Length(Interpreter.Code) do
        begin
            if Debug then Write('[debug]    [PC: ', ProgramCounter,'] ');
            Command := Interpreter.Code[ProgramCounter];
            if Debug then WriteLn(Command);

            if Command = '>' then Inc(Ptr)
            else if Command = '<' then Dec(Ptr)
            else if Command = '+' then Inc(Tape[Ptr])
            else if Command = '-' then Dec(Tape[Ptr])
            else if Command = '.' then Interpreter.Cout(Tape[Ptr])
            else if Command = ',' then Tape[Ptr] := Interpreter.Cin()
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