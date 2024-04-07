program BrainfuckTest;
uses BrainfuckUtility, BrainfuckInterpreter;
const Debug: boolean = false;

var
    CoutBuffer          : array of byte;
    CinBuffer           : array of byte;
    NextInBufferPos     : integer;
    Interpreter         : TBrainfuckInterpreter;

procedure ResetBuffers(Input: AnsiString);
var Idx: integer;
begin
    SetLength(CoutBuffer, 0);

    NextInBufferPos := 0;
    SetLength(CinBuffer, Length(Input));

    for Idx := 1 to Length(input) do begin
        CinBuffer[Idx - 1] := ord(Input[Idx]);
    end;
end;

procedure DefaultCout(x: byte);
begin
    SetLength(CoutBuffer, Length(CoutBuffer) + 1);
    CoutBuffer[Length(CoutBuffer) - 1] := x;
end;

function DefaultCin(): byte;
begin
   if NextInBufferPos >= Length(CinBuffer) then Panic('Reading over buffer limits');
   DefaultCin := CinBuffer[NextInBufferPos];
   Inc(NextInBufferPos);
end;

function ValidateOutBuffer(Expected: AnsiString) : boolean;
var
    Actual  : AnsiString;
    Tmp     : AnsiString;
    Idx     : integer;
begin
    Actual := '';

    for Idx := 0 to Length(CoutBuffer) - 1 do
    begin
       Str(CoutBuffer[Idx], Tmp);
       Actual := Actual + Chr(CoutBuffer[Idx]);
    end;

    ValidateOutBuffer := Actual = Expected;
end;

procedure TestEcho();
var
    Success: boolean;
    Code : AnsiString;
begin
    Write('- Echo: ');

    ResetBuffers('x');
    Success:= true;
    Code := ',.';
    InitializeBrainfuckInterpreter(Interpreter, Code, @DefaultCin, @DefaultCout, Debug);
    RunBrainfuckInterpreter(Interpreter, Debug);

    Success := Success and (NextInBufferPos = 1);
    Success := Success and ValidateOutBuffer('x');

    if Success then WriteLn('OK')
    else Panic('KO');
end;

{  Sample from https://github.com/brain-lang/brainfuck/blob/master/brainfuck.md#hello-world-example }
procedure TestHelloWorld();
var
    Success: boolean;
    Code : AnsiString;
begin
    Write('- Hello world: ');

    ResetBuffers('');
    Success:= true;
    Code := '++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.';
    InitializeBrainfuckInterpreter(Interpreter, Code, @DefaultCin, @DefaultCout, Debug);
    RunBrainfuckInterpreter(Interpreter, Debug);

    Success := Success and (NextInBufferPos = 0);
    Success := Success and ValidateOutBuffer('Hello World!'#10);

    if Success then WriteLn('OK')
    else Panic('KO');
end;

{  Sample from https://brainfuck.org/rot13.b }
procedure TestRot13();
var
    Success: boolean;
    Code : AnsiString;
begin
    Write('- ROT13: ');

    ResetBuffers('stefano'#0);
    Success:= true;
    Code := ','#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>++++++++++++++<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>>+++++[<----->-]<<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>++++++++++++++<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>++++++++++++++<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>>+++++[<----->-]<<-'#10;
    Code := Code + '[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-'#10;
    Code := Code + '[>++++++++++++++<-'#10;
    Code := Code + '[>+<-]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]'#10;
    Code := Code + ']]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]>.[-]<,]';
    InitializeBrainfuckInterpreter(Interpreter, Code, @DefaultCin, @DefaultCout, Debug);
    RunBrainfuckInterpreter(Interpreter, Debug);

    Success := Success and (NextInBufferPos = 8);
    Success := Success and ValidateOutBuffer('fgrsnab');

    if Success then WriteLn('OK')
    else Panic('KO');
end;

{  Sample from https://brainfuck.org/wc.b }
procedure TestWc();
var
    Success: boolean;
    Code : AnsiString;
begin
    Write('- WC: ');

    ResetBuffers('Hello world'#10'this is me'#0);
    Success:= true;
    Code := '>>>+>>>>>+>>+>>+[<<],['#10;
    Code := Code + '     -[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<'#10;
    Code := Code + '         [>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]'#10;
    Code := Code + '     <[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>['#10;
    Code := Code + '         <+['#10;
    Code := Code + '             >+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]'#10;
    Code := Code + '             <[>+<-]>[>>>>>++>[-]]+<'#10;
    Code := Code + '         ]>[-<<<<<<]>>>>'#10;
    Code := Code + '     ],'#10;
    Code := Code + ' ]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]';
    InitializeBrainfuckInterpreter(Interpreter, Code, @DefaultCin, @DefaultCout, Debug);
    RunBrainfuckInterpreter(Interpreter, Debug);

    Success := Success and (NextInBufferPos = 23);
    Success := Success and ValidateOutBuffer(''#9'1'#9'5'#9'22'#10);

    if Success then WriteLn('OK')
    else Panic('KO');
end;

begin
    TestHelloWorld();
    TestEcho();
    TestRot13();
    TestWc();
    Halt(EXIT_SUCCESS);
end.