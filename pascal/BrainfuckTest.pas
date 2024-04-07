program BrainfuckTest;
uses BrainfuckUtility, BrainfuckInterpreter;
const Debug: boolean = false;

var
    CoutBuffer          : array of byte;
    CinBuffer           : array of byte;
    CurrentInBufferPos  : integer;
    Interpreter         : TBrainfuckInterpreter;

procedure ResetBuffers();
begin
     SetLength(CoutBuffer, 0);
     SetLength(CinBuffer, 0);
     CurrentInBufferPos := 0;
end;

procedure DefaultCout(x: byte);
begin
    SetLength(CoutBuffer, Length(CoutBuffer) + 1);
    CoutBuffer[Length(CoutBuffer) - 1] := x;
end;

function DefaultCin(): byte;
begin
   if CurrentInBufferPos <= Length(CinBuffer) then Panic('Reading over buffer limits');
   DefaultCin := CinBuffer[CurrentInBufferPos];
   Inc(CurrentInBufferPos);
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

procedure TestHelloWorld();
var
    Success: boolean;
    Code : AnsiString;
begin
    Write('- Hello world: ');

    Success:= true;
    Code := '++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.';
    InitializeBrainfuckInterpreter(Interpreter, Code, @DefaultCin, @DefaultCout, Debug);
    RunBrainfuckInterpreter(Interpreter, Debug);

    Success := ValidateOutBuffer('Hello World!'#10);

    if Success then WriteLn('OK')
    else Panic('KO');
end;

begin
    TestHelloWorld();
    Halt(EXIT_SUCCESS);
end.