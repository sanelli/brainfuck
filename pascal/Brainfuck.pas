program Brainfuck;
uses BrainfuckInterpreter, BrainfuckUtility;

var
    Debug           : boolean;
    Interpreter     : TBrainfuckInterpreter;

procedure DefaultCout(x: byte);
begin
   Write(Chr(x));
end;

function DefaultCin(): byte;
var ch: char;
begin
   Read(ch);
   DefaultCin := Ord(ch);
end;

function ReadCode(Filename: string): AnsiString;
var
    InputFile       : file of char;
    CurrentError    : integer;
    CurrentErrorStr : string;
    CurrentChar     : char;
begin
    ReadCode := '';

    Assign(InputFile, Filename);
    CurrentError := IOResult;
    if CurrentError <> 0 then begin
        Str(CurrentError, CurrentErrorStr);
        Panic('Cannot open file "' + Filename +'" (code: ' + CurrentErrorStr +')');
    end;

    Reset(InputFile);
    CurrentError := IOResult;
    if CurrentError <> 0 then begin
        Str(CurrentError, CurrentErrorStr);
        Panic('Cannot open file "' + Filename +'" (code: ' + CurrentErrorStr +')');
    end;

    while not Eof(InputFile) do begin
        Read(InputFile, CurrentChar);
        CurrentError := IOResult;
        if CurrentError <> 0 then begin
            Str(CurrentError, CurrentErrorStr);
            Panic('Cannot read file "' + Filename +'" (code: ' + CurrentErrorStr +')');
        end;

        ReadCode := ReadCode + CurrentChar;
    end;

    Close(InputFile);
    if CurrentError <> 0 then begin
        Str(CurrentError, CurrentErrorStr);
        Panic('Cannot clode file "' + Filename +'" (code: ' + CurrentErrorStr +')');
    end;
end;

begin
    if (paramCount() <> 1) and (paramCount() <> 2) then begin
        writeLn('Usage: ', paramStr(0), ' <filename> [-debug]');
        Panic('')
    end;

    Debug := (paramCount() = 2) and (paramStr(2) = '-debug');
    InitializeBrainfuckInterpreter(Interpreter, ReadCode(paramStr(1)), @DefaultCin, @DefaultCout, Debug);
    RunBrainfuckInterpreter(Interpreter, Debug);
end.