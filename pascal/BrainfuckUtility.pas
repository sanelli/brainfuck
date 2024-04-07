unit BrainfuckUtility;
interface
    const
        EXIT_SUCCESS: integer = 0;
        EXIT_FAILURE: integer = 1;
    procedure Panic(Message: string);
implementation
    procedure Panic(Message: string);
    begin
        WriteLn(Message);
        Halt(EXIT_FAILURE);
    end;
end.