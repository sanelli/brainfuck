unit BrainfuckTyping;
interface
    uses BrainfuckUtility;

    type
        TProgramCounter = integer;
        TTapeEntry      = byte;
        TProgramCounterStack = array of TProgramCounter;

    procedure Clear(var Stack: TProgramCounterStack);
    procedure Push(var Stack: TProgramCounterStack; Value: TProgramCounter);
    function Pop(var Stack: TProgramCounterStack) : TProgramCounter;
    function Peek(var Stack: TProgramCounterStack) : TProgramCounter;
    function IsEmpty(var Stack: TProgramCounterStack) : boolean;

implementation

    procedure Clear(var Stack: TProgramCounterStack);
    begin
        SetLength(Stack, 0);
    end;

    procedure Push(var Stack: TProgramCounterStack; Value: TProgramCounter);
    begin
        SetLength(Stack, Length(Stack) + 1);
        Stack[Length(Stack) - 1] := Value;
    end;

    function Pop(var Stack: TProgramCounterStack) : TProgramCounter;
    begin
        if Length(Stack) <= 0 then Panic('Cannot pop from an empty stack');

        Pop := Stack[Length(Stack) - 1];
        SetLength(Stack, Length(Stack) - 1);
    end;

    function Peek(var Stack: TProgramCounterStack) : TProgramCounter;
    begin
        if Length(Stack) <= 0 then Panic('Cannot peek from an empty stack');

        Peek := Stack[Length(Stack) - 1];
    end;

    function IsEmpty(var Stack: TProgramCounterStack) : boolean;
    begin
        IsEmpty := Length(Stack) = 0;
    end;
end.