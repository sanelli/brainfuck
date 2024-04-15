package body BrainfuckInterpreter is
   function JumpTableHashFunction (key : Natural) return Hash_Type is
   begin
      return Hash_Type (key);
   end JumpTableHashFunction;
end BrainfuckInterpreter;
