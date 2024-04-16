with Ada.Containers.Vectors;
with Ada.Text_IO;
with BrainfuckInterpreterException; use BrainfuckInterpreterException;

package body BrainfuckInterpreter is
   TapeSize : constant := 30000;

   package JumpTableStack is new Ada.Containers.Vectors
      (Index_Type   => Natural, Element_Type => Natural);

   function JumpTableHashFunction (key : Natural) return Hash_Type is
   begin
      return Hash_Type (key);
   end JumpTableHashFunction;

   function DefaultCin return Byte is
      Chr : Character;
   begin
      Ada.Text_IO.Get_Immediate (Chr);
      return Character'Pos (Chr);
   end DefaultCin;

   procedure DefaultCout (c : Byte) is
   begin
      Ada.Text_IO.Put (Character'Val (c));
   end DefaultCout;

   procedure BuildJumpTable (
      interpreter : in out TBrainfuckInterpreter;
      program : Unbounded_String) is
      Stack : JumpTableStack.Vector;
   begin
      for ProgramCounter in 1 .. Length (program) loop
         case Element (program, ProgramCounter) is
            when '[' =>
               Stack.Append (ProgramCounter);
            when ']' =>
               if Stack.Is_Empty then
                  raise TBrainfuckInterpreterException
                     with "No matching start jump character";
               end if;

               interpreter.JumpTable.Insert
                  (Stack.Last_Element, ProgramCounter);
               interpreter.JumpTable.Insert
                  (ProgramCounter, Stack.Last_Element);
               Stack.Delete_Last;

            when others =>
               null;
         end case;
      end loop;

      if Stack.Length /= 0 then
         raise TBrainfuckInterpreterException
            with "No matching ending jump character";
      end if;

   end BuildJumpTable;

   procedure Initialize (
      interpreter : in out TBrainfuckInterpreter;
      program : Unbounded_String;
      Cin : TCin;
      Cout : TCout) is
   begin
      interpreter.Cin := Cin;
      interpreter.Cout := Cout;
      interpreter.BuildJumpTable (program);
   end Initialize;

   procedure Run (interpreter : in out TBrainfuckInterpreter) is
      Tape : array (-TapeSize .. TapeSize) of Byte;
      Pointer : Integer;
      ProgramCounter : Natural;
   begin
      Pointer := 0;
      for TapeIndex in Tape'First .. Tape'Last loop
         Tape (TapeIndex) := 0;
      end loop;

      ProgramCounter := 1;

      while ProgramCounter <= Length (interpreter.Program) loop

         case Element (interpreter.Program, ProgramCounter) is
            when '>' => Pointer := Pointer + 1;
            when '<' => Pointer := Pointer - 1;
            when '+' => Tape (Pointer) := Tape (Pointer) + 1;
            when '-' => Tape (Pointer) := Tape (Pointer) - 1;
            when '.' => interpreter.Cout (Tape (Pointer));
            when ',' => Tape (Pointer) := interpreter.Cin.all;
            when '[' =>
               if Tape (Pointer) = 0 then
                  ProgramCounter := TJumpTable.Element
                     (interpreter.JumpTable, ProgramCounter) - 1;
               end if;
            when ']' =>
               if Tape (Pointer) /= 0 then
                  ProgramCounter := TJumpTable.Element
                     (interpreter.JumpTable, ProgramCounter) - 1;
               end if;
            when others => null;
         end case;

         ProgramCounter := ProgramCounter + 1;
      end loop;

   end Run;
end BrainfuckInterpreter;
