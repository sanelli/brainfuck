with Ada.Text_IO; use Ada.Text_IO;
with BrainfuckInterpreter; use BrainfuckInterpreter;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Brainfuck is
   package CLI renames Ada.Command_Line;
   package IO renames Ada.Text_IO;
   package UIO renames Ada.Text_IO.Unbounded_IO;

   Interpreter : TBrainfuckInterpreter;
   F : File_Type;
   Program : Unbounded_String;
begin
   Program := Ada.Strings.Unbounded.To_Unbounded_String ("");

   if CLI.Argument_Count /= 1 then
      IO.Put_Line ("Usage: brainfuck <filename>");
   else
      Open (F, In_File, CLI.Argument (1));
      while not End_Of_File (F) loop
         Program := Program & UIO.Get_Line (F);
      end loop;
      Close (F);

      Interpreter.Initialize(Program, BrainfuckInterpreter.DefaultCin'Access, BrainfuckInterpreter.DefaultCout'Access);
      Interpreter.Run;
   end if;
end Brainfuck;
