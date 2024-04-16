with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers;        use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BrainfuckInterpreter is
   type Byte is range 0 .. 355;

   function JumpTableHashFunction (key : Natural) return Hash_Type;

   package TJumpTable is new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => Natural, Element_Type => Natural,
       Hash     => JumpTableHashFunction, Equivalent_Keys => "=");

   function DefaultCin return Byte;
   procedure DefaultCout (c : Byte);

   type TCin is access function return Byte;
   type TCout is access procedure (c : Byte);

   type TBrainfuckInterpreter is tagged record
       Program   : Unbounded_String;
       JumpTable : TJumpTable.Map;
       Cin       : TCin;
       Cout      : TCout;
   end record;

   procedure Initialize (
    interpreter : in out TBrainfuckInterpreter;
    program : Unbounded_String;
    Cin : TCin;
    Cout : TCout);
   procedure Run (interpreter : in out TBrainfuckInterpreter);
end BrainfuckInterpreter;
