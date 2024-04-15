with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers;        use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package BrainfuckInterpreter is
   function JumpTableHashFunction (key : Natural) return Hash_Type;

   package THashTable is new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => Natural, Element_Type => Natural,
       Hash     => JumpTableHashFunction, Equivalent_Keys => "=");

   type TBrainfuckInterpreter is record
       Program   : Unbounded_String;
       JumpTable : THashTable.Map;
   end record;
end BrainfuckInterpreter;
