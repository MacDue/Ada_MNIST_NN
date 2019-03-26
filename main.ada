with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Test;

procedure Custom_Floating_Types is
   type T3  is digits 3;
   type T15 is digits 15;
   type T18 is digits 1;
   type FixedString is array (Natural range <>) of Character;
   B : FixedString(0..2) := "Cat";
   procedure Apple is
   begin
     Put_Line("Apple");
   end;
   type Moos is (MOOOOOOOOOOOOO, Mooo, MOOOOOOOOOOOOOOOOOOOOOO, Moooo);
    mapmap : constant Moos := Mooo;
   type Index is range 0 .. 3;
   type MemeArray is array (Integer range <>) of Integer;
   J : MemeArray ( -1 .. 0 ) := (-1 => 0, 0 => 1);
   type MyArray is array (Index) of Integer;
   type WridArray is array (Moos) of Integer;
   Whay : constant WridArray := (1, 2, 3, 4);
   Arr : MyArray := (0, 23, 3,42);
   A : String(1 .. 3) := "Cat";
   C : Test.MemeMap;
begin
   null;
   Apple;
   Arr(0) := 666;
   J(-1) := 123;
   -- Put_Line(String(FixedString));
   Put_Line(Integer'Image(Arr(0)));
   Put_Line(Integer'Image(Whay(MOOOOOOOOOOOOOOOOOOOOOO)));
   Put_Line("Mapmap: " & Moos'Image(mapmap));
   Put_Line ("T3  requires " & Integer'Image (T3'Size) & " bits");
   Put_Line ("T15 requires " & Integer'Image (T15'Size) & " bits");
   Put_Line ("T18 requires " & Integer'Image (T18'Size) & " bits");

   C := Test.GetMemeMap;

   Put_Line(C(Test.Dank));

   declare
   type Integer_Ptr is access Integer;
   procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Ptr);
   Ptr : Integer_Ptr := new Integer; -- Allocated in the heap
    begin
      -- Ptr := 13;
       Ptr.all := 29377;
       Put_Line("WWWEee" & Integer'Image(Ptr.all));

       for I in Arr'Range loop
        Put_Line(Integer'Image(Arr(I)));
       end loop;

       Free (Ptr); -- Explicit deallocation
    end;
end Custom_Floating_Types;
