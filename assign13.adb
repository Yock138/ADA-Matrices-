with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure assign13 is

-- This program prompts a user to enter a lucky number and then displays
-- the student's name with that number
--
-- By: Dylan Yockey
--     CS 1510 (Spring)
--
-- Date: May 3rd, 2013
--
-- Input
--    The name of the data file with student information
--    A user-entered lucky number
--    A lucky number
--
-- Output
--    An unordered list of students (by name)
--    An ordered list of students (by name)
--    The student's name with the lucky number
--
-- Assumptions
--   1. The data file is formatted with name on one line followed by
--      lucky number on a second line
--   2. The name is in the form Last Name comma First Name

   subtype Name_String is String (1 .. 20);
   type    Lucky_Range is range 1000 .. 9999;

   type Name_Rec is
      record
         First : Name_String;
         Last  : Name_String;
      end record;

   type Student_Rec is
      record
         Name         : Name_Rec;
         Lucky_Number : Lucky_Range;
      end record;

   type    Student_Array is array (Positive range <>) of Student_Rec;
   subtype Class_Array   is Student_Array (1 .. 50);

   package Lucky_IO is new Ada.Text_IO.Integer_IO (Num => Lucky_Range);

   ----------------------------------------------------------------------------
   function Index (Source  : in String;
                   Pattern : in Character) return Natural is
   -- Returns the index of the first occurrence of Pattern in Source
   -- Preconditions  : None
   -- Postconditions : If Source contains Pattern then
   --                     Source (Result) = Pattern
   --                  Else
   --                     Result is zero

      Result : Natural;

   begin

      Result := Source'First;

      Index_Loop :
      loop
         exit Index_Loop when Result > Source'Last or else Source (Result) = Pattern;
         Result := Result + 1;
      end loop Index_Loop;

      if Result > Source'Last then
         Result := 0;
      end if;

      return Result;
   end Index;

   ----------------------------------------------------------------------------
   procedure Get_Name (File : in out Ada.Text_IO.File_Type;
                       Name :    out Name_Rec) is
   -- Gets a student name from single line of a data file
   -- Preconditions  : File is open for input
   -- Postconditions : Name contains blank padded first and last names

      subtype Name_String is String (1 .. 40);

      Whole_Name     : Name_String;
      Length         : Natural;
      Comma_Position : Natural;

   begin
      Ada.Text_IO.Get_Line (File => File,
                            Item => Whole_Name,
                            Last => Length);
      Comma_Position := Index (Source  => Whole_Name (1 .. Length),
                               Pattern => ',');
      -- Copy the last name and first name into the record
      Name.Last (1 .. Comma_Position - 1)  := Whole_Name (1 .. Comma_Position - 1);
      Name.First (1 .. Length - Comma_Position - 1) := Whole_Name (Comma_Position + 2 .. Length);

      -- Pad the last name and first name with blanks
      Name.Last  (Comma_Position .. Name.Last'Last)          := (others => ' ');
      Name.First (Length - Comma_Position .. Name.First'Last) := (others => ' ');
   end Get_Name;

   ----------------------------------------------------------------------------
   procedure Get_Students (Class : out Student_Array;
                           Last  : out Natural) is
   -- Gets the data for a class of students
   -- Preconditions  : None
   -- Postconditions : Class contains the data read
   --                  Last is the index of the last element read
   --                  If no elements are read, Last is Class'First - 1

      subtype File_String is String (1 .. 80);

      File      : Ada.Text_IO.File_Type;
      File_Name : File_String;
      Length    : Natural;

   begin
      Ada.Text_IO.Put_Line ("Enter the name of the data file with student information.");
      Ada.Text_IO.Get_Line (Item => File_Name,
                            Last => Length);
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name (1 .. Length));

      Last := Class'First - 1;

      -- Get the student data from the file
      -- Each iteration, get the data for one student
      loop
         exit when Ada.Text_IO.End_Of_File (File);
         Last := Last + 1;
         -- Get the student's name
         Get_Name (File => File,
                   Name => Class (Last).Name);
         -- Get the student's lucky number
         Lucky_IO.Get (File => File,
                       Item => Class (Last).Lucky_Number);

         if not Ada.Text_IO.End_Of_File (File) then
            Ada.Text_IO.Skip_Line (File);
         end if;
      end loop;

      Ada.Text_IO.Close (File);
   end Get_Students;

   ----------------------------------------------------------------------------
   procedure Display_Name (Students : in Name_Rec) is
   -- Display all the student names
   -- Preconditions  : None
   -- Postconditions : The students' last and first names are displayed

      Blank_Pos : Natural;

   begin
         -- Display the first name without the padded blanks
         Blank_Pos := Index (Source  => Students.Last,
                             Pattern => ' ');
         if Blank_Pos > 0 then
            Ada.Text_IO.Put (Students.Last (1 .. Blank_Pos - 1));
         else
            Ada.Text_IO.Put (Students.Last);
         end if;

         Ada.Text_IO.Put (", ");  -- Blank between first and last name

         -- Display the last name without the padded blanks
         Blank_Pos := Index (Source  => Students.First,
                             Pattern => ' ');
         if Blank_Pos > 0 then
            Ada.Text_IO.Put (Students.First (1 .. Blank_Pos - 1));
         else
            Ada.Text_IO.Put (Students.First);
         end if;

   end Display_Name;

   ----------------------------------------------------------------------------
   procedure Display_Students (Students : in Student_Array) is
   -- Display all the student information, one student per line
   -- Preconditions  : None
   -- Postconditions : Data in array Students is displayed

   begin

      for I in Students'Range loop
         -- Display the student's name
         Display_Name (Students => Students (I).Name);
         -- Display the student's lucky number
         Ada.Text_IO.Set_Col (To => 25);
         Lucky_IO.Put (Item  => Students (I).Lucky_Number,
                       Width => 1);
         Ada.Text_IO.New_Line;
      end loop;

   end Display_Students;

   ----------------------------------------------------------------------------
   procedure Search_Ordered (List  : in     Student_Array;
                             Item  : in     Lucky_Range;
                             Found :    out Boolean;
                             Index :    out Integer) is
   -- Search List for the given Item
   --
   -- Preconditions : Items in List are in ascending order
   --
   -- Postconditions : If Item is in List
   --                     Found is True
   --                     Index is the location of Item in List
   --                  If Item is not in List
   --                     Found is False
   --                     Index is the location of where Item would be
   --                     if it were in List


   begin
      Index := List'First; -- Start searching at the first component
      Search_Loop : -- Search for Item in List
      loop -- Each iteration, check one value in List
         exit Search_Loop when (Index > List'Last) or else
                               (Item <= List (Index).Lucky_Number);
         Index := Index + 1;
      end loop Search_Loop;

      -- Determine whether or not the item was found
      Found := (Index <= List'Last) and then (Item = List (Index).Lucky_Number);

   end Search_Ordered;

   ----------------------------------------------------------------------------
   procedure Sort_By_Lucky_Number (List : in out Student_Array) is

      First_Unsorted : Integer;     -- Index of first element of unsorted list
      Location       : Integer;     -- Insertion location in sorted list
      Value          : Student_Rec; -- Copy of first element of unsorted list
      Found          : Boolean;     -- Not used, needed for call to Search_ordered

   begin -- Insertion_Sort

      First_Unsorted := List'First + 1; -- Sorted part has one element at start
      loop -- Each iteration, the first element in the unsorted part of the
         -- array is inserted into the sorted part of the array
         exit when First_Unsorted > List'Last;
         -- Find where in the sorted portion of the array to insert
         Search_Ordered (List => List (List'First .. First_Unsorted - 1),
                                       Item       => List (First_Unsorted).Lucky_Number,
                                       Found      => Found,
                                       Index      => Location);
         -- Make a copy of List (First_Unsorted) so slide won't destroy it
         Value := List (First_Unsorted);
         -- Open up a space for the element by sliding all below it down
         List (Location + 1 .. First_Unsorted) := List (Location .. First_Unsorted - 1);
         -- Insert the current value
         List (Location) := Value;
         -- Shrink the unsorted part of the array
         First_Unsorted := First_Unsorted + 1;
      end loop;

   end Sort_By_Lucky_Number;

   ----------------------------------------------------------------------------
   procedure Binary_Search (List  : in  Student_Array;
                            Item  : in  Lucky_Range;
                            Found : out Boolean;
                            Index : out Integer) is
   -- Search List for the given Item
   --
   -- Preconditions : Items in List are in ascending order
   --
   -- Postconditions : If Item is in the List
   --                     Found is True
   --                     Index is the location of Item in List
   --                  If Item is not in List
   --                     Found is False
   --                     Index is undefined

      First  : Integer; -- Lower index bound of list
      Last   : Integer; -- Upper index bound of list
      Middle : Integer; -- Middle index

   begin

      First := List'First; -- Set up initial
      Last  := List'Last;  --    list bounds
      Found := False;

      Search_Loop : -- Search for Item in List (First .. Last)
      loop          -- Each iteration, check the middle value and half list
         exit Search_Loop when Found or      -- We found item
                               First > Last; -- No items left in list
         Middle := (First + Last) / 2;
         if Item <  List (Middle).Lucky_Number then
            -- Item is not in List (Middle .. Last)
            Last := Middle - 1; -- Redefine list to 1st half
         elsif Item > List (Middle).Lucky_Number then
            -- Item is not in List (First .. Middle)
            First := Middle + 1; -- Redefine list to 2nd half
         else
            Found := True;
            Index := Middle;
         end if;
      end loop Search_Loop;
   end Binary_Search;

-------------------------------------------------------------------------------

   Class        : Class_Array;   -- Information of a class
   Size         : Natural;       -- The number of students in the class
   Lucky_Num    : Natural;       -- A user-entered lucky number
   Location     : Integer;       -- The student's location
   Found        : Boolean;       -- If the number was found

begin
   Get_Students (Class => Class,
                 Last  => Size);
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put_Line ("The unordered list of students");
   Ada.Text_IO.New_Line;
   Display_Students (Class (1 .. Size));
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put_Line ("Students ordered by lucky number");
   Ada.Text_IO.New_Line;
   Sort_By_Lucky_Number (Class (1 .. Size));
   Display_Students (Class (1 .. Size));
   Ada.Text_IO.New_Line (2);

   Lucky_Number_Loop :
   loop
      Ada.Text_IO.Put_Line ("Please enter a lucky number");
      Ada.Integer_Text_IO.Get (Lucky_Num);
      exit Lucky_Number_Loop when Lucky_Num < Integer (Lucky_Range'First)
                               or Lucky_Num > Integer (Lucky_Range'Last);
      Binary_Search (List  => Class (1 .. Size), Item  => Lucky_Range (Lucky_Num),
                     Found => Found,             Index => Location);
      if Found then
         Display_Name (Class (Location).Name);
         Ada.Text_IO.Put (" has the lucky number ");
         Ada.Integer_Text_IO.Put (Item => Lucky_Num, Width => 1);
         Ada.Text_IO.Put_Line (".");
      else
         Ada.Text_IO.Put ("No student has lucky number ");
         Ada.Integer_Text_IO.Put (Lucky_Num, Width => 1);
         Ada.Text_IO.Put_Line (".");
      end if;
   end loop Lucky_Number_Loop;

end assign13;
      