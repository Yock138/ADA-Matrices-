with Ada.Float_Text_IO;
with Ada.Text_IO;
procedure Assign12 is

-- This program performs the basic matrix operations of addition, subtraction, and multiplication
-- All operations are performed on square matrices (number of rows equals number of columns)
--
-- By: Dylan Yockey
--     CS 1510 (Spring)
--
-- Date: April 26th, 2013
--
-- Input
--    Matrix size
--    Two Matrices
--
-- Output
--    The two input matrices
--    Sum of the two input matrices
--    Difference of the two input matrices
--    Product of the two input matrices
--
-- Assumptions
--    All input values are valid
--    The correct number of values are entered

   -- Types instead of subtypes so we do not create a mistake in our 2-dimensional array
   type Row_Index is range 1 .. Integer'Last;
   type Col_Index is range 1 .. Integer'Last;

   -- I/O Packages for index types
   package Row_Index_IO is new Ada.Text_IO.Integer_IO (Num => Row_Index);
   use Row_Index_IO;

   package Column_Index_IO is new Ada.Text_IO.Integer_IO (Num => Col_Index);
   use Column_Index_IO;

   type Unconstrained_Matrix_Type is array (Row_Index range <>, Col_Index range <>) of Float;

   ----------------------------------------------------------------------------
   function "+" (Left  : in Unconstrained_Matrix_Type;
                 Right : in Unconstrained_Matrix_Type) return Unconstrained_Matrix_Type is
   -- Adds two matrices
   -- Preconditions : Left'Range(1) is the same as Right'Range(1)
   --                 Left'Range(2) is the same as Right'Range(2)
   -- Postcondition : the sum of Left and Right is returned

      subtype Matrix_Type is Unconstrained_Matrix_Type (Left'Range (1), Left'Range (2));
      Result : Matrix_Type;

   begin

      for Row in Left'Range (1) loop    -- This loop adds the corresponding numbers from each matrix,
         for Column in Left'Range (2) loop -- going through the columns in each row
            Result (Row, Column) := Left (Row, Column) + Right (Row, Column);
         end loop;
      end loop;

      return Result;
   end "+";

   ----------------------------------------------------------------------------
   function "-" (Left  : in Unconstrained_Matrix_Type;
                 Right : in Unconstrained_Matrix_Type) return Unconstrained_Matrix_Type is
   -- Subtracts two matrices
   -- Preconditions : Left'Range(1) is the same as Right'Range(1)
   --                 Left'Range(2) is the same as Right'Range(2)
   -- Postcondition : the difference of Left and Right is returned

      subtype Matrix_Type is Unconstrained_Matrix_Type (Left'Range (1), Left'Range (2));
      Result : Matrix_Type;

   begin

      for Row in Left'Range (1) loop    -- This loop adds the corresponding numbers from each matrix,
         for Column in Left'Range (2) loop -- going through the columns in each row
            Result (Row, Column) := Left (Row, Column) - Right (Row, Column);
         end loop;
      end loop;

      return Result;
   end "-";

   ----------------------------------------------------------------------------
   function "*" (Left  : in Unconstrained_Matrix_Type;
                 Right : in Unconstrained_Matrix_Type) return Unconstrained_Matrix_Type is
   -- Multiplies two matrices
   -- Preconditions : Left'Range (2) is the same as Right'Range (1)
   -- Postcondition : The product of Left and Right is returned

      subtype Matrix_Type is Unconstrained_Matrix_Type (Left'Range (1), Right'Range (2));

      Result : Matrix_Type := (others => (others => 0.0)); -- Reset / initialize result to 0.0

   begin

      for Left_Row in Left'Range (1) loop           -- Iterates through the ROWS in matrix 1
         for Right_Column in Right'Range (2) loop   -- Iterates through the COLUMNS in matrix 2
            for Left_Column in Left'Range (2) loop  -- Iterates through the COLUMNS in matrix 1

               Result (Left_Row, Right_Column) := Result  (Left_Row, Right_Column)
                                                  + Left  (Left_Row, Left_Column)
                                                  * Right (Row_Index (Left_Column), Right_Column);
            end loop;
         end loop;
      end loop;

      return Result;
   end "*";

   ----------------------------------------------------------------------------
   procedure Put (Item : in Unconstrained_Matrix_Type;
                  Fore : in Positive := 4;
                  Aft  : in Positive := 1;
                  Exp  : in Natural  := 0) is
   -- Loops through a matrix (two-dimensional array) and neatly displays each value
   -- Preconditions  : None
   -- Postconditions : Displays each value in a matrix

   begin
      for Row in Item'Range (1) loop    -- This loop displays a matrix by going through
         for Col in Item'Range (2) loop -- all the columns in every row and displaying the value
            Ada.Float_Text_IO.Put (Item => Item (Row, Col),
                                   Fore => Fore,
                                   Aft  => Aft,
                                   Exp  => Exp);
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Put;

   ----------------------------------------------------------------------------
   procedure Get (Item : out Unconstrained_Matrix_Type) is
   -- Loops through a matrix (two-dimensional array) and gets each value
   -- Preconditions  : None (we already assumed that all input values were valid)
   -- Postconditions : Gets each value in a matrix

   begin
      for Row in Item'Range (1) loop    -- This loop gets a user-entered value by going
         for Col in Item'Range (2) loop -- through the columns in each row
            Ada.Float_Text_IO.Get (Item => Item (Row, Col));
         end loop;
      end loop;
   end Get;

   ----------------------------------------------------------------------------
   procedure Do_Matrix_Arithmetic (Rows_In_A : in Row_Index;
                                   Cols_In_A : in Col_Index;
                                   Rows_In_B : in Row_Index;
                                   Cols_In_B : in Col_Index) is
   -- Does the arithmetic for the matrices
   -- Preconditions  : None
   -- Postconditions : Gets the values for both matrixes,
   --                  Displays the values for both matrixes,
   --                  Calls the functions which do the arithmetic
   --                  Displays the results of the arithmetic
   --                  Displays an error message if the matrixes are the wrong size to add/sub/mult

      subtype A_Matrix_Type is Unconstrained_Matrix_Type (1 .. Rows_In_A, 1 .. Cols_In_A);
      subtype B_Matrix_Type is Unconstrained_Matrix_Type (1 .. Rows_In_B, 1 .. Cols_In_B);

      A : A_Matrix_Type;
      B : B_Matrix_Type;

   begin
      Ada.Text_IO.Put_Line ("Enter the values for matrix A");
      Ada.Text_IO.New_Line;
      Get (A);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Enter the values for matrix B");
      Ada.Text_IO.New_Line;
      Get (B);
      Ada.Text_IO.New_Line (2);

      Ada.Text_IO.Put_Line ("Matrix A is");
      Ada.Text_IO.New_Line;
      Put (A);
      Ada.Text_IO.New_Line (2);

      Ada.Text_IO.Put_Line ("Matrix B is");
      Ada.Text_IO.New_Line;
      Put (B);
      Ada.Text_IO.New_Line (2);

      if A'Last (1) = B'Last (1) and A'Last (2) = B'Last (2) then
         Ada.Text_IO.Put_Line ("A + B is");
         Ada.Text_IO.New_Line;
         Put (A + B);
         Ada.Text_IO.New_Line (2);

         Ada.Text_IO.Put_Line ("A - B is");
         Ada.Text_IO.New_Line;
         Put (A - B);
      else
         Ada.Text_IO.Put_Line ("Matrices are of wrong size to add or subtract");
      end if;
      Ada.Text_IO.New_Line (2);

      if Row_Index (A'Last (2)) = B'Last (1) then
         Ada.Text_IO.Put_Line ("A * B is");
         Ada.Text_IO.New_Line;
         Put (A * B);
      else
         Ada.Text_IO.Put_Line ("Matrices are of wrong size to multiply");
      end if;
      Ada.Text_IO.New_Line (2);

   end Do_Matrix_Arithmetic;

   -- The sizes of the two matrices
   Rows_In_First     : Row_Index;
   Columns_In_First  : Col_Index;
   Rows_In_Second    : Row_Index;
   Columns_In_Second : Col_Index;

begin
   Ada.Text_IO.Put_Line ("Enter the number of rows (in your first input) and columns (in your second input) in your first matrix");
   Get (Rows_In_First);
   Get (Columns_In_First);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Enter the number of rows (in your first input) and columns (in your second input) in your second matrix");
   Get (Rows_In_Second);
   Get (Columns_In_Second);
   Ada.Text_IO.New_Line;
   Do_Matrix_Arithmetic (Rows_In_A => Rows_In_First,
                         Cols_In_A => Columns_In_First,
                         Rows_In_B => Rows_In_Second,
                         Cols_In_B => Columns_In_Second);
end Assign12;