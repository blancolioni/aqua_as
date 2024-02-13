with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Parse_Args;

package body As.Options is

   use Parse_Args;

   AP : Argument_Parser;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Source_File_Vector : String_Vectors.Vector;

   function Load return Boolean is
   begin

      AP.Add_Option
        (Make_String_Option ("a.out"),
         "object file name", 'o', "object-name",
         "Write output to the given path (default: a.out)");

      AP.Add_Option
        (Make_Boolean_Option (False),
         "write listing", 'l', "write-listing",
         "Write a listing file");

      AP.Add_Option
        (Make_Boolean_Option (False),
         "main program", 'm', "main",
         "Compile to an object file suitable for running as a main program");

      AP.Allow_Tail_Arguments ("assembly source files ...");

      AP.Parse_Command_Line;

      if not AP.Parse_Success then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            AP.Parse_Message);
         return False;
      end if;

      for File_Name of AP.Tail loop
         Source_File_Vector.Append (File_Name);
      end loop;

      return True;

   end Load;

   ------------------
   -- Main_Program --
   ------------------

   function Main_Program return Boolean is
   begin
      return AP.Boolean_Value ("main program");
   end Main_Program;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name return String is
   begin
      return AP.String_Value ("object file name");
   end Output_File_Name;

   -----------------
   -- Source_File --
   -----------------

   function Source_File (Index : Positive) return String is
   begin
      return Source_File_Vector (Index);
   end Source_File;

   -----------------------
   -- Source_File_Count --
   -----------------------

   function Source_File_Count return Natural is
   begin
      return Source_File_Vector.Last_Index;
   end Source_File_Count;

   -------------------
   -- Write_Listing --
   -------------------

   function Write_Listing return Boolean is
   begin
      return AP.Boolean_Value ("write listing");
   end Write_Listing;

end As.Options;
