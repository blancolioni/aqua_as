with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body As.Options is

   package String_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   package Boolean_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Boolean);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Str_Values         : String_Maps.Map;
   Bool_Values        : Boolean_Maps.Map;
   Source_File_Vector : String_Vectors.Vector;

   Object_Name_Option   : constant String := "object file name";
   Config_Path_Option   : constant String := "config path";
   Write_Listing_Option : constant String := "write listing";
   Main_Program_Option  : constant String := "main program";

   -----------------
   -- Config_Path --
   -----------------

   function Config_Path return String is
   begin
      return Str_Values (Config_Path_Option);
   end Config_Path;

   ----------
   -- Load --
   ----------

   function Load return Boolean is
      use Ada.Command_Line;
      use Ada.Strings.Fixed;

      Arg_Index : Natural := 1;

      function Fail (Message : String) return Boolean;

      ----------
      -- Fail --
      ----------

      function Fail (Message : String) return Boolean is
      begin
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
         return False;
      end Fail;

   begin
      --  Defaults.
      Str_Values.Insert (Object_Name_Option, "a.out");
      Str_Values.Insert (Config_Path_Option, "");
      Bool_Values.Insert (Write_Listing_Option, False);
      Bool_Values.Insert (Main_Program_Option, False);

      while Arg_Index <= Argument_Count loop
         declare
            Arg : constant String := Argument (Arg_Index);

            function Take_Value (Inline : String; Has_Inline : Boolean)
                                 return String;

            ----------------
            -- Take_Value --
            ----------------

            function Take_Value (Inline : String; Has_Inline : Boolean)
                                 return String is
            begin
               if Has_Inline then
                  return Inline;
               else
                  Arg_Index := Arg_Index + 1;
                  return Argument (Arg_Index);
               end if;
            end Take_Value;

         begin
            if Arg'Length >= 2
              and then Arg (Arg'First .. Arg'First + 1) = "--"
            then

               --  Long option, optionally --name=value.

               declare
                  Text : constant String := Arg (Arg'First + 2 .. Arg'Last);
                  Eq   : constant Natural := Index (Text, "=");
                  Name : constant String :=
                           (if Eq = 0 then Text
                            else Text (Text'First .. Eq - 1));
                  Val  : constant String :=
                           (if Eq = 0 then "" else Text (Eq + 1 .. Text'Last));
                  Has_Val : constant Boolean := Eq /= 0;
               begin
                  if Name = "object-name" then
                     if not Has_Val and then Arg_Index = Argument_Count then
                        return Fail ("option --object-name requires a value");
                     end if;
                     Str_Values.Replace
                       (Object_Name_Option, Take_Value (Val, Has_Val));
                  elsif Name = "config-path" then
                     if not Has_Val and then Arg_Index = Argument_Count then
                        return Fail ("option --config-path requires a value");
                     end if;
                     Str_Values.Replace
                       (Config_Path_Option, Take_Value (Val, Has_Val));
                  elsif Name = "write-listing" then
                     Bool_Values.Replace (Write_Listing_Option, True);
                  elsif Name = "main" then
                     Bool_Values.Replace (Main_Program_Option, True);
                  else
                     return Fail ("unknown option: --" & Name);
                  end if;
               end;

            elsif Arg = "-o" then
               if Arg_Index = Argument_Count then
                  return Fail ("option -o requires a value");
               end if;
               Str_Values.Replace (Object_Name_Option, Take_Value ("", False));
            elsif Arg = "-l" then
               Bool_Values.Replace (Write_Listing_Option, True);
            elsif Arg = "-m" then
               Bool_Values.Replace (Main_Program_Option, True);
            elsif Arg'Length >= 1 and then Arg (Arg'First) = '-'
              and then Arg /= "-"
            then
               return Fail ("unknown option: " & Arg);
            else
               Source_File_Vector.Append (Arg);
            end if;
         end;
         Arg_Index := Arg_Index + 1;
      end loop;

      return True;

   end Load;

   ------------------
   -- Main_Program --
   ------------------

   function Main_Program return Boolean is
   begin
      return Bool_Values (Main_Program_Option);
   end Main_Program;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name return String is
   begin
      return Str_Values (Object_Name_Option);
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
      return Bool_Values (Write_Listing_Option);
   end Write_Listing;

end As.Options;
