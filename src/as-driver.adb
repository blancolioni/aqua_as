with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with As.Assembler;
with As.Options;

procedure As.Driver is

begin

   if not As.Options.Load then
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   if As.Options.Source_File_Count = 0 then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Ada.Command_Line.Command_Name
         & ": no input files");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   declare
      Is_Main_Program : constant Boolean := As.Options.Main_Program;
      Asm             : constant As.Assembler.Reference :=
                          As.Assembler.New_Assembler (Is_Main_Program);
   begin
      for I in 1 .. As.Options.Source_File_Count loop
         declare
            Path : constant String := As.Options.Source_File (I);
         begin
            if not Ada.Directories.Exists (Path) then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Command_Line.Command_Name
                  & ": "
                  & Path
                  & ": no such file or directory");
               Ada.Command_Line.Set_Exit_Status (3);
               return;
            end if;

            begin
               Asm.Load (Path);
            exception
               when E : others =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     Ada.Command_Line.Command_Name
                     & ": "
                     & Path
                     & ": cannot read: "
                     & Ada.Exceptions.Exception_Message (E));
                  Ada.Command_Line.Set_Exit_Status (4);
                  return;
            end;
         end;
      end loop;

      Asm.Assemble;

      if not Asm.Has_Errors then
         declare
            Main : constant String := As.Options.Source_File (1);
         begin
            Asm.List (Ada.Directories.Compose
                      (Ada.Directories.Containing_Directory (Main),
                         Ada.Directories.Base_Name (Main),
                         "lst"));
            Asm.Write (As.Options.Output_File_Name);
         end;
      end if;
   end;
end As.Driver;
