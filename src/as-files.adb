with Ada.Directories;

package body As.Files is

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (This : Instance'Class) return String is
   begin
      return Ada.Directories.Base_Name
        (As.Names."-" (This.Path));
   end Base_Name;

   ----------
   -- File --
   ----------

   function File (Path : String) return Reference is
   begin
      return new Instance'(Path => As.Names."+" (Path));
   end File;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Context : File_Context)
      return String
   is
   begin
      if Context.File = null then
         return "unknown";
      else
         declare
            File_Name    : constant String := Context.File.Base_Name;
            Line_Image   : constant String := Context.Line'Image;
            Column_Image : constant String := Context.Column'Image;
         begin
            return File_Name & ":"
              & Line_Image (2 .. Line_Image'Last) & ":"
              & Column_Image (2 .. Column_Image'Last);
         end;
      end if;
   end To_String;

end As.Files;
