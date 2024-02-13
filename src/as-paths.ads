package As.Paths is

   Config_Path : constant String :=
     "E:\git\aqua\aqua_as\share\aqua_as";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end As.Paths;
