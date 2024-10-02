package As.Options is

   function Load return Boolean;

   function Config_Path return String;

   function Source_File_Count return Natural;
   function Source_File (Index : Positive) return String;

   function Output_File_Name return String;
   function Write_Listing return Boolean;
   function Main_Program return Boolean;

end As.Options;
