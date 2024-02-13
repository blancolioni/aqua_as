private with As.Names;

private package As.Files is

   type Instance is tagged limited private;
   type Reference is access constant Instance'Class;

   function Base_Name (This : Instance'Class) return String;

   function File (Path : String) return Reference;

   type File_Context is
      record
         File   : Reference;
         Line   : Positive := 1;
         Column : Positive := 1;
      end record;

   function To_String
     (Context : File_Context)
      return String;

private

   type Instance is tagged limited
      record
         Path : As.Names.Symbol_Name;
      end record;

end As.Files;
