with As.Source;

private package As.Parser is

   function Has_Errors return Boolean;
   procedure Clear_Errors;

   function Load
     (Path : String)
      return As.Source.Reference;

end As.Parser;
