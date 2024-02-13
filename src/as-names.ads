private with Ada.Strings.Unbounded;

private package As.Names is

   type Symbol_Name is private;

   function "+" (X : String) return Symbol_Name;
   function "-" (X : Symbol_Name) return String;

private

   type Symbol_Name is
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function "+" (X : String) return Symbol_Name
   is (Text => Ada.Strings.Unbounded.To_Unbounded_String (X));

   function "-" (X : Symbol_Name) return String
   is (Ada.Strings.Unbounded.To_String (X.Text));

end As.Names;
