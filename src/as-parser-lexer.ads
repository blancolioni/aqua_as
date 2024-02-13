with As.Files;

with As.Parser.Tokens;            use As.Parser.Tokens;

private package As.Parser.Lexer is

   procedure Open (Path : String);
   procedure Close;

   procedure Scan;

   procedure Expect (Expected_Tok : Token);

   procedure Skip_To (Search_Tok : Token;
                      Consume    : Boolean := True);

   procedure Skip_Rest_Of_Line;

   procedure Expect_End_Of_Line;

   procedure Error (Message : String);
   procedure Warning (Message : String);
   procedure Information (Message : String);

   function Has_Errors return Boolean;
   function Has_Warnings return Boolean;
   procedure Clear_Errors;

   function Tok return Token;
   function Next_Tok return Token;
   function Tok_Text return String;

   function Tok_Line return Positive;
   function Tok_Column return Positive;
   function Tok_File return As.Files.Reference;

   function Tok_Context return As.Files.File_Context;

end As.Parser.Lexer;
