private package As.Parser.Tokens is

   type Token is
     (Tok_None, Tok_Bad_Character,
      Tok_End_Of_File, Tok_End_Of_Line,
      Tok_Identifier, Tok_Integer_Constant,
      Tok_Comment, Tok_String,
      Tok_Local_Reference,

      Tok_Colon,
      Tok_Double_Colon,
      Tok_Equal_Sign,
      Tok_Double_Equal_Sign,
      Tok_Percent_Sign,
      Tok_Number_Sign,
      Tok_At_Sign,
      Tok_Left_Parenthesis,
      Tok_Right_Parenthesis,
      Tok_Comma,
      Tok_Left_Angle_Bracket,
      Tok_Right_Angle_Bracket,
      Tok_Plus_Sign,
      Tok_Minus_Sign,
      Tok_Asterisk,
      Tok_Slash,
      Tok_Backslash,
      Tok_Ampersand,
      Tok_Exclamation_Point,
      Tok_Double_Quote,
      Tok_Single_Quote,
      Tok_Up_Arrow);

   subtype Operator_Token is
     Token range Tok_Plus_Sign .. Tok_Backslash;

end As.Parser.Tokens;
