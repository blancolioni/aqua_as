with As.Parser.Tokens;            use As.Parser.Tokens;
with As.Parser.Lexer;             use As.Parser.Lexer;

with As.Expressions;
with As.Files;

package body As.Parser is

   type Priority_Range is range 3 .. 4;

   Operator_Priority : constant array (As.Expressions.Operator_Type)
     of Priority_Range :=
       (3, 3, 4, 4, 4);

   function Parse_Line
      return As.Source.Source_Line;

   function At_Expression
     return Boolean;

   function At_Operator
     return Boolean;

   function Current_Operator return As.Expressions.Operator_Type
     with Pre => At_Operator;

   function Parse_Expression
     return As.Expressions.Reference;

   function Parse_Operator_Expression
     (Priority : Priority_Range)
      return As.Expressions.Reference;

   function Parse_Primary_Expression
     return As.Expressions.Reference;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression
     return Boolean
   is
   begin
      return Tok in Tok_Identifier | Tok_Integer_Constant | Tok_String
        | Tok_Local_Reference
        | Tok_Plus_Sign | Tok_Minus_Sign
        | Tok_Left_Parenthesis | Tok_Percent_Sign
        | Tok_At_Sign;
   end At_Expression;

   -----------------
   -- At_Operator --
   -----------------

   function At_Operator
     return Boolean
   is
   begin
      return Tok in Operator_Token;
   end At_Operator;

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors is
   begin
      Lexer.Clear_Errors;
   end Clear_Errors;

   ----------------------
   -- Current_Operator --
   ----------------------

   function Current_Operator return As.Expressions.Operator_Type is
   begin
      case Operator_Token (Tok) is
         when Tok_Plus_Sign =>
            return As.Expressions.Op_Plus;
         when Tok_Minus_Sign =>
            return As.Expressions.Op_Minus;
         when Tok_Asterisk =>
            return As.Expressions.Op_Multiply;
         when Tok_Slash =>
            return As.Expressions.Op_Divide;
         when Tok_Backslash =>
            return As.Expressions.Op_Mod;
      end case;
   end Current_Operator;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors return Boolean is
   begin
      return Lexer.Has_Errors;
   end Has_Errors;

   ----------
   -- Load --
   ----------

   function Load
     (Path : String)
      return As.Source.Reference
   is
   begin
      Open (Path);
      declare
         Source : constant As.Source.Variable_Reference :=
                    As.Source.Create (As.Files.File (Path));
      begin
         while Tok /= Tok_End_Of_File loop
            declare
               Line : constant As.Source.Source_Line := Parse_Line;
            begin
               Source.Append (Line);
            end;
         end loop;
         Close;
         return As.Source.Reference (Source);
      end;
   end Load;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     return As.Expressions.Reference
   is
   begin
      return Parse_Operator_Expression (Priority_Range'First);
   end Parse_Expression;

   ----------------
   -- Parse_Line --
   ----------------

   function Parse_Line
     return As.Source.Source_Line
   is
      Context : constant As.Files.File_Context := Tok_Context;
      Line    : As.Source.Source_Line := As.Source.Create (Context);
   begin

      if (Tok = Tok_Identifier or else Tok = Tok_At_Sign)
        and then Next_Tok = Tok_Equal_Sign
      then
         As.Source.Set_Label (Line, Tok_Text, False);
         As.Source.Add_Command (Line, "equ");

         Scan;
         Scan;

         declare
            Value : constant As.Expressions.Reference :=
                      Parse_Expression;
         begin
            As.Source.Add_Argument (Line, Value);
         end;
      else
         if (Tok = Tok_Identifier or else Tok = Tok_Integer_Constant)
           and then (Tok_Column = 1 or else Next_Tok = Tok_Colon)
         then
            As.Source.Set_Label (Line, Tok_Text,
                                 Local => Tok = Tok_Integer_Constant);

            Scan;
            if Tok = Tok_Colon then
               Scan;
            end if;
         end if;

         if Tok = Tok_Identifier then
            As.Source.Add_Command (Line, Tok_Text);
            Scan;

            while At_Expression loop
               if Tok = Tok_String then
                  for Ch of Tok_Text loop
                     As.Source.Add_Argument
                       (Line, Expressions.Word_Value (Character'Pos (Ch)));
                  end loop;
                  Scan;
               else
                  As.Source.Add_Argument (Line, Parse_Expression);
               end if;

               if Tok = Tok_Comma then
                  Scan;
                  if not At_Expression then
                     Error ("missing expression");
                  end if;
               elsif At_Expression then
                  Error ("missing ','");
               end if;
            end loop;

         end if;
      end if;

      if Tok = Tok_Comment then
         As.Source.Add_Comment (Line, Tok_Text);
         Scan;
      end if;

      Expect_End_Of_Line;

      return Line;

   end Parse_Line;

   -------------------------------
   -- Parse_Operator_Expression --
   -------------------------------

   function Parse_Operator_Expression
     (Priority : Priority_Range)
     return As.Expressions.Reference
   is

      function Inner_Parse return As.Expressions.Reference
      is (if Priority = Priority_Range'Last
          then Parse_Primary_Expression
          else Parse_Operator_Expression (Priority + 1));

      Left : As.Expressions.Reference := Inner_Parse;
   begin
      while At_Operator
        and then Operator_Priority (Current_Operator) = Priority
      loop
         declare
            Op : constant As.Expressions.Operator_Type := Current_Operator;
         begin
            Scan;
            Left := As.Expressions.Operate (Op, Left, Inner_Parse);
         end;
      end loop;
      return Left;
   end Parse_Operator_Expression;

   ------------------------------
   -- Parse_Primary_Expression --
   ------------------------------

   function Parse_Primary_Expression
     return As.Expressions.Reference
   is
   begin
      if Tok = Tok_Integer_Constant then
         declare
            Value : constant Word_32 := Word_32'Value (Tok_Text);
         begin
            Scan;
            return As.Expressions.Word_Value (Value);
         end;
      elsif Tok = Tok_Percent_Sign then
         Scan;
         if Tok = Tok_Integer_Constant then
            declare
               R : constant Natural := Natural'Value (Tok_Text);
            begin
               if R > 255 then
                  Error ("register must be in range 0 .. 255");
               end if;
               Scan;
               return As.Expressions.Register
                 (Register_Index (Natural'Min (R, 255)));
            end;
         else
            Error ("expected a register index");
            return As.Expressions.Register (0);
         end if;
      elsif Tok = Tok_Local_Reference then
         declare
            Text : constant String := Tok_Text;
            Index : constant Positive :=
                      Positive'Value (Text (Text'First .. Text'Last - 1));
            Forward : constant Boolean :=
                        Text (Text'Last) in 'f' | 'F';
         begin
            Scan;
            return As.Expressions.Local (Index, Forward);
         end;
      elsif Tok = Tok_Identifier then
         declare
            Name : constant String := Tok_Text;
         begin
            Scan;
            return As.Expressions.Identifier (Name);
         end;
      elsif Tok = Tok_At_Sign then
         Scan;
         return As.Expressions.Identifier ("@");
      elsif Tok = Tok_Left_Parenthesis then
         Scan;
         return Expr : constant As.Expressions.Reference := Parse_Expression do
            if Tok = Tok_Right_Parenthesis then
               Scan;
            else
               Error ("expected ')'");
            end if;
         end return;
      else
         Error ("expected an expression");
         return As.Expressions.Word_Value (1);
      end if;
   end Parse_Primary_Expression;

end As.Parser;
