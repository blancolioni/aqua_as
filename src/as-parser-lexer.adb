with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with As.Names;

package body As.Parser.Lexer is

   type Token_Record is
      record
         Tok    : Token := Tok_End_Of_Line;
         Text   : As.Names.Symbol_Name;
         Line   : Positive;
         Column : Positive;
      end record;

   package Token_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Token_Record);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   File_Lines  : String_Vectors.Vector;

   Token_List  : Token_Lists.List;
   Current     : Token_Lists.Cursor;
   Source_File : As.Files.Reference;
   Got_Error   : Boolean := False;
   Got_Warning : Boolean := False;

   procedure Load (Path : String);

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors is
   begin
      Got_Error := False;
      Got_Warning := False;
   end Clear_Errors;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Token_List.Clear;
      Current := Token_Lists.No_Element;
   end Close;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         As.Files.To_String (Tok_Context)
         & ": " & Message);
      Got_Error := True;
   end Error;

   ------------
   -- Expect --
   ------------

   procedure Expect (Expected_Tok : Token) is
   begin
      if Tok = Expected_Tok then
         Scan;
      else
         Error ("expected " & Token'Image (Expected_Tok));
      end if;
   end Expect;

   ------------------------
   -- Expect_End_Of_Line --
   ------------------------

   procedure Expect_End_Of_Line is
   begin
      if Tok = Tok_End_Of_Line then
         Scan;
      else
         Error ("expected end of line");
         Skip_Rest_Of_Line;
      end if;
   end Expect_End_Of_Line;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors return Boolean is
   begin
      return Got_Error;
   end Has_Errors;

   ------------------
   -- Has_Warnings --
   ------------------

   function Has_Warnings return Boolean is
   begin
      return Got_Warning;
   end Has_Warnings;

   -----------------
   -- Information --
   -----------------

   procedure Information (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         As.Files.To_String (Tok_Context)
         & ": information: " & Message);
   end Information;

   ----------
   -- Load --
   ----------

   procedure Load (Path : String) is
      use Ada.Text_IO;
      File        : File_Type;
      Line_Number : Positive := 1;

   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line             : constant String :=
                                 Get_Line (File) & ' ';
            Scanning         : Boolean := False;
            Scanning_Tok     : Token := Tok_None;
            Scanning_Start   : Natural := 0;
            Scanning_Comment : Boolean := False;
            Scanning_String  : Boolean := False;

            procedure Append
              (Tok    : Token;
               Start  : Positive;
               Finish : Natural);

            ------------
            -- Append --
            ------------

            procedure Append
              (Tok    : Token;
               Start  : Positive;
               Finish : Natural)
            is
            begin
               Token_List.Append
                 (Token_Record'
                    (Tok    => Tok,
                     Text   => As.Names."+" (Line (Start .. Finish)),
                     Line   => Line_Number,
                     Column => Start));
            end Append;

         begin

            File_Lines.Append (Line);

            for I in Line'Range loop
               declare
                  use Ada.Characters.Latin_1;
                  Ch : constant Character := Line (I);
                  Next_Ch : constant Character :=
                              (if I < Line'Last then Line (I + 1) else ' ');

                  procedure Finish (Tok : Token);

                  procedure Next (Tok : Token);

                  ------------
                  -- Finish --
                  ------------

                  procedure Finish (Tok : Token) is
                  begin
                     if Scanning then
                        Append (Scanning_Tok, Scanning_Start, I - 1);
                     end if;

                     Append (Tok, I, I);
                     Scanning := False;
                  end Finish;

                  ----------
                  -- Next --
                  ----------

                  procedure Next (Tok : Token) is
                  begin
                     if Scanning then
                        if Tok /= Scanning_Tok then
                           Append (Scanning_Tok, Scanning_Start, I - 1);
                           Scanning_Tok := Tok;
                           Scanning_Start := I;
                        end if;
                     else
                        Scanning := True;
                        Scanning_Start := I;
                        Scanning_Tok := Tok;
                     end if;
                  end Next;

               begin
                  if Scanning_Comment then
                     if I = Line'Last then
                        Append (Tok_Comment, Scanning_Start, I - 1);
                        Scanning := False;
                        Scanning_Comment := False;
                     end if;
                  elsif Scanning_String then
                     if Ch = '"' or else I = Line'Last then
                        if Ch /= '"' then
                           Error ("unterminated string literal");
                        end if;
                        Append (Tok_String, Scanning_Start, I - 1);
                        Scanning_String := False;
                        Scanning := False;
                     end if;
                  else

                     case Ch is
                        when ' ' | HT | CR =>
                           if Scanning then
                              Append (Scanning_Tok, Scanning_Start, I - 1);
                              Scanning := False;
                           end if;
                        when '0' .. '9' =>
                           if Scanning
                             and then Scanning_Tok = Tok_Identifier
                           then
                              null;
                           else
                              Next (Tok_Integer_Constant);
                           end if;
                        when 'A' .. 'Z' | 'a' .. 'z' | '_' | '.' | '$' =>
                           if Scanning
                             and then Scanning_Tok = Tok_Integer_Constant
                             and then Ch in 'b' | 'B' | 'f' | 'F'
                             and then Next_Ch not in 'A' .. 'Z'
                               | 'a' .. 'z' | '_' | '.' | '$'
                           then
                              Append (Tok_Local_Reference,
                                      Scanning_Start, I);
                              Scanning := False;
                           else
                              Next (Tok_Identifier);
                           end if;

                        when ';' =>
                           Next (Tok_Comment);
                           Scanning_Comment := True;
                           Scanning_Start := I + 1;

                        when '"' =>
                           Next (Tok_String);
                           Scanning_String := True;
                           Scanning_Start := I + 1;

                        when ':' =>
                           Finish (Tok_Colon);

                        when '=' =>
                           Finish (Tok_Equal_Sign);

                        when '%' =>
                           Finish (Tok_Percent_Sign);

                        when '#' =>
                           Finish (Tok_Number_Sign);

                        when '@' =>
                           Finish (Tok_At_Sign);

                        when '(' =>
                           Finish (Tok_Left_Parenthesis);

                        when ')' =>
                           Finish (Tok_Right_Parenthesis);

                        when ',' =>
                           Finish (Tok_Comma);

                        when '<' =>
                           Finish (Tok_Left_Angle_Bracket);

                        when '>' =>
                           Finish (Tok_Right_Angle_Bracket);

                        when '+' =>
                           Finish (Tok_Plus_Sign);

                        when '-' =>
                           Finish (Tok_Minus_Sign);

                        when '*' =>
                           Finish (Tok_Asterisk);

                        when '/' =>
                           Finish (Tok_Slash);

                        when '\' =>
                           Finish (Tok_Backslash);

                        when '&' =>
                           Finish (Tok_Ampersand);

                        when '!' =>
                           Finish (Tok_Exclamation_Point);

                        when others =>
                           Finish (Tok_Bad_Character);
                     end case;
                  end if;
               end;
            end loop;

            Append (Tok_End_Of_Line, Line'Last + 1, Line'Last);
            Line_Number := Line_Number + 1;

         end;
      end loop;
   end Load;

   --------------
   -- Next_Tok --
   --------------

   function Next_Tok return Token is
      Next : constant Token_Lists.Cursor :=
               Token_Lists.Next (Current);
   begin
      if Token_Lists.Has_Element (Next) then
         return Token_Lists.Element (Next).Tok;
      else
         return Tok_End_Of_File;
      end if;
   end Next_Tok;

   ----------
   -- Open --
   ----------

   procedure Open (Path : String) is
   begin
      Source_File := As.Files.File (Path);
      Load (Path);
      Token_List.Append
        (Token_Record'
           (Tok    => Tok_End_Of_File,
            Text   => As.Names."+" (""),
            Line   => 1,
            Column => 1));
      Current := Token_List.First;
   end Open;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      Token_Lists.Next (Current);
   end Scan;

   -----------------------
   -- Skip_Rest_Of_Line --
   -----------------------

   procedure Skip_Rest_Of_Line is
   begin
      while Tok /= Tok_End_Of_Line
        and then Tok /= Tok_End_Of_File
      loop
         Scan;
      end loop;

      if Tok = Tok_End_Of_File then
         Scan;
      end if;
   end Skip_Rest_Of_Line;

   -------------
   -- Skip_To --
   -------------

   procedure Skip_To (Search_Tok : Token;
                      Consume    : Boolean := True)
   is
   begin
      while Tok /= Search_Tok
        and then Tok /= Tok_End_Of_Line
        and then Tok /= Tok_End_Of_File
      loop
         Scan;
      end loop;

      if Consume and then (Tok = Search_Tok or else Tok = Tok_End_Of_File) then
         Scan;
      end if;
   end Skip_To;

   ---------
   -- Tok --
   ---------

   function Tok return Token is
   begin
      return Token_Lists.Element (Current).Tok;
   end Tok;

   ----------------
   -- Tok_Column --
   ----------------

   function Tok_Column return Positive is
   begin
      return Token_Lists.Element (Current).Column;
   end Tok_Column;

   -----------------
   -- Tok_Context --
   -----------------

   function Tok_Context return As.Files.File_Context is
   begin
      return (Tok_File, Tok_Line, Tok_Column);
   end Tok_Context;

   --------------
   -- Tok_File --
   --------------

   function Tok_File return As.Files.Reference is
   begin
      return Source_File;
   end Tok_File;

   --------------
   -- Tok_Line --
   --------------

   function Tok_Line return Positive is
   begin
      return Token_Lists.Element (Current).Line;
   end Tok_Line;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text return String is
   begin
      return As.Names."-" (Token_Lists.Element (Current).Text);
   end Tok_Text;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         As.Files.To_String (Tok_Context)
         & ": warning: " & Message);
      Got_Warning := True;
   end Warning;

end As.Parser.Lexer;
