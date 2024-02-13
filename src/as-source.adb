package body As.Source is

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Line : in out Source_Line; Argument : As.Expressions.Reference)
   is
   begin
      Line.Arguments.Append (Argument);
   end Add_Argument;

   -----------------
   -- Add_Command --
   -----------------

   procedure Add_Command (Line : in out Source_Line; Command : String) is
   begin
      Line.Command := As.Names."+" (Command);
   end Add_Command;

   -----------------
   -- Add_Comment --
   -----------------

   procedure Add_Comment (Line : in out Source_Line; Comment : String) is
   begin
      Line.Comment := As.Names."+" (Comment);
   end Add_Comment;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Instance'Class; Line : Source_Line) is
   begin
      This.Lines.Append (Line);
   end Append;

   ------------
   -- Create --
   ------------

   function Create (File : As.Files.Reference) return Variable_Reference is
   begin
      return new Instance'
        (File  => File,
         Lines => <>);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Context : As.Files.File_Context) return Source_Line is
   begin
      return Source_Line'
        (Context => Context, others => <>);
   end Create;

   ---------------------
   -- Has_Local_Label --
   ---------------------

   function Has_Local_Label
     (Line    : Source_Line)
      return Boolean
   is
   begin
      return Line.Temporary;
   end Has_Local_Label;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This : Instance'Class;
      Process : not null access
        procedure (Line : Source_Line))
   is
   begin
      for Line of This.Lines loop
         Process (Line);
      end loop;
   end Iterate;

   -----------------------
   -- Iterate_Arguments --
   -----------------------

   procedure Iterate_Arguments
     (Line : Source_Line;
      Process : not null access
        procedure (Argument : As.Expressions.Reference))
   is
   begin
      for Arg of Line.Arguments loop
         Process (Arg);
      end loop;
   end Iterate_Arguments;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Line  : in out Source_Line;
      Label : String;
      Local : Boolean)
   is
   begin
      Line.Label := As.Names."+" (Label);
      Line.Temporary := Local;
   end Set_Label;

end As.Source;
