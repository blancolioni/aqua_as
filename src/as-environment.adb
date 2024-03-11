with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with As.Instructions;

package body As.Environment is

   --------------------------
   -- Contains_Local_Label --
   --------------------------

   function Contains_Local_Label
     (This    : Instance'Class;
      Index   : Positive;
      Forward : Boolean)
      return Boolean
   is
   begin
      if Index <= This.Locals.Last_Index then
         declare
            Set : Local_Label_Sets.Set renames This.Locals (Index);
            Loc : constant Word_32 :=
                    This.Location (As.Segments.Code_Segment);
            Position : constant Local_Label_Sets.Cursor :=
                         (if Forward
                          then Set.Ceiling (Loc + 4)
                          else Set.Floor (Loc));
         begin
            return Local_Label_Sets.Has_Element (Position);
         end;
      else
         return False;
      end if;
   end Contains_Local_Label;

   ------------
   -- Create --
   ------------

   function Create return Reference is

      Env : constant Reference := new Instance;

      procedure Instr
        (Name : String;
         Value : As.Instructions.Reference);

      procedure Branch
        (Name    : String;
         Base_Op : Word_8);

      procedure YZ_Imm
        (Name    : String;
         Base_Op : Word_8);

      procedure Z_Imm
        (Name  : String;
         Base_Op : Word_8;
         Y_Imm   : Boolean := False);

      ------------
      -- Branch --
      ------------

      procedure Branch
        (Name    : String;
         Base_Op : Word_8)
      is
      begin
         Instr (Name, As.Instructions.I_Branch (Base_Op));
      end Branch;

      -----------
      -- Instr --
      -----------

      procedure Instr
        (Name : String;
         Value : As.Instructions.Reference)
      is
      begin
         Env.Insert (Name, As.Expressions.Instruction (Value));
      end Instr;

      ------------
      -- YZ_Imm --
      ------------

      procedure YZ_Imm
        (Name    : String;
         Base_Op : Word_8)
      is
      begin
         Instr (Name, As.Instructions.I_YZ_Imm (Base_Op));
      end YZ_Imm;

      -----------
      -- Z_Imm --
      -----------

      procedure Z_Imm
        (Name    : String;
         Base_Op : Word_8;
         Y_Imm   : Boolean := False)
      is
      begin
         Instr (Name, As.Instructions.I_Z_Imm (Base_Op, Y_Imm));
      end Z_Imm;

   begin
      Z_Imm ("mul", 16#18#);
      Z_Imm ("div", 16#1C#);
      Z_Imm ("add", 16#20#);
      Z_Imm ("sub", 16#24#);
      Z_Imm ("cmp", 16#30#);
      Z_Imm ("sl", 16#38#);
      Z_Imm ("sr", 16#3C#);
      Z_Imm ("neg", 16#34#, True);

      Z_Imm ("ldb", 16#80#);
      Z_Imm ("ld", 16#88#);
      Z_Imm ("stb", 16#A0#);
      Z_Imm ("st", 16#A8#);

      Z_Imm ("csn", 16#60#);
      Z_Imm ("csz", 16#62#);
      Z_Imm ("csp", 16#64#);
      Z_Imm ("csod", 16#66#);
      Z_Imm ("csnn", 16#68#);
      Z_Imm ("csnz", 16#6A#);
      Z_Imm ("csnp", 16#6C#);
      Z_Imm ("csev", 16#6E#);

      Z_Imm ("zsn", 16#70#);
      Z_Imm ("zsz", 16#72#);
      Z_Imm ("zsp", 16#74#);
      Z_Imm ("zsod", 16#76#);
      Z_Imm ("zsnn", 16#78#);
      Z_Imm ("zsnz", 16#7A#);
      Z_Imm ("zsnp", 16#7C#);
      Z_Imm ("zsev", 16#7E#);

      Z_Imm ("go", 16#9E#);
      Z_Imm ("pushgo", 16#BE#);

      Z_Imm ("or", 16#C0#);
      Z_Imm ("nor", 16#C4#);
      Z_Imm ("xor", 16#C6#);
      Z_Imm ("and", 16#C8#);
      Z_Imm ("nand", 16#CC#);

      YZ_Imm ("seth", 16#E0#);
      YZ_Imm ("setl", 16#E1#);

      YZ_Imm ("inch", 16#E4#);
      YZ_Imm ("incl", 16#E5#);

      Branch ("bn", 16#40#);
      Branch ("bz", 16#42#);
      Branch ("bp", 16#44#);
      Branch ("bod", 16#46#);
      Branch ("bnn", 16#48#);
      Branch ("bnz", 16#4A#);
      Branch ("bnp", 16#4C#);
      Branch ("bev", 16#4E#);

      Branch ("pushj", 16#F2#);
      Branch ("geta", 16#F4#);

      Instr ("get", As.Instructions.I_Get);

      Instr ("jmp", As.Instructions.I_Jmp);

      Instr ("pop", As.Instructions.I_Pop);

      Instr ("put", As.Instructions.I_Put);

      Instr ("set", As.Instructions.I_Set);

      Instr ("resume", As.Instructions.I_Resume);

      Instr ("trap", As.Instructions.I_Trap);

      Instr ("byte", As.Instructions.Data (1));
      Instr ("word", As.Instructions.Data (4));

      Instr ("code", As.Instructions.Segment (".code"));
      Instr ("data", As.Instructions.Segment (".data"));
      Instr ("text", As.Instructions.Segment (".text"));
      Instr ("note", As.Instructions.Note);

      Instr ("export", As.Instructions.Export);
      Instr ("extern", As.Instructions.Extern);

      Env.Insert ("@", As.Expressions.Current_Location);

      Env.Add_Segment (As.Segments.Code_Segment);
      Env.Add_Segment (As.Segments.Text_Segment);
      Env.Add_Segment (As.Segments.Data_Segment);
      Env.Add_Segment (As.Segments.Heap_Segment);
      Env.Add_Segment (As.Segments.Info_Segment);
      Env.Add_Segment (As.Segments.Note_Segment);

      Env.Set_Current (".code");

      return Env;
   end Create;

   ------------
   -- Export --
   ------------

   procedure Export
     (This : in out Instance'Class;
      Name : String)
   is
   begin
      if This.Entry_Map.Contains (Name) then
         This.Entry_Map (Name).Exported := True;
      else
         This.Entry_Map.Insert
           (Name, This.Export_Entry);
      end if;
   end Export;

   procedure Extern
     (This : in out Instance'Class;
      Name : String)
   is
   begin
      if This.Entry_Map.Contains (Name) then
         This.Entry_Map (Name).External := True;
      else
         This.Entry_Map.Insert
           (Name, This.External_Entry);
      end if;
   end Extern;

   ----------------------
   -- Find_Local_Label --
   ----------------------

   function Find_Local_Label
     (This    : Instance'Class;
      Index   : Positive;
      Forward : Boolean)
      return Word_32
   is
      pragma Assert (Index <= This.Locals.Last_Index);
      Set      : Local_Label_Sets.Set renames This.Locals (Index);
      Loc      : constant Word_32 :=
                   This.Location (As.Segments.Code_Segment);
      Position : constant Local_Label_Sets.Cursor :=
                   (if Forward
                    then Set.Ceiling (Loc + 4)
                    else Set.Floor (Loc));
      pragma Assert (Local_Label_Sets.Has_Element (Position));
   begin
      return Local_Label_Sets.Element (Position);
   end Find_Local_Label;

   ----------------
   -- Get_Global --
   ----------------

   procedure Get_Global
     (This    : Instance'Class;
      Context : As.Files.File_Context;
      G       : out Register_Index;
      W       : out Word_32)
   is
      use type As.Files.Reference;
   begin
      for Index in This.Last_Global .. Register_Index'Last loop
         if This.Globals (Index).Context.File = Context.File
           and then This.Globals (Index).Context.Line = Context.Line
         then
            G := Index;
            W := This.Globals (Index).Value;
            return;
         end if;
      end loop;
      G := 0;
      W := 0;
   end Get_Global;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This  : in out Instance'Class;
      Name  : String;
      Value : As.Expressions.Reference)
   is
   begin
      This.Entry_Map.Insert
        (Name, This.Value_Entry (Value));
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This : in out Instance'Class;
      Name  : String;
      Value : Word_32)
   is
   begin
      This.Insert (Name, Expressions.Word_Value (Value));
   end Insert;

   ------------
   -- Insert --
   ------------

   --  procedure Insert
   --    (This : in out Instance'Class;
   --     Name : String)
   --  is
   --  begin
   --     This.Entry_Map.Insert
   --       (Name, Entry_Record'(False, False, False,
   --        This.Current_Segment, null));
   --  end Insert;

   --------------------
   -- Insert_Current --
   --------------------

   procedure Insert_Current
     (This : in out Instance'Class;
      Name : String)
   is
   begin
      This.Entry_Map.Insert
        (Name, This.Current_Entry);
   end Insert_Current;

   -------------------
   -- Insert_Global --
   -------------------

   procedure Insert_Global
     (This    : in out Instance'Class;
      Context : As.Files.File_Context;
      Name    : String;
      Value   : Word_32)
   is
      Index : constant Register_Index := This.Last_Global - 1;
   begin
      if Name /= "" then
         This.Insert (Name, As.Expressions.Register (Index));
      end if;
      This.Globals (Index) := (Context, Index, Value);
      This.Last_Global := This.Last_Global - 1;
   end Insert_Global;

   ------------------------
   -- Insert_Local_Label --
   ------------------------

   procedure Insert_Local_Label
     (This  : in out Instance'Class;
      Index : Positive)
   is
   begin
      while This.Locals.Last_Index < Index loop
         This.Locals.Append (Local_Label_Sets.Empty);
      end loop;

      This.Locals (Index).Insert (This.Location (As.Segments.Code_Segment));
   end Insert_Local_Label;

   -----------------
   -- Is_Exported --
   -----------------

   function Is_Exported
     (This : Instance'Class;
      Name : String)
      return Boolean
   is
   begin
      return This.Entry_Map.Contains (Name)
        and then This.Entry_Map (Name).Exported;
   end Is_Exported;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This : Instance'Class;
      Process : not null access
        procedure (Name : String))
   is
      package Name_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
      Names : Name_Lists.List;
      package Name_Sorting is new Name_Lists.Generic_Sorting ("<");
   begin
      for Position in This.Entry_Map.Iterate loop
         Names.Append (Entry_Maps.Key (Position));
      end loop;
      Name_Sorting.Sort (Names);
      for Name of Names loop
         Process (Name);
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This    : not null access constant Instance'Class;
      Process : not null access
        procedure (Name : String;
                   Segment : As.Segments.Reference;
                   Defined : Boolean;
                   Exported : Boolean;
                   Value : Word_32))
   is
   begin
      for Position in This.Entry_Map.Iterate loop
         declare
            use type As.Segments.Reference;
            use type As.Expressions.Reference;
            Rec : Entry_Record renames This.Entry_Map (Position);
            Value : constant Word_32 :=
                      (if Rec.Value = null
                       or else not Rec.Value.Has_Word_Value (This)
                       then 0
                       else Rec.Value.Get_Word_Value (This));
         begin
            if not Rec.Const
              and then Rec.Segment /= null
            then
               Process (Name => Entry_Maps.Key (Position),
                        Segment => Rec.Segment,
                        Defined => Rec.Defined,
                        Exported => Rec.Exported,
                        Value    => Value);
            end if;
         end;
      end loop;
   end Iterate;

   ----------------------
   -- Iterate_Mentions --
   ----------------------

   procedure Iterate_Mentions
     (This    : not null access constant Instance'Class;
      Process : not null access
        procedure (Name : String;
                   Segment : As.Segments.Reference;
                   Offset  : Word_32;
                   Context : Mention_Context))
   is
   begin
      for Position in This.Entry_Map.Iterate loop
         declare
            Name : constant String := Entry_Maps.Key (Position);
            Rec : Entry_Record renames This.Entry_Map (Position);
         begin
            if True or else Rec.External then
               for Mention of Rec.Mentions loop
                  Process (Name, Mention.Segment,
                           Mention.Offset, Mention.Context);
               end loop;
            end if;
         end;
      end loop;
   end Iterate_Mentions;

   -------------
   -- Mention --
   -------------

   procedure Mention
     (This    : in out Instance'Class;
      Name    : String;
      Context : Mention_Context;
      Offset  : Word_32 := 0)
   is
   begin
      if not This.Entry_Map.Contains (Name) then
         This.Entry_Map.Insert (Name, This.Undefined_Entry);
      end if;

      declare
         Rec : Entry_Record renames This.Entry_Map (Name);
      begin
         Rec.Mentions.Append
           (Mention_Record'
              (Segment => This.Current_Segment,
               Offset  => As.Segments.Location (This.Location) + Offset,
               Context => Context));
      end;
   end Mention;

   -------------------
   -- Needs_Mention --
   -------------------

   function Needs_Mention
     (This   : not null access constant Instance'Class;
      Name   : String)
      return Boolean
   is
   begin
      if not This.Entry_Map.Contains (Name) then
         return True;
      end if;

      declare
         Rec : Entry_Record renames This.Entry_Map (Name);
      begin
         return not Rec.Const;
      end;
   end Needs_Mention;

   ------------
   -- Update --
   ------------

   procedure Update
     (This  : in out Instance'Class;
      Name  : String;
      Value : As.Expressions.Reference)
   is
      Rec : Entry_Record renames This.Entry_Map (Name);
   begin
      Rec.Defined := True;
      Rec.Value := Value;
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (This  : in out Instance'Class;
      Name  : String;
      Value : As.Segments.Segment_Location)
   is
      Rec : Entry_Record renames This.Entry_Map (Name);
   begin
      Rec.Defined := True;
      Rec.Value := Expressions.Word_Value (As.Segments.Location (Value));
      Rec.Segment := As.Segments.Segment (Value);
   end Update;

end As.Environment;
