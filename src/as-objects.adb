with WL.Files.ELF;

with As.Environment;

package body As.Objects is

   function Get_Context
     (This : Instance'Class;
      From : As.Files.File_Context)
      return Context_Record
     with Pre => This.Has_Context (From);

   -----------
   -- Align --
   -----------

   overriding procedure Align
     (This      : in out Instance;
      Alignment : Word_32)
   is
   begin
      while Word_32 (This.Segment_List (This.Active).Data.Length)
      mod Alignment
        /= 0
      loop
         This.Append (Word_8'(0));
      end loop;
   end Align;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Instance'Class; Value : Word_8) is
      Seg : Segment_Record renames This.Segment_List (This.Active);
   begin
      Seg.Data.Append (Value);
      Seg.Loc := Seg.Loc + 1;

      This.Set_Location (Seg.Loc);

      if Context_Lists.Has_Element (This.Current) then
         declare
            Context : Context_Record renames This.Contexts (This.Current);
         begin
            Context.Segment := Seg.Segment;
            Context.Last := Seg.Data.Last_Index;
         end;
      end if;

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Instance'Class; Value : Word_16) is
   begin
      This.Append (Word_8 (Value / 256));
      This.Append (Word_8 (Value mod 256));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append (This : in out Instance'Class; Value : Word_32) is
   begin
      This.Append (Word_16 (Value / 65536));
      This.Append (Word_16 (Value mod 65536));
   end Append;

   ------------
   -- Create --
   ------------

   function Create
     (Env : not null access constant As.Environment.Instance'Class)
      return Reference
   is
   begin
      return This : constant Reference := new Instance do
         This.Env := Environment_Reference (Env);
         This.Initialize (Env);
      end return;
   end Create;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Word_32
   is
   begin
      return Get_Context (This, Context).Address;
   end Get_Address;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (This : Instance'Class;
      From : As.Files.File_Context)
      return Context_Record
   is
      use type As.Files.Reference;
   begin
      for Rec of This.Contexts loop
         if Rec.Context.File = From.File
           and then Rec.Context.Line = From.Line
         then
            return Rec;
         end if;
      end loop;
      raise Constraint_Error with
        "no such context: " & As.Files.To_String (From);
   end Get_Context;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Context_Data
   is
      Rec     : constant Context_Record := This.Get_Context (Context);
      First   : constant Positive := Rec.First;
      Last    : constant Natural := Rec.Last;
      Segment : constant As.Segments.Reference := Rec.Segment;
      Seg_Pos : Segment_Lists.Cursor renames This.Segment_Map (Segment.Name);
      Seg     : Segment_Record renames This.Segment_List (Seg_Pos);
   begin
      return Data : Context_Data (1 .. Last - First + 1) do
         for I in First .. Last loop
            Data (I - First + 1) := Seg.Data (I);
         end loop;
      end return;
   end Get_Data;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This : in out Instance;
      From : not null access constant As.Segments.Segment_State'Class)
   is

      procedure Add_Segment (Segment : As.Segments.Reference);

      -----------------
      -- Add_Segment --
      -----------------

      procedure Add_Segment (Segment : As.Segments.Reference) is
         Name : constant String := Segment.Name;
         Rec : constant Segment_Record := Segment_Record'
           (Segment => Segment,
            Start   => 0, Size => 0, Loc => 0, Data => <>);
      begin
         This.Segment_List.Append (Rec);
         This.Segment_Map.Insert (Name, This.Segment_List.Last);
      end Add_Segment;

   begin
      As.Segments.Segment_State (This).Initialize (From);
      From.Iterate_Segments (Add_Segment'Access);
      This.Active := This.Segment_List.First;
   end Initialize;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (This    : in out Instance'Class;
      Context : As.Files.File_Context)
   is
      Active : Segment_Record renames This.Segment_List (This.Active);
   begin
      This.Contexts.Append
        (Context_Record'
           (Context => Context,
            Address => Active.Loc,
            Segment => Active.Segment,
            First   => Active.Data.Last_Index + 1,
            Last    => Active.Data.Last_Index));
      This.Current := This.Contexts.Last;
   end Set_Context;

   -----------------
   -- Set_Current --
   -----------------

   overriding procedure Set_Current
     (This         : in out Instance;
      Segment_Name : String)
   is
   begin
      Parent (This).Set_Current (Segment_Name);
      This.Active := This.Segment_Map (Segment_Name);
   end Set_Current;

   -----------
   -- Write --
   -----------

   procedure Write (This : in out Instance'Class; Path : String) is
      use WL.Files.ELF;
      File : File_Type;
      Note_Data : Word_8_Vectors.Vector;

      procedure Add_Global_Symbol
        (Name     : String;
         Segment  : As.Segments.Reference;
         Defined  : Boolean;
         Exported : Boolean;
         Value    : Word_32);

      procedure Add_Local_Symbol
        (Name     : String;
         Segment  : As.Segments.Reference;
         Defined  : Boolean;
         Exported : Boolean;
         Value    : Word_32);

      procedure Add_Note
        (Name : String;
         Tag  : Word_32;
         Desc : String);

      procedure Add_Symbol_Reference
        (Name    : String;
         Segment : As.Segments.Reference;
         Offset  : Word_32;
         Context : Mention_Context);

      -----------------------
      -- Add_Global_Symbol --
      -----------------------

      procedure Add_Global_Symbol
        (Name     : String;
         Segment  : As.Segments.Reference;
         Defined  : Boolean;
         Exported : Boolean;
         Value    : Word_32)
      is
      begin
         if Exported or else not Defined then
            WL.Files.ELF.New_Symbol
              (File         => File,
               Name         => Name,
               Value        => Address_32 (Value),
               Size         => 4,
               Binding      => Global,
               Typ          => Func,
               Visibility   => Default,
               Section_Name =>
                 (if Defined then Segment.Name else ""));
         end if;
      end Add_Global_Symbol;

      ----------------------
      -- Add_Local_Symbol --
      ----------------------

      procedure Add_Local_Symbol
        (Name     : String;
         Segment  : As.Segments.Reference;
         Defined  : Boolean;
         Exported : Boolean;
         Value    : Word_32)
      is
      begin
         if not Exported
           and then Defined
         then
            WL.Files.ELF.New_Symbol
              (File         => File,
               Name         => Name,
               Value        => Address_32 (Value),
               Size         => 4,
               Binding      => Local,
               Typ          => Func,
               Visibility   => Default,
               Section_Name => Segment.Name);
         end if;
      end Add_Local_Symbol;

      --------------
      -- Add_Note --
      --------------

      procedure Add_Note
        (Name : String;
         Tag  : Word_32;
         Desc : String)
      is
         procedure Put_String (S : String);
         procedure Put_Word_32 (W : Word_32);

         -----------------
         -- Put_Word_32 --
         -----------------

         procedure Put_Word_32 (W : Word_32) is
            Octets : array (1 .. 4) of Word_8;
            It     : Word_32 := W;
         begin
            for I in reverse Octets'Range loop
               Octets (I) := Word_8 (It mod 256);
               It := It / 256;
            end loop;
            for Octet of Octets loop
               Note_Data.Append (Octet);
            end loop;
         end Put_Word_32;

         ----------------
         -- Put_String --
         ----------------

         procedure Put_String (S : String) is
         begin
            for Ch of S loop
               Note_Data.Append (Character'Pos (Ch));
            end loop;
            if S'Length mod 4 /= 0 then
               for I in 1 .. 4 - S'Length mod 4 loop
                  Note_Data.Append (0);
               end loop;
            end if;
         end Put_String;

      begin
         Put_Word_32 (Word_32 (Name'Length));
         Put_Word_32 (Word_32 (Desc'Length));
         Put_Word_32 (Tag);
         Put_String (Name);
         Put_String (Desc);
      end Add_Note;

      --------------------------
      -- Add_Symbol_Reference --
      --------------------------

      procedure Add_Symbol_Reference
        (Name    : String;
         Segment : As.Segments.Reference;
         Offset  : Word_32;
         Context : Mention_Context)
      is
      begin
         WL.Files.ELF.Add_Symbol_Reference
           (File, Name, Segment.Name, Address_32 (Offset),
           Mention_Context'Pos (Context));
      end Add_Symbol_Reference;

   begin

      This.Env.Iterate_Notes (Add_Note'Access);

      for Segment of This.Segment_List loop
         if Segment.Segment.Name = ".note" then
            Segment.Data := Note_Data;
         end if;
      end loop;

      Create (File, Out_File, Path);

      for Segment of This.Segment_List loop
         New_Section_Header
           (File        => File,
            Name        => Segment.Segment.Name,
            Header_Type =>
              (if Segment.Segment.Name = ".note"
               then WL.Files.ELF.Note
               elsif Segment.Segment.Initialize
               then WL.Files.ELF.Progbits
               else WL.Files.ELF.Nobits),
            Write       => Segment.Segment.Write,
            Execinstr   => Segment.Segment.Execute,
            Alloc       => Segment.Segment.Allocate);
         for W of Segment.Data loop
            Put (File, Octet (W));
         end loop;
      end loop;

      This.Env.Iterate (Add_Local_Symbol'Access);
      This.Env.Iterate (Add_Global_Symbol'Access);

      This.Env.Iterate_Mentions (Add_Symbol_Reference'Access);

      Close (File);
   end Write;

end As.Objects;
