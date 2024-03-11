package body As.Segments is

   Local_Code : aliased constant Instance :=
                  Instance'(Name       => As.Names."+" (".code"),
                            Read       => True,
                            Write      => False,
                            Execute    => True,
                            Allocate   => True,
                            Initialize => True);

   Local_Data : aliased constant Instance :=
                  Instance'(Name       => As.Names."+" (".data"),
                            Read       => True,
                            Write      => True,
                            Execute    => False,
                            Allocate   => True,
                            Initialize => True);

   Local_Heap : aliased constant Instance :=
                  Instance'(Name       => As.Names."+" (".bss"),
                            Read       => True,
                            Write      => True,
                            Execute    => False,
                            Allocate   => False,
                            Initialize => False);

   Local_Info : aliased constant Instance :=
                  Instance'(Name       => As.Names."+" (".info"),
                            Read       => True,
                            Write      => False,
                            Execute    => False,
                            Allocate   => False,
                            Initialize => True);

   Local_Note : aliased constant Instance :=
                  Instance'(Name       => As.Names."+" (".note"),
                            Read       => True,
                            Write      => False,
                            Execute    => False,
                            Allocate   => False,
                            Initialize => True);

   Local_Text : aliased constant Instance :=
                  Instance'(Name       => As.Names."+" (".text"),
                            Read       => True,
                            Write      => False,
                            Execute    => False,
                            Allocate   => True,
                            Initialize => True);

   --------------
   -- Add_Note --
   --------------

   procedure Add_Note
     (This        : in out Segment_State'Class;
      Name        : String;
      Tag         : Word_32;
      Description : String)
   is
   begin
      This.Notes.Append
        (Note_Record'
           (Name => As.Names."+" (Name),
            Tag  => Tag,
            Desc => As.Names."+" (Description)));
   end Add_Note;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
     (This    : in out Segment_State'Class;
      Segment : not null access constant Instance'Class)
   is
      Name : constant String := As.Names."-" (Segment.Name);
      Loc  : constant Segment_Location := (Reference (Segment), 0);
   begin
      This.Map.Insert (Name, Loc);
      This.List.Append (Reference (Segment));
   end Add_Segment;

   -----------
   -- Align --
   -----------

   procedure Align
     (This      : in out Segment_State;
      Alignment : Word_32)
   is
      Loc : constant Word_32 := This.Map (This.Current).Location;
   begin
      if Loc mod Alignment /= 0 then
         This.Map (This.Current).Location :=
           Loc + Alignment - Loc mod Alignment;
      end if;
   end Align;

   ------------------
   -- Code_Segment --
   ------------------

   function Code_Segment return Reference is
   begin
      return Local_Code'Access;
   end Code_Segment;

   ------------
   -- Create --
   ------------

   function Create
     (Name       : String;
      R, W, X    : Boolean := False;
      Allocate   : Boolean := True;
      Initialize : Boolean := True)
      return Reference
   is
   begin
      return new Instance'
        (Name       => As.Names."+" (Name),
         Read       => R,
         Write      => W,
         Execute    => X,
         Allocate   => Allocate,
         Initialize => Initialize);
   end Create;

   ------------------
   -- Data_Segment --
   ------------------

   function Data_Segment return Reference is
   begin
      return Local_Data'Access;
   end Data_Segment;

   ------------------
   -- Heap_Segment --
   ------------------

   function Heap_Segment return Reference is
   begin
      return Local_Heap'Access;
   end Heap_Segment;

   ------------------
   -- Info_Segment --
   ------------------

   function Info_Segment return Reference is
   begin
      return Local_Info'Access;
   end Info_Segment;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Segment_State;
                         From : not null access constant Segment_State'Class)
   is
   begin
      This.Map := From.Map;
      This.List := From.List;
      This.Current := Segment_Maps.No_Element;
      This.Reset;
   end Initialize;

   -------------------
   -- Iterate_Notes --
   -------------------

   procedure Iterate_Notes
     (This    : Segment_State'Class;
      Process : not null access
        procedure (Name   : String;
                   Tag    : Word_32;
                   Description : String))
   is
   begin
      for Note of This.Notes loop
         Process (As.Names."-" (Note.Name), Note.Tag,
                  As.Names."-" (Note.Desc));
      end loop;
   end Iterate_Notes;

   ----------------------
   -- Iterate_Segments --
   ----------------------

   procedure Iterate_Segments
     (This    : Segment_State'Class;
      Process : not null access
        procedure (Segment : Reference))
   is
   begin
      for Element of This.List loop
         Process (Element);
      end loop;
   end Iterate_Segments;

   ------------------
   -- Note_Segment --
   ------------------

   function Note_Segment return Reference is
   begin
      return Local_Note'Access;
   end Note_Segment;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Segment_State'Class) is
   begin
      for Loc of This.Map loop
         Loc.Location := 0;
      end loop;
   end Reset;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current
     (This         : in out Segment_State;
      Segment_Name : String)
   is
   begin
      This.Current := This.Map.Find (Segment_Name);
   end Set_Current;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (This : in out Segment_State'Class;
      Loc  : Word_32)
   is
   begin
      This.Map (This.Current).Location := Loc;
   end Set_Location;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (This : in out Segment_State'Class;
      Loc  : Segment_Location)
   is
   begin
      This.Map (Name (Loc.Segment.all)).Location := Loc.Location;
   end Set_Location;

   ------------------
   -- Text_Segment --
   ------------------

   function Text_Segment return Reference is
   begin
      return Local_Text'Access;
   end Text_Segment;

end As.Segments;
