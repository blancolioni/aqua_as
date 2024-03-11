private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;
private with As.Names;

package As.Segments is

   type Instance is tagged private;
   type Reference is access constant Instance'Class;

   function Create
     (Name       : String;
      R, W, X    : Boolean := False;
      Allocate   : Boolean := True;
      Initialize : Boolean := True)
      return Reference;

   function Name (This : Instance'Class) return String;

   function Read (This : Instance'Class) return Boolean;
   function Write (This : Instance'Class) return Boolean;
   function Execute (This : Instance'Class) return Boolean;
   function Allocate (This : Instance'Class) return Boolean;
   function Initialize (This : Instance'Class) return Boolean;

   function Code_Segment return Reference;
   function Data_Segment return Reference;
   function Heap_Segment return Reference;
   function Info_Segment return Reference;
   function Note_Segment return Reference;
   function Text_Segment return Reference;

   type Segment_Location is private;

   function Segment (This : Segment_Location) return Reference;
   function Location (This : Segment_Location) return Word_32;

   type Segment_State is tagged private;

   procedure Add_Segment
     (This    : in out Segment_State'Class;
      Segment : not null access constant Instance'Class);

   function Current_Segment
     (This : Segment_State'Class)
      return As.Segments.Reference;

   procedure Set_Current
     (This         : in out Segment_State;
      Segment_Name : String);

   function Location
     (This : Segment_State'Class)
      return Segment_Location;

   function Location
     (This    : Segment_State'Class;
      Segment : Reference)
      return Word_32;

   procedure Set_Location
     (This : in out Segment_State'Class;
      Loc  : Word_32);

   procedure Set_Location
     (This : in out Segment_State'Class;
      Loc  : Segment_Location);

   procedure Align
     (This      : in out Segment_State;
      Alignment : Word_32);

   procedure Reset (This : in out Segment_State'Class);
   procedure Initialize (This : in out Segment_State;
                         From : not null access constant Segment_State'Class);

   procedure Add_Note
     (This        : in out Segment_State'Class;
      Name        : String;
      Tag         : Word_32;
      Description : String);

   procedure Iterate_Segments
     (This : Segment_State'Class;
      Process : not null access
        procedure (Segment : Reference));

   procedure Iterate_Notes
     (This    : Segment_State'Class;
      Process : not null access
        procedure (Name   : String;
                   Tag    : Word_32;
                   Description : String));

private

   type Note_Record is
      record
         Name : As.Names.Symbol_Name;
         Tag  : Word_32;
         Desc : As.Names.Symbol_Name;
      end record;

   package Note_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Note_Record);

   subtype Dispatch is Instance'Class;

   type Instance is tagged
      record
         Name       : As.Names.Symbol_Name;
         Read       : Boolean;
         Write      : Boolean;
         Execute    : Boolean;
         Allocate   : Boolean;
         Initialize : Boolean;
      end record;

   function Name (This : Instance'Class) return String
   is (As.Names."-" (This.Name));

   function Read (This : Instance'Class) return Boolean
   is (This.Read);

   function Write (This : Instance'Class) return Boolean
   is (This.Write);

   function Execute (This : Instance'Class) return Boolean
   is (This.Execute);

   function Allocate (This : Instance'Class) return Boolean
   is (This.Allocate);

   function Initialize (This : Instance'Class) return Boolean
   is (This.Initialize);

   type Segment_Location is
      record
         Segment  : Reference;
         Location : Word_32;
      end record;

   function Segment (This : Segment_Location) return Reference
   is (This.Segment);

   function Location (This : Segment_Location) return Word_32
   is (This.Location);

   package Segment_Maps is
     new WL.String_Maps (Segment_Location);

   package Segment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   type Segment_State is tagged
      record
         Map     : Segment_Maps.Map;
         List    : Segment_Lists.List;
         Current : Segment_Maps.Cursor;
         Notes   : Note_Lists.List;
      end record;

   function Location
     (This : Segment_State'Class)
      return Segment_Location
   is (Segment_Maps.Element (This.Current));

   function Location
     (This    : Segment_State'Class;
      Segment : Reference)
      return Word_32
   is (This.Map (As.Names."-" (Segment.Name)).Location);

   function Current_Segment
     (This : Segment_State'Class)
      return As.Segments.Reference
   is (if Segment_Maps.Has_Element (This.Current)
       then Segment_Maps.Element (This.Current).Segment
       else null);

end As.Segments;
