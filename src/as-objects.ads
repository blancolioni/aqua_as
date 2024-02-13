private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with WL.String_Maps;

limited with As.Environment;
with As.Files;
with As.Segments;

private package As.Objects is

   subtype Parent is As.Segments.Segment_State;

   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Create
     (Env : not null access constant As.Environment.Instance'Class)
     return Reference;

   procedure Set_Context
     (This    : in out Instance'Class;
      Context : As.Files.File_Context);

   function Has_Context
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Boolean;

   function Get_Address
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Word_32;

   type Context_Data is array (Positive range <>) of Word_8;

   function Get_Data
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Context_Data;

   procedure Append
     (This : in out Instance'Class;
      Value : Word_8);

   procedure Append
     (This : in out Instance'Class;
      Value : Word_16);

   procedure Append
     (This : in out Instance'Class;
      Value : Word_32);

   procedure Write
     (This : in out Instance'Class;
      Path : String);

private

   Loader_Value : constant Word_8 := 16#98#;

   package Word_8_Vectors is
     new Ada.Containers.Vectors (Positive, Word_8);

   type Context_Record is
      record
         Context : As.Files.File_Context;
         Segment : As.Segments.Reference;
         Address : Word_32;
         First   : Positive;
         Last    : Natural;
      end record;

   package Context_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Context_Record);

   type Segment_Record is
      record
         Segment : As.Segments.Reference;
         Start   : Word_32;
         Size    : Word_32;
         Loc     : Word_32;
         Data    : Word_8_Vectors.Vector;
      end record;

   package Segment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Segment_Record);

   package Segment_Maps is
     new WL.String_Maps (Segment_Lists.Cursor, Segment_Lists."=");

   type Environment_Reference is access constant As.Environment.Instance'Class;

   type Instance is new Parent with
      record
         Env          : Environment_Reference;
         Segment_List : Segment_Lists.List;
         Segment_Map  : Segment_Maps.Map;
         Contexts     : Context_Lists.List;
         Current      : Context_Lists.Cursor;
         Active       : Segment_Lists.Cursor;
      end record;

   overriding procedure Initialize
     (This : in out Instance;
      From : not null access constant As.Segments.Segment_State'Class);

   overriding procedure Set_Current
     (This         : in out Instance;
      Segment_Name : String);

   overriding procedure Align
     (This      : in out Instance;
      Alignment : Word_32);

   function Has_Context
     (This    : Instance'Class;
      Context : As.Files.File_Context)
      return Boolean
   is (for some Element of This.Contexts =>
          As.Files."=" (Element.Context.File, Context.File)
       and then Element.Context.Line = Context.Line);

end As.Objects;
