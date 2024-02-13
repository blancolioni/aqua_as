private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

private with As.Names;

with As.Expressions;
with As.Files;

private package As.Source is

   type Instance is tagged private;
   type Reference is access constant Instance'Class;
   type Variable_Reference is access all Instance'Class;

   function Create
     (File : As.Files.Reference)
      return Variable_Reference;

   type Source_Line is private;

   function Context (Line : Source_Line) return As.Files.File_Context;

   function Has_Command (Line : Source_Line) return Boolean;
   function Get_Command (Line : Source_Line) return String
     with Pre => Has_Command (Line);

   function Has_Comment (Line : Source_Line) return Boolean;
   function Get_Comment (Line : Source_Line) return String
     with Pre => Has_Comment (Line);

   function Get_Argument_Count
     (Line : Source_Line)
      return Natural;

   function Get_Argument
     (Line : Source_Line;
      Index : Positive)
      return As.Expressions.Reference
     with Pre => Index <= Get_Argument_Count (Line);

   function Get_Label
     (Line    : Source_Line)
      return String;

   function Has_Local_Label
     (Line    : Source_Line)
      return Boolean;

   procedure Iterate_Arguments
     (Line : Source_Line;
      Process : not null access
        procedure (Argument : As.Expressions.Reference));

   function Create (Context : As.Files.File_Context) return Source_Line;

   procedure Set_Label
     (Line  : in out Source_Line;
      Label : String;
      Local : Boolean);

   procedure Add_Command
     (Line    : in out Source_Line;
      Command : String);

   procedure Add_Argument
     (Line     : in out Source_Line;
      Argument : As.Expressions.Reference);

   procedure Add_Comment
     (Line    : in out Source_Line;
      Comment : String);

   procedure Append
     (This : in out Instance'Class;
      Line : Source_Line);

   procedure Iterate
     (This : Instance'Class;
      Process : not null access
        procedure (Line : Source_Line));

private

   type Label_Record is
      record
         Name      : As.Names.Symbol_Name;
         Temporary : Boolean;
      end record;

   package Label_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Label_Record);

   package Argument_Vectors is
     new Ada.Containers.Vectors
       (Positive, As.Expressions.Reference, As.Expressions."=");

   type Source_Line is
      record
         Context   : As.Files.File_Context;
         Label     : As.Names.Symbol_Name;
         Temporary : Boolean := False;
         Command   : As.Names.Symbol_Name;
         Arguments : Argument_Vectors.Vector;
         Comment   : As.Names.Symbol_Name;
      end record;

   function Context (Line : Source_Line) return As.Files.File_Context
   is (Line.Context);

   function Get_Argument_Count
     (Line : Source_Line)
      return Natural
   is (Line.Arguments.Last_Index);

   function Get_Argument
     (Line : Source_Line;
      Index : Positive)
      return As.Expressions.Reference
   is (Line.Arguments.Element (Index));

   function Has_Command (Line : Source_Line) return Boolean
   is (As.Names."-" (Line.Command) /= "");

   function Get_Command (Line : Source_Line) return String
   is (As.Names."-" (Line.Command));

   function Has_Comment (Line : Source_Line) return Boolean
   is (As.Names."-" (Line.Comment) /= "");

   function Get_Comment (Line : Source_Line) return String
   is (As.Names."-" (Line.Comment));

   function Get_Label
     (Line    : Source_Line)
      return String
   is (As.Names."-" (Line.Label));

   package Source_Line_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Source_Line);

   type Instance is tagged
      record
         File  : As.Files.Reference;
         Lines : Source_Line_Lists.List;
      end record;

end As.Source;
