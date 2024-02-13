private with Ada.Containers.Doubly_Linked_Lists;
private with As.Environment;
private with As.Objects;
private with As.Source;

package As.Assembler is

   type Instance is tagged private;
   type Reference is access all Instance'Class;

   function New_Assembler
     (Is_Main_Program : Boolean)
     return Reference;

   procedure Load (This : in out Instance'Class;
                   Path : String);

   procedure Assemble (This : in out Instance'Class);

   procedure List (This : in out Instance'Class;
                   Path : String);

   procedure Write (This : Instance'Class;
                    Path : String);

   function Has_Errors
     (This : Instance'Class)
      return Boolean;

private

   package Source_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (As.Source.Reference, As.Source."=");

   type Instance is tagged
      record
         Env     : As.Environment.Reference;
         Sources : Source_Lists.List;
         Obj     : As.Objects.Reference;
         Errors  : Boolean := False;
      end record;

end As.Assembler;
