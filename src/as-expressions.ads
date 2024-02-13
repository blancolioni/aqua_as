private with As.Names;

limited with As.Environment;

with As.Instructions;

private package As.Expressions is

   type Operator_Type is (Op_Plus, Op_Minus, Op_Multiply, Op_Divide, Op_Mod);

   type Instance (<>) is tagged private;
   type Reference is access constant Instance'Class;

   function Has_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean;

   function Has_Static_Value
     (This : Instance)
      return Boolean;

   function Has_Instruction_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean
     with Pre => Instance'Class (This).Has_Value (Env);

   function Has_Register_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean
     with Pre => Instance'Class (This).Has_Value (Env);

   function Has_Word_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Boolean
     with Pre => Instance'Class (This).Has_Value (Env);

   function Get_Instruction_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return As.Instructions.Reference
     with Pre => Instance'Class (This).Has_Instruction_Value (Env);

   function Get_Register_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Register_Index
     with Pre => Instance'Class (This).Has_Register_Value (Env);

   function Get_Static_Value
     (This : Instance)
      return Word_32
     with Pre => Instance'Class (This).Has_Static_Value;

   function Get_Word_Value
     (This : Instance;
      Env  : not null access constant As.Environment.Instance'Class)
      return Word_32
     with Pre => Instance'Class (This).Has_Word_Value (Env);

   function Needs_Mention
     (This   : Instance;
      Env    : not null access As.Environment.Instance'Class)
      return Boolean;

   procedure Mention
     (This    : Instance;
      Env     : not null access As.Environment.Instance'Class;
      Context : Mention_Context;
      Offset  : Word_32 := 0);

   function Word_Value
     (Value : Word_32)
      return Reference;

   function Register
     (Value : Register_Index)
      return Reference;

   function Current_Location
      return Reference;

   function Instruction
     (Value : As.Instructions.Reference)
      return Reference;

   function Identifier
     (Name : String)
      return Reference;

   function Is_Identifier
     (This : Instance'Class)
      return Boolean;

   function Local
     (Index   : Positive;
      Forward : Boolean)
      return Reference;

   function Operate
     (Operator    : Operator_Type;
      Left, Right : Reference)
      return Reference;

   function To_String (This : Instance) return String;

private

   type Node_Class is
     (Word_Node, Register_Node, Instruction_Node,
      Local_Node, Identifier_Node,
      Operator_Node);

   Operator_Symbol : constant array (Operator_Type) of Character :=
                       ('+', '-', '*', '/', '\');

   type Instance (Class : Node_Class) is tagged
      record
         case Class is
            when Word_Node =>
               Current_Loc  : Boolean;
               Word_Value   : Word_32;
            when Register_Node =>
               Reg_Value   : Register_Index;
            when Instruction_Node =>
               Instr_Value : As.Instructions.Reference;
            when Local_Node =>
               Label_Index  : Positive;
               Forward      : Boolean;
            when Identifier_Node =>
               Id_Value     : As.Names.Symbol_Name;
            when Operator_Node =>
               Op           : Operator_Type;
               Left, Right  : Reference;
         end case;
      end record;

   function Is_Identifier
     (This : Instance'Class)
      return Boolean
   is (This.Class = Identifier_Node);

end As.Expressions;
