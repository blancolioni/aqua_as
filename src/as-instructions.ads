private with As.Names;

limited with As.Environment;
limited with As.Expressions;

with As.Objects;

private package As.Instructions is

   Instruction_Error : exception;

   type Instance is tagged private;
   type Reference is access constant Instance'Class;

   function Alignment (This : Instance'Class) return Word_32;

   function To_String (This : Instance'Class) return String;

   function I_Get return Reference;
   function I_Jmp return Reference;
   function I_Pop return Reference;
   function I_Put return Reference;
   function I_Resume return Reference;
   function I_Trap return Reference;

   function I_Set return Reference;

   function I_Z_Imm
     (Base_Op : Word_8;
      Y_Imm   : Boolean := False)
      return Reference;
   function I_YZ_Imm (Base_Op : Word_8) return Reference;
   function I_Branch (Base_Op : Word_8) return Reference;

   function Data (Element_Size : Positive) return Reference;
   function Segment (Name : String) return Reference;
   function Export return Reference;
   function Extern return Reference;

   type Expression_Reference is access constant As.Expressions.Instance'Class;

   type Instruction_Arguments is
     array (Positive range <>) of Expression_Reference;

   procedure Skip
     (This      : Instance'Class;
      Env       : not null access As.Environment.Instance'Class;
      Arguments : Instruction_Arguments);

   procedure Assemble
     (This      : Instance'Class;
      Env       : not null access constant As.Environment.Instance'Class;
      Arguments : Instruction_Arguments;
      Target    : As.Objects.Reference);

private

   type Special_Instruction is (Get, Put, Resume, Set);

   type Directive_Instruction is (Set_Segment, Export_Symbol, Extern_Symbol);

   type Instance is tagged
      record
         Base_Op       : Word_8  := 0;
         Is_Data       : Boolean := False;
         Is_Special    : Boolean := False;
         Is_Directive  : Boolean := False;
         Z_Imm_Option  : Boolean := False;
         Y_Immediate   : Boolean := False;
         YZ_Immediate  : Boolean := False;
         XYZ_Immediate : Boolean := False;
         Is_Branch     : Boolean := False;
         Has_Rel_Addr  : Boolean := False;
         Is_Pop        : Boolean := False;
         Data_Size     : Positive := 4;
         Mention       : Mention_Context := No_Context;
         Segment_Name  : As.Names.Symbol_Name;
         Special       : Special_Instruction := Get;
         Directive     : Directive_Instruction := Set_Segment;
      end record;

   function Alignment (This : Instance'Class) return Word_32
   is (if This.Is_Data then Word_32 (This.Data_Size) else 4);

   function I_Z_Imm
     (Base_Op : Word_8;
      Y_Imm   : Boolean := False)
      return Reference
   is (new Instance'(Base_Op,
       Z_Imm_Option => True,
       Y_Immediate  => Y_Imm,
       others       => <>));

   function I_YZ_Imm (Base_Op : Word_8) return Reference
   is (new Instance'(Base_Op, YZ_Immediate => True, others => <>));

   function I_Branch (Base_Op : Word_8) return Reference
   is (new Instance'(Base_Op, Is_Branch => True,
                     Mention => Relative_XY, others => <>));

   function Data (Element_Size : Positive) return Reference
   is (new Instance'(Is_Data => True, Data_Size => Element_Size,
                     others  => <>));

   function Segment (Name : String) return Reference
   is (new Instance'(Is_Directive => True,
                     Directive    => Set_Segment,
                     Segment_Name => As.Names."+" (Name),
                     others       => <>));

   function Export return Reference
   is (new Instance'(Is_Directive => True,
                     Directive    => Export_Symbol,
                     others       => <>));

   function Extern return Reference
   is (new Instance'(Is_Directive => True,
                     Directive    => Extern_Symbol,
                     others       => <>));

   Local_Jmp  : aliased constant Instance :=
                  (16#F0#, Has_Rel_Addr => True,
                   Mention => Relative_XYZ, others => <>);

   Local_Pop : aliased constant Instance :=
                 (16#F8#, Is_Pop => True, others => <>);

   Local_Trap : aliased constant Instance :=
                  (16#00#, XYZ_Immediate => True, others => <>);

   function I_Jmp return Reference is (Local_Jmp'Access);
   function I_Pop return Reference is (Local_Pop'Access);
   function I_Trap return Reference is (Local_Trap'Access);

   function I_Put return Reference is
     (new Instance'(16#F6#, Is_Special => True, Special => Put, others => <>));

   function I_Get return Reference is
     (new Instance'(16#FE#, Is_Special => True, Special => Get, others => <>));

   function I_Set return Reference is
     (new Instance'(16#C1#, Is_Special => True, Special => Set, others => <>));

   function I_Resume return Reference is
     (new Instance'(16#F9#, Is_Special => True, Special => Resume,
                    others             => <>));

end As.Instructions;
