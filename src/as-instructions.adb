with As.Environment;
with As.Expressions;
with As.Images;
with As.Segments;

package body As.Instructions is

   --------------
   -- Assemble --
   --------------

   procedure Assemble
     (This      : Instance'Class;
      Env       : not null access constant As.Environment.Instance'Class;
      Arguments : Instruction_Arguments;
      Target    : As.Objects.Reference)
   is
      procedure Asm (Op, X, Y, Z : Word_8);

      function Op1 return Expression_Reference
      is (Arguments (Arguments'First));

      function Op2 return Expression_Reference
      is (Arguments (Arguments'First + 1));

      function Op3 return Expression_Reference
      is (Arguments (Arguments'First + 2));

      function Immediate_Z return Boolean
      is (This.Z_Imm_Option
          and then (Arguments'Length = 2
            or else not (Op3.Has_Register_Value (Env))));

      ---------
      -- Asm --
      ---------

      procedure Asm (Op, X, Y, Z : Word_8) is
      begin
         Target.Append (Op);
         Target.Append (X);
         Target.Append (Y);
         Target.Append (Z);
      end Asm;

   begin
      if This.Is_Data then
         for Arg of Arguments loop
            declare
               Ws : array (1 .. This.Data_Size) of Word_8;
               It : Word_32 :=
                      (if Arg.Has_Value (Env)
                       then Arg.Get_Word_Value (Env)
                       else 0);
            begin
               for X of reverse Ws loop
                  X := Word_8 (It mod 256);
                  It := It / 256;
               end loop;
               for X of Ws loop
                  Target.Append (X);
               end loop;
            end;
         end loop;
      elsif This.Is_Directive then
         case This.Directive is
            when Export_Symbol =>
               null;
            when Extern_Symbol =>
               null;
            when Set_Segment =>
               Target.Set_Current (As.Names."-" (This.Segment_Name));
         end case;
      elsif This.Is_Branch then
         declare
            PC : constant Word_32 :=
                   As.Segments.Location (Target.Location);
            Address : constant Word_32 :=
                        (if Op2.Has_Value (Env)
                         then Op2.Get_Word_Value (Env)
                         else 0);
            Offset  : constant Word_32 :=
                        (if Address >= PC
                         then Address - PC
                         else (not (PC - Address)) + 1)
                        / 4;
            Code    : constant Word_8 :=
                        (if Address >= PC or else not Op2.Has_Value (Env)
                         then This.Base_Op
                         else This.Base_Op + 1);
            X       : constant Word_8 :=
                        Word_8 (Op1.Get_Register_Value (Env));
         begin
            Asm (Code, X,
                 Word_8 (Offset / 256 mod 256),
                 Word_8 (Offset mod 256));
         end;
      elsif This.XYZ_Immediate then
         Asm (This.Base_Op,
              Word_8 (Op1.Get_Word_Value (Env) mod 256),
              Word_8 (Op2.Get_Word_Value (Env) mod 256),
              Word_8 (Op3.Get_Word_Value (Env) mod 256));
      elsif This.YZ_Immediate then
         declare
            X       : constant Word_8 :=
                        Word_8 (Op1.Get_Register_Value (Env));
            YZ      : constant Word_32 :=
                        Op2.Get_Word_Value (Env);
         begin
            Asm (This.Base_Op, X,
                 Word_8 (YZ / 256 mod 256),
                 Word_8 (YZ mod 256));
         end;
      elsif This.Z_Imm_Option then
         if Immediate_Z
           and then Op3.Get_Word_Value (Env) > 255
         then
            raise Instruction_Error with
              "immediate Z must be <= 255";
         end if;

         declare
            Immediate_Z : constant Boolean :=
                            Arguments'Length = 2
                            or else not (Op3.Has_Register_Value (Env));
            Opcode      : constant Word_8 :=
                            (if Immediate_Z
                             then This.Base_Op + 1
                             else This.Base_Op);
            X       : constant Word_8 :=
                        Word_8 (Op1.Get_Register_Value (Env));
            Y           : constant Word_8 :=
                            (if This.Y_Immediate
                             then Word_8 (Op2.Get_Word_Value (Env))
                             else Word_8 (Op2.Get_Register_Value (Env)));
            Z           : constant Word_8 :=
                            (if Arguments'Length = 2
                             then 0
                             elsif Immediate_Z
                             then Word_8 (Op3.Get_Word_Value (Env))
                             else Word_8 (Op3.Get_Register_Value (Env)));
         begin
            if This.Y_Immediate and then Arguments'Length = 2 then
               Asm (Opcode, X, Z, Y);
            else
               Asm (Opcode, X, Y, Z);
            end if;
         end;
      elsif This.Has_Rel_Addr then
         declare
            PC      : constant Word_32 :=
                        As.Segments.Location (Target.Location);
            Address : constant Word_32 :=
                        (if Op1.Has_Value (Env)
                         then Op1.Get_Word_Value (Env)
                         else 0);
            Offset  : constant Word_32 :=
                        (if Address >= PC
                         then Address - PC
                         else (not (PC - Address)) + 1)
                        / 4;
            Code    : constant Word_8 :=
                        (if Address >= PC
                         then This.Base_Op
                         else This.Base_Op + 1);
         begin
            Asm (Code,
                 Word_8 (Offset / 65536 mod 256),
                 Word_8 (Offset / 256 mod 256),
                 Word_8 (Offset mod 256));
         end;
      elsif This.Is_Pop then
         declare
            X : constant Word_8 := Word_8 (Op1.Get_Word_Value (Env));
            Offset : constant Word_32 := Op2.Get_Word_Value (Env);
         begin
            Asm (This.Base_Op, X,
                 Word_8 (Offset / 256), Word_8 (Offset mod 256));
         end;
      elsif This.Is_Special then
         case This.Special is
            when Get =>
               declare
                  X : constant Word_8 := Word_8 (Op1.Get_Register_Value (Env));
                  Z : constant Word_8 := Word_8 (Op2.Get_Word_Value (Env));
               begin
                  Asm (This.Base_Op, X, 0, Z);
               end;
            when Put =>
               declare
                  Immediate : constant Boolean := Op2.Has_Word_Value (Env);
                  Code      : constant Word_8 :=
                                This.Base_Op + Boolean'Pos (Immediate);
                  X         : constant Word_8 :=
                                Word_8 (Op1.Get_Word_Value (Env));
                  Z         : constant Word_32 :=
                                (if Immediate
                                 then Op2.Get_Word_Value (Env) mod 65536
                                 else Word_32 (Op2.Get_Register_Value (Env)));
               begin
                  Asm (Code, X, Word_8 (Z / 256 mod 256), Word_8 (Z mod 256));
               end;

            when Resume =>
               declare
                  Z : constant Word_8 := Word_8 (Op1.Get_Word_Value (Env));
               begin
                  Asm (This.Base_Op, 0, 0, Z);
               end;

            when Set =>
               declare
                  Is_RR : constant Boolean :=
                            Op2.Has_Register_Value (Env);
                  Is_RW : constant Boolean :=
                            Op2.Has_Word_Value (Env);
                  Opcode      : constant Word_8 :=
                                  (if Is_RR
                                   then 16#C1#
                                   elsif Is_RW
                                   then 16#E1#
                                   else (raise Constraint_Error with
                                     "bad argument to set"));
                  X           : constant Word_8 :=
                                  Word_8 (Op1.Get_Register_Value (Env));
               begin
                  if Is_RR then
                     declare
                        Y           : constant Word_8 :=
                                        Word_8 (Op2.Get_Register_Value (Env));
                     begin
                        Asm (Opcode, X, Y, 0);
                     end;
                  else
                     declare
                        YZ : constant Word_16 :=
                               Word_16 (Op2.Get_Word_Value (Env));
                     begin
                        Asm (Opcode, X,
                             Word_8 (YZ / 256), Word_8 (YZ mod 256));
                     end;
                  end if;
               end;

         end case;
      else
         Asm (16#BA#, 16#AD#, 16#F0#, 16#0D#);
      end if;
   end Assemble;

   ----------
   -- Skip --
   ----------

   procedure Skip
     (This      : Instance'Class;
      Env       : not null access As.Environment.Instance'Class;
      Arguments : Instruction_Arguments)
   is
   begin
      if This.Is_Data then
         declare
            Offset : Word_32 := 0;
         begin
            for Arg of Arguments loop
               Arg.Mention (Env, No_Context, Offset);
               Offset := Offset + Word_32 (This.Data_Size);
            end loop;
         end;

         Env.Set_Location (As.Segments.Location (Env.Location)
                           + Word_32 (This.Data_Size)
                           * Word_32 (Arguments'Length));
      elsif This.Is_Directive then
         case This.Directive is
            when Export_Symbol =>
               for Arg of Arguments loop
                  Env.Export (Arg.To_String);
               end loop;
            when Extern_Symbol =>
               for Arg of Arguments loop
                  Env.Extern (Arg.To_String);
               end loop;
            when Set_Segment =>
               Env.Set_Current (As.Names."-" (This.Segment_Name));
         end case;
      else
         for Arg of Arguments loop
            Arg.Mention (Env, This.Mention);
         end loop;
         Env.Set_Location (As.Segments.Location (Env.Location) + 4);
      end if;
   end Skip;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Instance'Class) return String is
   begin
      return "[" & As.Images.Hex_Image (This.Base_Op) & "]";
   end To_String;

end As.Instructions;
