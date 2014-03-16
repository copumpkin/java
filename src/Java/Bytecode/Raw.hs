{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Java.Bytecode.Raw where

import Data.Int
import Data.Word
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.ByteString as B

import Data.Eliminator.TH

newtype Var1 = Var1 { getVar1 :: Word8  } deriving (Eq, Show)
newtype Var2 = Var2 { getVar2 :: Word16 } deriving (Eq, Show)
newtype Con1 = Con1 { getCon1 :: Word8  } deriving (Eq, Ord, Show, U.Unbox, G.Vector U.Vector, GM.MVector U.MVector)
newtype Con2 = Con2 { getCon2 :: Word16 } deriving (Eq, Ord, Show, U.Unbox, G.Vector U.Vector, GM.MVector U.MVector)
newtype Offset2 = Offset2 { getOffset2 :: Int16 } deriving (Eq, Show, U.Unbox, G.Vector U.Vector, GM.MVector U.MVector)
newtype Offset4 = Offset4 { getOffset4 :: Int32 } deriving (Eq, Show, U.Unbox, G.Vector U.Vector, GM.MVector U.MVector)

data PrimType = T_Boolean | T_Char | T_Float | T_Double | T_Byte | T_Short | T_Int | T_Long
  deriving (Eq, Show)

data Instruction
  = Nop

  | Aconst_null
  | Iconst_m1 | Iconst_0 | Iconst_1 | Iconst_2 | Iconst_3 | Iconst_4 | Iconst_5
  | Lconst_0 | Lconst_1
  | Fconst_0 | Fconst_1 | Fconst_2
  | Dconst_0 | Dconst_1

  | Bipush !Word8
  | Sipush !Word16

  | Ldc    !Con1
  | Ldc_w  !Con2
  | Ldc2_w !Con2

  | Iload !Var1 
  | Lload !Var1
  | Fload !Var1
  | Dload !Var1
  | Aload !Var1

  | Iload_0 | Iload_1 | Iload_2 | Iload_3
  | Lload_0 | Lload_1 | Lload_2 | Lload_3
  | Fload_0 | Fload_1 | Fload_2 | Fload_3
  | Dload_0 | Dload_1 | Dload_2 | Dload_3
  | Aload_0 | Aload_1 | Aload_2 | Aload_3
  
  | Iaload | Laload | Faload | Daload | Aaload | Baload | Caload | Saload

  | Istore !Var1
  | Lstore !Var1
  | Fstore !Var1
  | Dstore !Var1
  | Astore !Var1

  | Istore_0 | Istore_1 | Istore_2 | Istore_3 
  | Lstore_0 | Lstore_1 | Lstore_2 | Lstore_3
  | Fstore_0 | Fstore_1 | Fstore_2 | Fstore_3
  | Dstore_0 | Dstore_1 | Dstore_2 | Dstore_3
  | Astore_0 | Astore_1 | Astore_2 | Astore_3
  
  | Iastore | Lastore | Fastore | Dastore | Aastore | Bastore | Castore | Sastore

  | Pop | Pop2
  | Dup | Dup_x1 | Dup_x2 | Dup2 | Dup2_x1 | Dup2_x2
  | Swap

  | Iadd | Ladd | Fadd | Dadd
  | Isub | Lsub | Fsub | Dsub
  | Imul | Lmul | Fmul | Dmul
  | Idiv | Ldiv | Fdiv | Ddiv
  | Irem | Lrem | Frem | Drem
  | Ineg | Lneg | Fneg | Dneg

  | Ishl  | Lshl
  | Ishr  | Lshr
  | Iushr | Lushr
  | Iand  | Land
  | Ior   | Lor
  | Ixor  | Lxor

  | Iinc !Var1 !Int8

  | I2l | I2f | I2d
  | L2i | L2f | L2d
  | F2i | F2l | F2d
  | D2i | D2l | D2f
  | I2b | I2c | I2s

  | Lcmp | Fcmpl | Fcmpg | Dcmpl | Dcmpg

  | Ifeq      !Offset2
  | Ifne      !Offset2
  | Iflt      !Offset2
  | Ifge      !Offset2
  | Ifgt      !Offset2
  | Ifle      !Offset2
  | If_icmpeq !Offset2     
  | If_icmpne !Offset2     
  | If_icmplt !Offset2     
  | If_icmpge !Offset2     
  | If_icmpgt !Offset2     
  | If_icmple !Offset2     
  | If_acmpeq !Offset2     
  | If_acmpne !Offset2     

  | Goto !Offset2     
  | Jsr  !Offset2     
  | Ret  !Var1

  | Tableswitch  !Offset4 !Int32 !(U.Vector Offset4)
  | Lookupswitch !Offset4 !(U.Vector (Int32, Offset4))

  | Ireturn | Lreturn | Freturn | Dreturn | Areturn | Return

  | Getstatic !Con2
  | Putstatic !Con2
  
  | Getfield !Con2
  | Putfield !Con2

  | Invokevirtual   !Con2
  | Invokespecial   !Con2
  | Invokestatic    !Con2
  | Invokeinterface !Con2
  | Invokedynamic   !Con2

  | New       !Con2
  | Newarray  !PrimType
  | Anewarray !Con2
  | Arraylength

  | Athrow

  | Checkcast  !Con2
  | Instanceof !Con2

  | Monitorenter | Monitorexit

  | Wide WideInstruction
  | Multianewarray !Con2 !Word8
  
  | Ifnull    !Offset2
  | Ifnonnull !Offset2

  | Goto_w !Offset4
  | Jsr_w  !Offset4

  | Breakpoint | Impdep1 | Impdep2
  deriving (Eq, Show)

data WideInstruction
  = Iload_w  !Var2
  | Fload_w  !Var2
  | Aload_w  !Var2
  | Lload_w  !Var2
  | Dload_w  !Var2
  | Istore_w !Var2
  | Fstore_w !Var2
  | Astore_w !Var2
  | Lstore_w !Var2
  | Dstore_w !Var2
  | Ret_w    !Var2 
  | Iinc_w   !Var2 !Int16
  deriving (Eq, Show)

mkElim ''PrimType
mkElim ''Instruction
mkElim ''WideInstruction
