{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fcontext-stack=250 #-}
module Java.Bytecode.Encoding where

import Data.Int
import Data.Word
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString as B
import Data.Binary hiding (encode)
import Data.Binary.Get
import Data.Binary.Builder
import Control.Applicative
import Data.Prickler

import Java.Bytecode.Raw

word8  = Prickler getWord8 singleton
word16 = Prickler getWord16be putWord16be
word32 = Prickler getWord32be putWord32be

int8  = wrap fromIntegral fromIntegral word8
int16 = wrap fromIntegral fromIntegral word16
int32 = wrap fromIntegral fromIntegral word32

con1    = wrap Con1    getCon1    word8
con2    = wrap Con2    getCon2    word16
var1    = wrap Var1    getVar1    word8
var2    = wrap Var2    getVar2    word16
offset2 = wrap Offset2 getOffset2 int16
offset4 = wrap Offset4 getOffset4 int32

uvector :: (Integral i, U.Unbox a) => Prickler i -> Prickler a -> Prickler (U.Vector a)
uvector (Prickler gl pl) (Prickler ga pa) = Prickler getter builder
  where
  getter = do len <- gl; U.replicateM (fromIntegral len) ga
  builder xs = pl (fromIntegral (U.length xs)) <> U.foldr' ((<>) . pa) mempty xs -- where's my foldMap??

tableswitch :: Prickler Table
tableswitch = undefined

lookupswitch :: Prickler Lookup
lookupswitch = undefined

primType :: Prickler PrimType
primType = taggedData word8 $ Data (EliminatorWrapper . elim_PrimType) $
     0x04 # Case T_Boolean Nil
  :> 0x05 # Case T_Char    Nil
  :> 0x06 # Case T_Float   Nil
  :> 0x07 # Case T_Double  Nil
  :> 0x08 # Case T_Byte    Nil
  :> 0x09 # Case T_Short   Nil
  :> 0x0a # Case T_Int     Nil
  :> 0x0b # Case T_Long    Nil
  :> Nil

wideInstruction :: Prickler WideInstruction
wideInstruction = taggedData word8 $ Data (EliminatorWrapper . elim_WideInstruction) $
     0x15 # Case Iload_w  (var2 :> Nil)
  :> 0x16 # Case Fload_w  (var2 :> Nil)
  :> 0x17 # Case Aload_w  (var2 :> Nil)
  :> 0x18 # Case Lload_w  (var2 :> Nil)
  :> 0x19 # Case Dload_w  (var2 :> Nil)
  :> 0x36 # Case Istore_w (var2 :> Nil)
  :> 0x37 # Case Fstore_w (var2 :> Nil)
  :> 0x38 # Case Astore_w (var2 :> Nil)
  :> 0x39 # Case Lstore_w (var2 :> Nil)
  :> 0x3a # Case Dstore_w (var2 :> Nil)
  :> 0xa9 # Case Ret_w    (var2 :> Nil)
  :> 0x84 # Case Iinc_w   (var2 :> int16 :> Nil)
  :> Nil

instruction :: Prickler Instruction
instruction = taggedData word8 $ Data (EliminatorWrapper . elim_Instruction) $
     0x00 # Case Nop             Nil      
  :> 0x01 # Case Aconst_null     Nil
  :> 0x02 # Case Iconst_m1       Nil   
  :> 0x03 # Case Iconst_0        Nil   
  :> 0x04 # Case Iconst_1        Nil   
  :> 0x05 # Case Iconst_2        Nil   
  :> 0x06 # Case Iconst_3        Nil   
  :> 0x07 # Case Iconst_4        Nil   
  :> 0x08 # Case Iconst_5        Nil   
  :> 0x09 # Case Lconst_0        Nil   
  :> 0x0a # Case Lconst_1        Nil   
  :> 0x0b # Case Fconst_0        Nil   
  :> 0x0c # Case Fconst_1        Nil   
  :> 0x0d # Case Fconst_2        Nil   
  :> 0x0e # Case Dconst_0        Nil   
  :> 0x0f # Case Dconst_1        Nil   
  :> 0x10 # Case Bipush          (word8 :> Nil)    
  :> 0x11 # Case Sipush          (word16 :> Nil)    
  :> 0x12 # Case Ldc             (con1 :> Nil)    
  :> 0x13 # Case Ldc_w           (con2 :> Nil)    
  :> 0x14 # Case Ldc2_w          (con2 :> Nil)    
  :> 0x15 # Case Iload           (var1 :> Nil)
  :> 0x16 # Case Lload           (var1 :> Nil)
  :> 0x17 # Case Fload           (var1 :> Nil)
  :> 0x18 # Case Dload           (var1 :> Nil)
  :> 0x19 # Case Aload           (var1 :> Nil)
  :> 0x1a # Case Iload_0         Nil  
  :> 0x1b # Case Iload_1         Nil  
  :> 0x1c # Case Iload_2         Nil  
  :> 0x1d # Case Iload_3         Nil  
  :> 0x1e # Case Lload_0         Nil  
  :> 0x1f # Case Lload_1         Nil  
  :> 0x20 # Case Lload_2         Nil  
  :> 0x21 # Case Lload_3         Nil  
  :> 0x22 # Case Fload_0         Nil  
  :> 0x23 # Case Fload_1         Nil  
  :> 0x24 # Case Fload_2         Nil  
  :> 0x25 # Case Fload_3         Nil  
  :> 0x26 # Case Dload_0         Nil  
  :> 0x27 # Case Dload_1         Nil  
  :> 0x28 # Case Dload_2         Nil  
  :> 0x29 # Case Dload_3         Nil  
  :> 0x2a # Case Aload_0         Nil  
  :> 0x2b # Case Aload_1         Nil  
  :> 0x2c # Case Aload_2         Nil  
  :> 0x2d # Case Aload_3         Nil  
  :> 0x2e # Case Iaload          Nil  
  :> 0x2f # Case Laload          Nil  
  :> 0x30 # Case Faload          Nil  
  :> 0x31 # Case Daload          Nil  
  :> 0x32 # Case Aaload          Nil  
  :> 0x33 # Case Baload          Nil  
  :> 0x34 # Case Caload          Nil  
  :> 0x35 # Case Saload          Nil  
  :> 0x36 # Case Istore          (var1 :> Nil)
  :> 0x37 # Case Lstore          (var1 :> Nil)   
  :> 0x38 # Case Fstore          (var1 :> Nil)  
  :> 0x39 # Case Dstore          (var1 :> Nil)  
  :> 0x3a # Case Astore          (var1 :> Nil)  
  :> 0x3b # Case Istore_0        Nil 
  :> 0x3c # Case Istore_1        Nil 
  :> 0x3d # Case Istore_2        Nil 
  :> 0x3e # Case Istore_3        Nil 
  :> 0x3f # Case Lstore_0        Nil 
  :> 0x40 # Case Lstore_1        Nil 
  :> 0x41 # Case Lstore_2        Nil 
  :> 0x42 # Case Lstore_3        Nil 
  :> 0x43 # Case Fstore_0        Nil 
  :> 0x44 # Case Fstore_1        Nil 
  :> 0x45 # Case Fstore_2        Nil 
  :> 0x46 # Case Fstore_3        Nil 
  :> 0x47 # Case Dstore_0        Nil 
  :> 0x48 # Case Dstore_1        Nil 
  :> 0x49 # Case Dstore_2        Nil 
  :> 0x4a # Case Dstore_3        Nil 
  :> 0x4b # Case Astore_0        Nil 
  :> 0x4c # Case Astore_1        Nil 
  :> 0x4d # Case Astore_2        Nil 
  :> 0x4e # Case Astore_3        Nil 
  :> 0x4f # Case Iastore         Nil 
  :> 0x50 # Case Lastore         Nil 
  :> 0x51 # Case Fastore         Nil 
  :> 0x52 # Case Dastore         Nil 
  :> 0x53 # Case Aastore         Nil 
  :> 0x54 # Case Bastore         Nil 
  :> 0x55 # Case Castore         Nil 
  :> 0x56 # Case Sastore         Nil 
  :> 0x57 # Case Pop             Nil 
  :> 0x58 # Case Pop2            Nil 
  :> 0x59 # Case Dup             Nil 
  :> 0x5a # Case Dup_x1          Nil 
  :> 0x5b # Case Dup_x2          Nil 
  :> 0x5c # Case Dup2            Nil 
  :> 0x5d # Case Dup2_x1         Nil 
  :> 0x5e # Case Dup2_x2         Nil 
  :> 0x5f # Case Swap            Nil 
  :> 0x60 # Case Iadd            Nil 
  :> 0x61 # Case Ladd            Nil 
  :> 0x62 # Case Fadd            Nil 
  :> 0x63 # Case Dadd            Nil 
  :> 0x64 # Case Isub            Nil 
  :> 0x65 # Case Lsub            Nil 
  :> 0x66 # Case Fsub            Nil 
  :> 0x67 # Case Dsub            Nil 
  :> 0x68 # Case Imul            Nil 
  :> 0x69 # Case Lmul            Nil 
  :> 0x6a # Case Fmul            Nil 
  :> 0x6b # Case Dmul            Nil 
  :> 0x6c # Case Idiv            Nil 
  :> 0x6d # Case Ldiv            Nil 
  :> 0x6e # Case Fdiv            Nil 
  :> 0x6f # Case Ddiv            Nil 
  :> 0x70 # Case Irem            Nil 
  :> 0x71 # Case Lrem            Nil 
  :> 0x72 # Case Frem            Nil 
  :> 0x73 # Case Drem            Nil 
  :> 0x74 # Case Ineg            Nil 
  :> 0x75 # Case Lneg            Nil 
  :> 0x76 # Case Fneg            Nil 
  :> 0x77 # Case Dneg            Nil 
  :> 0x78 # Case Ishl            Nil 
  :> 0x79 # Case Lshl            Nil 
  :> 0x7a # Case Ishr            Nil 
  :> 0x7b # Case Lshr            Nil 
  :> 0x7c # Case Iushr           Nil 
  :> 0x7d # Case Lushr           Nil 
  :> 0x7e # Case Iand            Nil 
  :> 0x7f # Case Land            Nil 
  :> 0x80 # Case Ior             Nil 
  :> 0x81 # Case Lor             Nil 
  :> 0x82 # Case Ixor            Nil 
  :> 0x83 # Case Lxor            Nil 
  :> 0x84 # Case Iinc            (var1 :> int8 :> Nil)  
  :> 0x85 # Case I2l             Nil   
  :> 0x86 # Case I2f             Nil   
  :> 0x87 # Case I2d             Nil   
  :> 0x88 # Case L2i             Nil   
  :> 0x89 # Case L2f             Nil   
  :> 0x8a # Case L2d             Nil   
  :> 0x8b # Case F2i             Nil   
  :> 0x8c # Case F2l             Nil   
  :> 0x8d # Case F2d             Nil   
  :> 0x8e # Case D2i             Nil   
  :> 0x8f # Case D2l             Nil   
  :> 0x90 # Case D2f             Nil   
  :> 0x91 # Case I2b             Nil   
  :> 0x92 # Case I2c             Nil   
  :> 0x93 # Case I2s             Nil   
  :> 0x94 # Case Lcmp            Nil   
  :> 0x95 # Case Fcmpl           Nil   
  :> 0x96 # Case Fcmpg           Nil   
  :> 0x97 # Case Dcmpl           Nil   
  :> 0x98 # Case Dcmpg           Nil   
  :> 0x99 # Case Ifeq            (offset2 :> Nil)  
  :> 0x9a # Case Ifne            (offset2 :> Nil)  
  :> 0x9b # Case Iflt            (offset2 :> Nil)  
  :> 0x9c # Case Ifge            (offset2 :> Nil)  
  :> 0x9d # Case Ifgt            (offset2 :> Nil)  
  :> 0x9e # Case Ifle            (offset2 :> Nil)  
  :> 0x9f # Case If_icmpeq       (offset2 :> Nil)  
  :> 0xa0 # Case If_icmpne       (offset2 :> Nil)  
  :> 0xa1 # Case If_icmplt       (offset2 :> Nil)  
  :> 0xa2 # Case If_icmpge       (offset2 :> Nil)  
  :> 0xa3 # Case If_icmpgt       (offset2 :> Nil)  
  :> 0xa4 # Case If_icmple       (offset2 :> Nil)  
  :> 0xa5 # Case If_acmpeq       (offset2 :> Nil)  
  :> 0xa6 # Case If_acmpne       (offset2 :> Nil)  
  :> 0xa7 # Case Goto            (offset2 :> Nil)     
  :> 0xa8 # Case Jsr             (offset2 :> Nil)     
  :> 0xa9 # Case Ret             (var1 :> Nil)     
  :> 0xaa # Case Tableswitch     (tableswitch :> Nil)
  :> 0xab # Case Lookupswitch    (lookupswitch :> Nil)   
  :> 0xac # Case Ireturn         Nil   
  :> 0xad # Case Lreturn         Nil   
  :> 0xae # Case Freturn         Nil   
  :> 0xaf # Case Dreturn         Nil   
  :> 0xb0 # Case Areturn         Nil   
  :> 0xb1 # Case Return          Nil   
  :> 0xb2 # Case Getstatic       (con2 :> Nil)
  :> 0xb3 # Case Putstatic       (con2 :> Nil)
  :> 0xb4 # Case Getfield        (con2 :> Nil)
  :> 0xb5 # Case Putfield        (con2 :> Nil)
  :> 0xb6 # Case Invokevirtual   (con2 :> Nil)
  :> 0xb7 # Case Invokespecial   (con2 :> Nil)
  :> 0xb8 # Case Invokestatic    (con2 :> Nil)
  :> 0xb9 # Case Invokeinterface (con2 :> Nil)
  :> 0xba # Case Invokedynamic   (con2 :> Nil)
  :> 0xbb # Case New             (con2 :> Nil)
  :> 0xbc # Case Newarray        (primType :> Nil) 
  :> 0xbd # Case Anewarray       (con2 :> Nil)  
  :> 0xbe # Case Arraylength     Nil   
  :> 0xbf # Case Athrow          Nil   
  :> 0xc0 # Case Checkcast       (con2 :> Nil) 
  :> 0xc1 # Case Instanceof      (con2 :> Nil) 
  :> 0xc2 # Case Monitorenter    Nil   
  :> 0xc3 # Case Monitorexit     Nil   
  :> 0xc4 # Case Wide            (wideInstruction :> Nil) 
  :> 0xc5 # Case Multianewarray  (con2 :> word8 :> Nil)
  :> 0xc6 # Case Ifnull          (offset2 :> Nil)
  :> 0xc7 # Case Ifnonnull       (offset2 :> Nil)
  :> 0xc8 # Case Goto_w          (offset4 :> Nil) 
  :> 0xc9 # Case Jsr_w           (offset4 :> Nil)
  :> 0xca # Case Breakpoint      Nil   
  :> 0xfe # Case Impdep1         Nil   
  :> 0xff # Case Impdep2         Nil
  :> Nil

