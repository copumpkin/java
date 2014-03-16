{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fcontext-stack=250 #-}
module Java.Bytecode.Encoding where
import Data.Int
import Data.Word
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString as B
import Data.Binary.Get hiding (Partial)
import Data.Binary.Builder
import Control.Applicative
import Data.Prickler
import Java.Bytecode.Raw

con1    = wrap Con1    getCon1    word8
con2    = wrap Con2    getCon2    word16be
var1    = wrap Var1    getVar1    word8
var2    = wrap Var2    getVar2    word16be
offset2 = wrap Offset2 getOffset2 int16be
offset4 = wrap Offset4 getOffset4 int32be

tableswitch :: Partial Put Get Instruction '[ Offset4, Int32, U.Vector Offset4 ]
tableswitch = Partial getter putter Tableswitch
  where
  getter = alignedGet 4 $ do
    def  <- get offset4
    low  <- get int32be
    high <- get int32be
    offs <- U.replicateM (fromIntegral $ high - low + 1) (get offset4)
    return $ Tableswitch def low offs

  putter def low offs = alignedPut 4 $ put offset4 def <> put int32be low <> put int32be (low + fromIntegral (U.length offs) + 1) <> U.foldr' ((<>) . put offset4) mempty offs


lookupswitch :: Partial Put Get Instruction '[ Offset4, U.Vector (Int32, Offset4) ]
lookupswitch = Partial getter putter Lookupswitch
  where
  getter = alignedGet 4 $ do
    def   <- get offset4
    len   <- get int32be
    pairs <- U.replicateM (fromIntegral len) (liftA2 (,) (get int32be) (get offset4))
    return $ Lookupswitch def pairs

  putPair = put (pair int32be offset4)
  putter def pairs = alignedPut 4 $ put offset4 def <> put int32be (fromIntegral (U.length pairs)) <> U.foldr' ((<>) . putPair) mempty pairs

primType :: Prickler PrimType
primType = tagged word8 (EliminatorWrapper . elim_PrimType)
  $  0x04 # alt T_Boolean Nil
  :> 0x05 # alt T_Char    Nil
  :> 0x06 # alt T_Float   Nil
  :> 0x07 # alt T_Double  Nil
  :> 0x08 # alt T_Byte    Nil
  :> 0x09 # alt T_Short   Nil
  :> 0x0a # alt T_Int     Nil
  :> 0x0b # alt T_Long    Nil
  :> Nil

wideInstruction :: Prickler WideInstruction
wideInstruction = tagged word8 (EliminatorWrapper . elim_WideInstruction)
  $  0x15 # alt Iload_w  (var2 :> Nil)
  :> 0x16 # alt Fload_w  (var2 :> Nil)
  :> 0x17 # alt Aload_w  (var2 :> Nil)
  :> 0x18 # alt Lload_w  (var2 :> Nil)
  :> 0x19 # alt Dload_w  (var2 :> Nil)
  :> 0x36 # alt Istore_w (var2 :> Nil)
  :> 0x37 # alt Fstore_w (var2 :> Nil)
  :> 0x38 # alt Astore_w (var2 :> Nil)
  :> 0x39 # alt Lstore_w (var2 :> Nil)
  :> 0x3a # alt Dstore_w (var2 :> Nil)
  :> 0xa9 # alt Ret_w    (var2 :> Nil)
  :> 0x84 # alt Iinc_w   (var2 :> int16be :> Nil)
  :> Nil

-- My condolences to anyone who causes a type error in this expression
instruction :: Prickler Instruction
instruction = tagged word8 (EliminatorWrapper . elim_Instruction)
  $  0x00 # alt Nop             Nil
  :> 0x01 # alt Aconst_null     Nil
  :> 0x02 # alt Iconst_m1       Nil
  :> 0x03 # alt Iconst_0        Nil
  :> 0x04 # alt Iconst_1        Nil
  :> 0x05 # alt Iconst_2        Nil
  :> 0x06 # alt Iconst_3        Nil
  :> 0x07 # alt Iconst_4        Nil
  :> 0x08 # alt Iconst_5        Nil
  :> 0x09 # alt Lconst_0        Nil
  :> 0x0a # alt Lconst_1        Nil
  :> 0x0b # alt Fconst_0        Nil
  :> 0x0c # alt Fconst_1        Nil
  :> 0x0d # alt Fconst_2        Nil
  :> 0x0e # alt Dconst_0        Nil
  :> 0x0f # alt Dconst_1        Nil
  :> 0x10 # alt Bipush          (word8 :> Nil)    
  :> 0x11 # alt Sipush          (word16be :> Nil)    
  :> 0x12 # alt Ldc             (con1 :> Nil)    
  :> 0x13 # alt Ldc_w           (con2 :> Nil)    
  :> 0x14 # alt Ldc2_w          (con2 :> Nil)    
  :> 0x15 # alt Iload           (var1 :> Nil)
  :> 0x16 # alt Lload           (var1 :> Nil)
  :> 0x17 # alt Fload           (var1 :> Nil)
  :> 0x18 # alt Dload           (var1 :> Nil)
  :> 0x19 # alt Aload           (var1 :> Nil)
  :> 0x1a # alt Iload_0         Nil
  :> 0x1b # alt Iload_1         Nil
  :> 0x1c # alt Iload_2         Nil
  :> 0x1d # alt Iload_3         Nil
  :> 0x1e # alt Lload_0         Nil
  :> 0x1f # alt Lload_1         Nil
  :> 0x20 # alt Lload_2         Nil
  :> 0x21 # alt Lload_3         Nil
  :> 0x22 # alt Fload_0         Nil
  :> 0x23 # alt Fload_1         Nil
  :> 0x24 # alt Fload_2         Nil
  :> 0x25 # alt Fload_3         Nil
  :> 0x26 # alt Dload_0         Nil
  :> 0x27 # alt Dload_1         Nil
  :> 0x28 # alt Dload_2         Nil
  :> 0x29 # alt Dload_3         Nil
  :> 0x2a # alt Aload_0         Nil
  :> 0x2b # alt Aload_1         Nil
  :> 0x2c # alt Aload_2         Nil
  :> 0x2d # alt Aload_3         Nil
  :> 0x2e # alt Iaload          Nil
  :> 0x2f # alt Laload          Nil
  :> 0x30 # alt Faload          Nil
  :> 0x31 # alt Daload          Nil
  :> 0x32 # alt Aaload          Nil
  :> 0x33 # alt Baload          Nil
  :> 0x34 # alt Caload          Nil
  :> 0x35 # alt Saload          Nil
  :> 0x36 # alt Istore          (var1 :> Nil)
  :> 0x37 # alt Lstore          (var1 :> Nil)   
  :> 0x38 # alt Fstore          (var1 :> Nil)  
  :> 0x39 # alt Dstore          (var1 :> Nil)  
  :> 0x3a # alt Astore          (var1 :> Nil)  
  :> 0x3b # alt Istore_0        Nil
  :> 0x3c # alt Istore_1        Nil
  :> 0x3d # alt Istore_2        Nil
  :> 0x3e # alt Istore_3        Nil
  :> 0x3f # alt Lstore_0        Nil
  :> 0x40 # alt Lstore_1        Nil
  :> 0x41 # alt Lstore_2        Nil
  :> 0x42 # alt Lstore_3        Nil
  :> 0x43 # alt Fstore_0        Nil
  :> 0x44 # alt Fstore_1        Nil
  :> 0x45 # alt Fstore_2        Nil
  :> 0x46 # alt Fstore_3        Nil
  :> 0x47 # alt Dstore_0        Nil
  :> 0x48 # alt Dstore_1        Nil
  :> 0x49 # alt Dstore_2        Nil
  :> 0x4a # alt Dstore_3        Nil
  :> 0x4b # alt Astore_0        Nil
  :> 0x4c # alt Astore_1        Nil
  :> 0x4d # alt Astore_2        Nil
  :> 0x4e # alt Astore_3        Nil
  :> 0x4f # alt Iastore         Nil
  :> 0x50 # alt Lastore         Nil
  :> 0x51 # alt Fastore         Nil
  :> 0x52 # alt Dastore         Nil
  :> 0x53 # alt Aastore         Nil
  :> 0x54 # alt Bastore         Nil
  :> 0x55 # alt Castore         Nil
  :> 0x56 # alt Sastore         Nil
  :> 0x57 # alt Pop             Nil
  :> 0x58 # alt Pop2            Nil
  :> 0x59 # alt Dup             Nil
  :> 0x5a # alt Dup_x1          Nil
  :> 0x5b # alt Dup_x2          Nil
  :> 0x5c # alt Dup2            Nil
  :> 0x5d # alt Dup2_x1         Nil
  :> 0x5e # alt Dup2_x2         Nil
  :> 0x5f # alt Swap            Nil
  :> 0x60 # alt Iadd            Nil
  :> 0x61 # alt Ladd            Nil
  :> 0x62 # alt Fadd            Nil
  :> 0x63 # alt Dadd            Nil
  :> 0x64 # alt Isub            Nil
  :> 0x65 # alt Lsub            Nil
  :> 0x66 # alt Fsub            Nil
  :> 0x67 # alt Dsub            Nil
  :> 0x68 # alt Imul            Nil
  :> 0x69 # alt Lmul            Nil
  :> 0x6a # alt Fmul            Nil
  :> 0x6b # alt Dmul            Nil
  :> 0x6c # alt Idiv            Nil
  :> 0x6d # alt Ldiv            Nil
  :> 0x6e # alt Fdiv            Nil
  :> 0x6f # alt Ddiv            Nil
  :> 0x70 # alt Irem            Nil
  :> 0x71 # alt Lrem            Nil
  :> 0x72 # alt Frem            Nil
  :> 0x73 # alt Drem            Nil
  :> 0x74 # alt Ineg            Nil
  :> 0x75 # alt Lneg            Nil
  :> 0x76 # alt Fneg            Nil
  :> 0x77 # alt Dneg            Nil
  :> 0x78 # alt Ishl            Nil
  :> 0x79 # alt Lshl            Nil
  :> 0x7a # alt Ishr            Nil
  :> 0x7b # alt Lshr            Nil
  :> 0x7c # alt Iushr           Nil
  :> 0x7d # alt Lushr           Nil
  :> 0x7e # alt Iand            Nil
  :> 0x7f # alt Land            Nil
  :> 0x80 # alt Ior             Nil
  :> 0x81 # alt Lor             Nil
  :> 0x82 # alt Ixor            Nil
  :> 0x83 # alt Lxor            Nil
  :> 0x84 # alt Iinc            (var1 :> int8 :> Nil)
  :> 0x85 # alt I2l             Nil
  :> 0x86 # alt I2f             Nil
  :> 0x87 # alt I2d             Nil
  :> 0x88 # alt L2i             Nil
  :> 0x89 # alt L2f             Nil
  :> 0x8a # alt L2d             Nil
  :> 0x8b # alt F2i             Nil
  :> 0x8c # alt F2l             Nil
  :> 0x8d # alt F2d             Nil
  :> 0x8e # alt D2i             Nil
  :> 0x8f # alt D2l             Nil
  :> 0x90 # alt D2f             Nil
  :> 0x91 # alt I2b             Nil
  :> 0x92 # alt I2c             Nil
  :> 0x93 # alt I2s             Nil
  :> 0x94 # alt Lcmp            Nil
  :> 0x95 # alt Fcmpl           Nil
  :> 0x96 # alt Fcmpg           Nil
  :> 0x97 # alt Dcmpl           Nil
  :> 0x98 # alt Dcmpg           Nil
  :> 0x99 # alt Ifeq            (offset2 :> Nil)
  :> 0x9a # alt Ifne            (offset2 :> Nil)
  :> 0x9b # alt Iflt            (offset2 :> Nil)
  :> 0x9c # alt Ifge            (offset2 :> Nil)
  :> 0x9d # alt Ifgt            (offset2 :> Nil)
  :> 0x9e # alt Ifle            (offset2 :> Nil)
  :> 0x9f # alt If_icmpeq       (offset2 :> Nil)
  :> 0xa0 # alt If_icmpne       (offset2 :> Nil)
  :> 0xa1 # alt If_icmplt       (offset2 :> Nil)
  :> 0xa2 # alt If_icmpge       (offset2 :> Nil)
  :> 0xa3 # alt If_icmpgt       (offset2 :> Nil)
  :> 0xa4 # alt If_icmple       (offset2 :> Nil)
  :> 0xa5 # alt If_acmpeq       (offset2 :> Nil)
  :> 0xa6 # alt If_acmpne       (offset2 :> Nil)
  :> 0xa7 # alt Goto            (offset2 :> Nil)
  :> 0xa8 # alt Jsr             (offset2 :> Nil)
  :> 0xa9 # alt Ret             (var1 :> Nil)
  :> 0xaa # tableswitch
  :> 0xab # lookupswitch
  :> 0xac # alt Ireturn         Nil
  :> 0xad # alt Lreturn         Nil
  :> 0xae # alt Freturn         Nil
  :> 0xaf # alt Dreturn         Nil
  :> 0xb0 # alt Areturn         Nil
  :> 0xb1 # alt Return          Nil
  :> 0xb2 # alt Getstatic       (con2 :> Nil)
  :> 0xb3 # alt Putstatic       (con2 :> Nil)
  :> 0xb4 # alt Getfield        (con2 :> Nil)
  :> 0xb5 # alt Putfield        (con2 :> Nil)
  :> 0xb6 # alt Invokevirtual   (con2 :> Nil)
  :> 0xb7 # alt Invokespecial   (con2 :> Nil)
  :> 0xb8 # alt Invokestatic    (con2 :> Nil)
  :> 0xb9 # alt Invokeinterface (con2 :> Nil)
  :> 0xba # alt Invokedynamic   (con2 :> Nil)
  :> 0xbb # alt New             (con2 :> Nil)
  :> 0xbc # alt Newarray        (primType :> Nil) 
  :> 0xbd # alt Anewarray       (con2 :> Nil)  
  :> 0xbe # alt Arraylength     Nil
  :> 0xbf # alt Athrow          Nil
  :> 0xc0 # alt Checkcast       (con2 :> Nil) 
  :> 0xc1 # alt Instanceof      (con2 :> Nil) 
  :> 0xc2 # alt Monitorenter    Nil
  :> 0xc3 # alt Monitorexit     Nil
  :> 0xc4 # alt Wide            (wideInstruction :> Nil) 
  :> 0xc5 # alt Multianewarray  (con2 :> word8 :> Nil)
  :> 0xc6 # alt Ifnull          (offset2 :> Nil)
  :> 0xc7 # alt Ifnonnull       (offset2 :> Nil)
  :> 0xc8 # alt Goto_w          (offset4 :> Nil) 
  :> 0xc9 # alt Jsr_w           (offset4 :> Nil)
  :> 0xca # alt Breakpoint      Nil
  :> 0xfe # alt Impdep1         Nil
  :> 0xff # alt Impdep2         Nil
  :> Nil