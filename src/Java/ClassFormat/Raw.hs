{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Java.ClassFormat.Raw where

import Data.Int
import Data.Word
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Control.Lens.TH

import Data.Eliminator.TH

import Java.Bytecode.Raw

{-}
data FieldAttribute
  = ConstantValue -- exactly one
  | Synthetic
  | Signature -- 49.0 or above
  | Deprecated
  | RuntimeVisibleAnnotations -- 49.0 or above
  | RuntimeInvisibleAnnotations -- 49.0 or above

data MethodAttribute
  = Code -- native/abstract = 0, otherwise = 1
  | Exceptions
  | Signature -- 49.0 or above
  | Deprecated
  | RuntimeVisibleAnnotations -- 49.0 or above
  | RuntimeInvisibleAnnotations -- 49.0 or above
  | RuntimeVisibleParameterAnnotations -- 49.0 or above
  | RuntimeInvisibleParameterAnnotations -- 49.0 or above
  | AnnotationDefault -- 49.0 or above

data ClassAttribute
  = InnerClasses
  | EnclosingMethod
  | Synthetic
  | Signature -- 49.0 or above
  | SourceFile
  | SourceDebugExtension
  | Deprecated
  | RuntimeVisibleAnnotations -- 49.0 or above
  | RuntimeInvisibleAnnotations -- 49.0 or above
  | BootstrapMethods -- 51.0 or above

data CodeAttribute
  = LineNumberTable
  | LocalVariableTable
  | LocalVariableTypeTable
  | StackMapTable -- 50.0 or above
  -- ignore others
-}

data ReferenceKind
  = Ref_getField
  | Ref_getStatic
  | Ref_putField
  | Ref_putStatic
  | Ref_invokeVirtual
  | Ref_invokeStatic
  | Ref_invokeSpecial
  | Ref_newInvokeSpecial
  | Ref_invokeInterface
  deriving (Eq, Show)

data Constant
  = Utf8               !BL.ByteString -- Use Text, but I'm too lazy to figure out their modified UTF-8 right now
  | Integer            !Word32
  | Float              !Float
  | Long               !Word64
  | Double             !Double
  | ClassName          !Con2
  | String             !Con2
  | FieldRef           !Con2 !Con2
  | MethodRef          !Con2 !Con2
  | InterfaceMethodRef !Con2 !Con2
  | NameAndType        !Con2 !Con2
  | MethodHandle       !ReferenceKind !Con2
  | MethodType         !Con2
  | InvokeDynamic      !Word16 !Con2
  deriving (Eq, Show)

data Exception = Exception { startEx :: !Word16, endEx :: !Word16, handler :: !Word16, catchTypeIndex :: !Con2 } deriving (Eq, Show)

data CodeAttribute = CodeAttribute { maxStack :: !Word16, maxLocals :: !Word16, code :: !BL.ByteString, exceptionTable :: !(V.Vector Exception), codeAttributes :: !(V.Vector Attribute) } deriving (Eq, Show)

data InnerClass = InnerClass { innerClassInfo :: !Con2, outerClassInfo :: !Con2, innerClassName :: !Con2, innerClassAccessFlags :: !Word16 } deriving (Eq, Show)
data LocalVariable = LocalVariable { startPc :: !Word16, length :: !Word16, localVariableName :: !Con2, infoIndex :: !Con2, index :: !Word16 } deriving (Eq, Show)
data Annotation = Annotation { typeIndex :: !Con2, elementValuePairs :: !(V.Vector (Con2, Value)) } deriving (Eq, Show)

-- With better picklers, I could just factor out the PrimType into ConstVal...
data Value
  = ConstBoolVal   !Con2
  | ConstCharVal   !Con2
  | ConstFloatVal  !Con2
  | ConstDoubleVal !Con2
  | ConstByteVal   !Con2
  | ConstShortVal  !Con2
  | ConstIntVal    !Con2
  | ConstLongVal   !Con2
  | ConstStringVal !Con2
  | EnumVal        !Con2 !Con2
  | ClassVal       !Con2
  | AnnotationVal  !Annotation
  | ArrayVal       !(V.Vector Value)
  deriving (Eq, Show)


data Attribute
  = ConstantValue Con2
  | Code !CodeAttribute
  | StackMapTable
  | Exceptions !(V.Vector Con2)
  | InnerClasses !(V.Vector InnerClass)
  | EnclosingMethod !Con2 !Con2
  | Synthetic -- empty
  | Signature !Con2
  | SourceFile !Con2
  | SourceDebugExtension !BL.ByteString
  | LineNumberTable !(U.Vector (Word16, Word16))
  | LocalVariableTable !(V.Vector LocalVariable)
  | LocalVariableTypeTable !(V.Vector LocalVariable)
  | Deprecated -- empty
  | RuntimeVisibleAnnotations !(V.Vector Annotation)
  | RuntimeInvisibleAnnotations !(V.Vector Annotation)
  | RuntimeVisibleParameterAnnotations !(V.Vector (V.Vector Annotation))
  | RuntimeInvisibleParameterAnnotations !(V.Vector (V.Vector Annotation))
  | AnnotationDefault !Value
  | BootstrapMethods
  | Custom !BL.ByteString
  deriving (Eq, Show)

-- field_info and method_info are identical structures, called Entity here
data Entity = Entity { entityAccessFlags :: !Word16, entityName :: !Con2, entityDescriptor :: !Con2, entityAttributes :: !(V.Vector Attribute) } deriving (Eq, Show)

data Class = Class
  { majorVersion      :: {-# UNPACK #-} !Word16
  , minorVersion      :: {-# UNPACK #-} !Word16
  , constantPool      :: !(V.Vector Constant)
  , classAccessFlags  :: {-# UNPACK #-} !Word16
  , className         :: {-# UNPACK #-} !Con2
  , superClassName    :: {-# UNPACK #-} !Con2
  , interfaces        :: !(U.Vector Con2)
  , fields            :: !(V.Vector Entity)
  , methods           :: !(V.Vector Entity)
  , attributes        :: !(V.Vector Attribute)
  } deriving (Eq, Show)

mkElim ''Class
mkElim ''Entity
mkElim ''Attribute
mkElim ''Value
mkElim ''Annotation
mkElim ''LocalVariable
mkElim ''InnerClass
mkElim ''CodeAttribute
mkElim ''Exception
mkElim ''Constant
mkElim ''ReferenceKind

makePrisms ''Constant