{-# OPTIONS_GHC -funbox-strict-fields #-}
module Java.ClassFormat.Raw where

import Data.Int
import Data.Word
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

newtype Con1 = Con1 { getCon1 :: Word8  } deriving (Eq, Show)
newtype Con2 = Con2 { getCon2 :: Word16 } deriving (Eq, Show)

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

data Constant
  = Utf8
  | Integer
  | Float
  | Long
  | Double
  | ClassName
  | String
  | FieldRef
  | MethodRef
  | InterfaceMethodRef
  | NameAndType
  | MethodHandle
  | MethodType
  | InvokeDynamic

data Exception = Exception { startPc :: !Word16, endPc :: !Word16, handlerPc :: !Word16, catchTypeIndex :: !Con2 }

data CodeAttribute = CodeAttribute { maxStack :: !Word16, maxLocals :: !Word16, code :: !B.ByteString, exceptionTable :: !(V.Vector Exception), codeAttributes :: !(V.Vector Attribute) }
data EnclosingMethodAttribute = EnclosingMethodAttribute { classIndex :: !Con2, methodIndex :: !Con2 }

data InnerClass = InnerClass { innerClassInfo :: !Con2, outerClassInfo :: !Con2, innerName :: !Con2, accessFlags :: !Word16 }
data LocalVariable = LocalVariable { startPc :: !Word16, length :: !Word16, nameIndex :: !Con2, descriptorIndex :: !Con2, index :: !Word16 }
data LocalVariableType = LocalVariableType { startPc :: !Word16, length :: !Word16, nameIndex :: !Con2, signatureIndex :: !Con2, index :: !Word16 }
data Annotation = Annotation { typeIndex :: Con2, elementValuePairs :: !(Vector (Con2, Value)) }
data Value
  = Const !Con2
  | Enum !Con2 !Con2
  | Class !Con2
  | Annotation !Annotation
  | Array !(V.Vector Value)

data Attribute
  = ConstantValue Con2
  | Code !CodeAttribute
  | StackMapTable
  | Exceptions !(V.Vector Con2)
  | InnerClasses !(V.Vector InnerClass)
  | EnclosingMethod !EnclosingMethodAttribute
  | Synthetic -- empty
  | Signature !Con2
  | SourceFile !Con2
  | SourceDebugExtension !B.ByteString
  | LineNumberTable !(U.Vector (Word16, Word16))
  | LocalVariableTable !(V.Vector LocalVariable)
  | LocalVariableTypeTable !(V.Vector LocalVariableType)
  | Deprecated -- empty
  | RuntimeVisibleAnnotations !(V.Vector Annotation)
  | RuntimeInvisibleAnnotations !(V.Vector Annotation)
  | RuntimeVisibleParameterAnnotations !(V.Vector (V.Vector Annotation))
  | RuntimeInvisibleParameterAnnotations !(V.Vector (V.Vector Annotation))
  | AnnotationDefault
  | BootstrapMethods
  | Custom B.ByteString

data Class = Class
  { majorVersion :: {-# UNPACK #-} !Word16
  , minorVersion :: {-# UNPACK #-} !Word16
  , constantPool :: !(V.Vector Constant)
  , accessFlags  :: {-# UNPACK #-} !Word16
  , className    :: {-# UNPACK #-} !Word16
  , superClass   :: {-# UNPACK #-} !Word16
  , interfaces   :: !(V.Vector Interface)
  , fields       :: !(V.Vector Field)
  , methods      :: !(V.Vector Method)
  , attributes   :: !(V.Vector Attribute)
  }
