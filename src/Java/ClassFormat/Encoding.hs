{-# LANGUAGE OverloadedStrings #-}
module Java.ClassFormat.Encoding where

import Data.Char
import Data.Word
import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Prickler
import qualified Data.Vector as V
import Data.Binary.Get hiding (skip, Partial)
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Control.Lens hiding ((#), Indexed)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Debug.Trace

import Java.ClassFormat.Raw hiding (constantPool, interfaces, fields, methods, attributes)
import Java.Bytecode.Raw
import Java.Bytecode.Encoding

import Text.Show.Pretty (ppShow)


value = tagged (wrap (chr . fromIntegral) (fromIntegral . ord) word8) (EliminatorWrapper . elim_Value)
  $  'B' # alt ConstByteVal   (con2 :> Nil)
  :> 'C' # alt ConstCharVal   (con2 :> Nil)
  :> 'D' # alt ConstDoubleVal (con2 :> Nil)
  :> 'F' # alt ConstFloatVal  (con2 :> Nil)
  :> 'I' # alt ConstIntVal    (con2 :> Nil)
  :> 'J' # alt ConstLongVal   (con2 :> Nil)
  :> 'S' # alt ConstShortVal  (con2 :> Nil)
  :> 'Z' # alt ConstBoolVal   (con2 :> Nil)
  :> 's' # alt ConstStringVal (con2 :> Nil)
  :> 'e' # alt EnumVal        (con2 :> con2 :> Nil)
  :> 'c' # alt ClassVal       (con2 :> Nil)
  :> '@' # alt AnnotationVal  (annotation :> Nil)
  :> '[' # alt ArrayVal       (gvector word16be value :> Nil)
  :> Nil

exception     = untagged elim_Exception     . alt Exception     $ word16be :> word16be :> word16be :> con2 :> Nil
annotation    = untagged elim_Annotation    . alt Annotation    $ con2 :> gvector word16be (pair con2 value) :> Nil
innerClass    = untagged elim_InnerClass    . alt InnerClass    $ con2 :> con2 :> con2 :> word16be :> Nil
localVariable = untagged elim_LocalVariable . alt LocalVariable $ word16be :> word16be :> con2 :>con2 :> word16be :> Nil


attribute :: IM.IntMap Constant -> Prickler Attribute
attribute cpool = inner
  where
  inner = taggedSized con2 word32be (EliminatorWrapper . elim_Attribute)
    $  attr "ConstantValue"                        #? alt ConstantValue                        (con2 :> Nil)
    :> attr "Code"                                 #? alt Code                                 (codeAttribute inner :> Nil)
    :> attr "StackMapTable"                        #? alt StackMapTable                        Nil
    :> attr "Exceptions"                           #? alt Exceptions                           (gvector word16be con2 :> Nil)
    :> attr "InnerClasses"                         #? alt InnerClasses                         (gvector word16be innerClass :> Nil)
    :> attr "EnclosingMethod"                      #? alt EnclosingMethod                      (con2 :> con2 :> Nil)
    :> attr "Synthetic"                            #? alt Synthetic                            Nil
    :> attr "Signature"                            #? alt Signature                            (con2 :> Nil)
    :> attr "SourceFile"                           #? alt SourceFile                           (con2 :> Nil)
    :> attr "SourceDebugExtension"                 #? alt SourceDebugExtension                 (remainingByteString :> Nil)
    :> attr "LineNumberTable"                      #? alt LineNumberTable                      (gvector word16be (pair word16be word16be) :> Nil)
    :> attr "LocalVariableTable"                   #? alt LocalVariableTable                   (gvector word16be localVariable :> Nil)
    :> attr "LocalVariableTypeTable"               #? alt LocalVariableTypeTable               (gvector word16be localVariable :> Nil)
    :> attr "Deprecated"                           #? alt Deprecated                           Nil
    :> attr "RuntimeVisibleAnnotations"            #? alt RuntimeVisibleAnnotations            (gvector word16be annotation :> Nil)
    :> attr "RuntimeInvisibleAnnotations"          #? alt RuntimeInvisibleAnnotations          (gvector word16be annotation :> Nil)
    :> attr "RuntimeVisibleParameterAnnotations"   #? alt RuntimeVisibleParameterAnnotations   (gvector word8 (gvector word16be annotation) :> Nil)
    :> attr "RuntimeInvisibleParameterAnnotations" #? alt RuntimeInvisibleParameterAnnotations (gvector word8 (gvector word16be annotation) :> Nil)
    :> attr "AnnotationDefault"                    #? alt AnnotationDefault                    (value :> Nil)
    :> attr "BootstrapMethods"                     #? alt BootstrapMethods                     Nil
    :> attr "Scala"                                #? alt Custom                               (remainingByteString :> Nil) -- TODO: add catchall
    :> Nil  
  
  (#?) :: Maybe Con2 -> Partial Put Get Attribute ts -> Indexed Con2 (Partial Put Get Attribute) ts
  Nothing #? a = Con2 0 # Partial (fail "failed") (error "failed") (error "failed")
  Just c  #? a = c # a

  attr :: BL.ByteString -> Maybe Con2
  attr = traceShow tagMap $ fmap Con2 . flip M.lookup tagMap

  tagMap :: M.Map BL.ByteString Word16
  tagMap = M.fromList . map (fmap fromIntegral . swap) . mapMaybe (_2 (^? _Utf8) $) . IM.toList $ cpool

-- attribute = undefined

entity        attribute = untagged elim_Entity        . alt Entity        $ word16be :> con2 :> con2 :> gvector word16be attribute :> Nil
codeAttribute attribute = untagged elim_CodeAttribute . alt CodeAttribute $ word16be :> word16be :> byteString word32be :> gvector word16be exception :> gvector word16be attribute :> Nil


referenceKind = tagged word8 (EliminatorWrapper . elim_ReferenceKind)
  $  1 # alt Ref_getField         Nil
  :> 2 # alt Ref_getStatic        Nil
  :> 3 # alt Ref_putField         Nil
  :> 4 # alt Ref_putStatic        Nil
  :> 5 # alt Ref_invokeVirtual    Nil
  :> 6 # alt Ref_invokeStatic     Nil
  :> 7 # alt Ref_invokeSpecial    Nil
  :> 8 # alt Ref_newInvokeSpecial Nil
  :> 9 # alt Ref_invokeInterface  Nil
  :> Nil

constant = tagged word8 (EliminatorWrapper . elim_Constant)
  $   1 # alt Utf8               (byteString word16be :> Nil)
  :>  3 # alt Integer            (word32be :> Nil)
  :>  4 # alt Float              (float :> Nil)
  :>  5 # alt Long               (word64be :> Nil)
  :>  6 # alt Double             (double :> Nil)
  :>  7 # alt ClassName          (con2 :> Nil)
  :>  8 # alt String             (con2 :> Nil)
  :>  9 # alt FieldRef           (con2 :> con2 :> Nil)
  :> 10 # alt MethodRef          (con2 :> con2 :> Nil)
  :> 11 # alt InterfaceMethodRef (con2 :> con2 :> Nil)
  :> 12 # alt NameAndType        (con2 :> con2 :> Nil)
  :> 15 # alt MethodHandle       (referenceKind :> con2 :> Nil)
  :> 16 # alt MethodType         (con2 :> Nil)
  :> 18 # alt InvokeDynamic      (word16be :> con2 :> Nil)
  :> Nil

constantPool :: Prickler (IM.IntMap Constant)
constantPool = Prickler getter putter
  where
  listGetter :: Int -> Int -> Get [(Int, Constant)]
  listGetter 0 off = pure []
  listGetter n off = do
    x   <- get constant
    let delta = size x
    xs  <- listGetter (n - delta) (off + delta)
    return ((off, x) : xs)

  getter    = do len <- getWord16be; IM.fromList <$> listGetter (fromIntegral len - 1) 1
  putter xs = put word16be (fromIntegral . sum $ map (size . snd) flat) <> mapM_ (put constant . snd) flat
    where flat = IM.toAscList xs

  size :: Constant -> Int
  size (Long   _) = 2
  size (Double _) = 2
  size _ = 1

basicClass :: Prickler Class
basicClass = Prickler getter putter
  where
  getter = do
    maj   <- get word16be
    min   <- get word16be
    cons  <- get constantPool -- this should really be at least partially synthetic...
    flags <- get word16be
    cname <- get con2
    sname <- get con2
    ifs   <- get $ gvector word16be con2
    let attr = attribute cons
        ents = gvector word16be (entity attr)
    fs    <- get $ ents
    ms    <- get $ ents
    attrs <- get $ gvector word16be attr
    return $ Class maj min cons flags cname sname ifs fs ms attrs

  putter (Class maj min cons flags cname sname ifs fs ms attrs) = put word16be maj <> put word16be min <> put constantPool cons <> put word16be flags <> put con2 cname <> put con2 sname <> put (gvector word16be con2) ifs <> put ents fs <> put ents ms <> put (gvector word16be attr) attrs
    where
    attr = attribute cons
    ents = gvector word16be (entity attr)


klass :: Prickler Class
klass = skip (expect 0xcafebabe word32be) basicClass

test = do
  x <- BL.readFile "/Users/copumpkin/Sandbox/Haskell/java/test/Test.class"
  putStrLn (ppShow $ runGet (get klass) x)


