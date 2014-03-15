{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-} 
module Data.Prickler where

import Control.Applicative

import Data.Maybe
import Data.Monoid hiding (Sum, Product, All)
import Data.Binary.Get hiding (Done)
import Data.Binary.Builder
import qualified Data.IntMap as IM
import Data.ByteString.Base16
import qualified Data.ByteString.Lazy as BL

import Data.Int
import Data.Word

newtype (:$:) f a = F { unF :: f a }

newtype K a b = K { unK :: a }
data (:*:) f g x = f x :*: g x
data Exists f = forall a. Exists { getValue :: f a }

newtype Flip  r t  = Flip  { runFlip  :: t   -> r }
newtype NFlip r ts = NFlip { runNFlip :: ts @-> r }

type family (@->) (ts :: [*]) (r :: *) :: *
type instance '[] @-> r = r
type instance (t ': ts) @-> r = t -> (ts @-> r)

type family Eliminator (css :: [[*]]) (r :: *) :: *
type instance Eliminator '[] r = r
type instance Eliminator (cs ': css) r = (cs @-> r) -> Eliminator css r



infixr 1 :>

data All f (ts :: [a]) where
  Nil  :: All f '[]
  (:>) :: f x -> All f xs -> All f (x ': xs)

mapAll :: (forall a. f a -> g a) -> All f ts -> All g ts
mapAll f Nil = Nil
mapAll f (a :> as) = f a :> mapAll f as

mapAllF :: (forall a. f a -> b) -> All f ts -> [b]
mapAllF f Nil = []
mapAllF f (a :> as) = f a : mapAllF f as

foldlAll :: (a -> b -> a) -> a -> All (Flip b) ts -> (ts @-> a)
foldlAll f z Nil = z
foldlAll f z (Flip g :> gs) = \x -> foldlAll f (f z (g x)) gs

zipWithAll :: (forall a. f a -> g a -> h a) -> All f ts -> All g ts -> All h ts
zipWithAll f Nil Nil = Nil
zipWithAll f (x :> xs) (y :> ys) = f x y :> zipWithAll f xs ys

-- data Partial g b ts a = Partial { build :: g a, break :: ts @->  }
-- incremental All? When building, we know the argument and can compute the rest, when reading, we know the value and can compute the rest

data Case f a ts = Case { cons :: ts @-> a, shape :: All f ts }
data Data f g a = forall ts. Data { elim :: forall r. a -> EliminatorWrapper ts r, sum :: All (g (Case f a)) ts}

newtype EliminatorWrapper ts r = EliminatorWrapper { getEliminator :: Eliminator ts r }


-- All glory to glguy
apN :: Applicative f => f (ts @-> r) -> All f ts -> f r
apN f Nil = f
apN f (x :> xs) = apN (f <*> x) xs

liftAn :: Applicative f => (ts @-> r) -> All f ts -> f r
liftAn = apN . pure


merge :: All (NFlip r) ts -> Eliminator ts r -> r
merge Nil acc = acc
merge (NFlip x :> xs) acc = merge xs (acc x)


eliminate :: Monoid m => All (K m :*: All (Flip m)) ts -> EliminatorWrapper ts m -> m
eliminate xs = merge (mapAll (\(K i :*: shape) -> NFlip (foldlAll (<>) i shape)) xs) . getEliminator

















data Prickler a = Prickler { get :: Get a, put :: a -> Builder } -- laws: get . put . get == get, put . get . put == put

expect :: Eq a => a -> Prickler a -> Prickler ()
expect x (Prickler ga pa) = Prickler (do y <- ga; if x == y then return () else fail "Expectation failed") (const (pa x))

ignore :: a -> Prickler a -> Prickler ()
ignore x (Prickler ga pa) = Prickler (() <$ ga) (const (pa x))

skip :: Prickler () -> Prickler a -> Prickler a
skip (Prickler gu pu) (Prickler ga pa) = Prickler (gu *> ga) ((pu () <>) . pa)

wrap :: (a -> b) -> (b -> a) -> Prickler a -> Prickler b
wrap f g (Prickler ga pa) = Prickler (f <$> ga) (pa . g)

pair :: Prickler a -> Prickler b -> Prickler (a, b)
pair (Prickler ga pa) (Prickler gb pb) = Prickler (liftA2 (,) ga gb) (\(x, y) -> pa x <> pb y)

type Contiguous = (:$:)
type Indexed i = (:*:) (K i)

contiguousData :: Integral i => Prickler i -> Data Prickler Contiguous a -> Prickler a
contiguousData = undefined

taggedData :: Integral i => Prickler i -> Data Prickler (Indexed i) a -> Prickler a
taggedData (Prickler gi pi) (Data elim sum) = Prickler getter (eliminate (mapAll (adjust pi) sum) . elim)
  where
  adjust :: (i -> Builder) -> (K i :*: Case Prickler a) ts -> (K Builder :*: All (Flip Builder)) ts
  adjust pi (K i :*: Case _ shape) = K (pi i) :*: mapAll (Flip . put) shape

  ps = IM.fromList (mapAllF (\(K x :*: y) -> (fromIntegral x, Exists y)) sum)

  getter = do
    tag <- gi
    case IM.lookup (fromIntegral tag) ps of
      Nothing                         -> fail $ "Invalid tag: " ++ show (fromIntegral tag :: Integer)
      Just (Exists (Case cons shape)) -> liftAn cons (mapAll get shape)


(#) :: a -> g x -> (K a :*: g) x
i # x = K i :*: x
