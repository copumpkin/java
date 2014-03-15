module Data.Eliminator.TH where

import Language.Haskell.TH
import Control.Monad

-- This is by glguy

conFields :: Con -> [Type]
conFields (NormalC _ xs) = [ t | (  _,t) <- xs ]
conFields (RecC    _ xs) = [ t | (_,_,t) <- xs ]
conFields ForallC{}      = error "It's going to take more work to support universal quantification"

conName :: Con -> Name
conName (NormalC n xs) = n
conName (RecC    n xs) = n
conName (ForallC _ _ c) = conName c

mkElimClause :: [Con] -> ClauseQ
mkElimClause cons =
  do let n = length cons
     x  <- newName "x"
     fs <- replicateM n (newName "f")
     let matches = zipWith mkMatch fs cons
     clause (map varP (x : fs)) (normalB (caseE (varE x) matches)) []
  where
  mkMatch f con =
    do let m = length (conFields con)
       ys <- replicateM m (newName "y")
       match (conName con `conP` map varP ys)
             (normalB (appsE (varE f : map varE ys)))
             []

appsT :: Type -> [Type] -> Type
appsT = foldl AppT

(@->) :: [Type] -> Type -> Type
(@->) xs r = foldr (\x y -> ArrowT `appsT` [x,y]) r xs

bndrVarType :: TyVarBndr -> Type
bndrVarType (PlainTV  x  ) = VarT x
bndrVarType (KindedTV x _) = VarT x

mkElimType :: Cxt -> Name -> [TyVarBndr] -> [Con] -> TypeQ
mkElimType c tyName bndrs cons
  = do r <- newName "r"

       let appliedType  = ConT tyName `appsT` map bndrVarType bndrs
           finalType    = VarT r
           elimConT con = conFields con                     @-> finalType
           elimType     = (appliedType : map elimConT cons) @-> finalType
           bndrs'       = PlainTV r : bndrs

       return (ForallT bndrs' c elimType)

mkElim :: Name -> DecsQ
mkElim tyName =
  do let funName = mkName ("elim_" ++ nameBase tyName)
     info <- reify tyName
     (c, tyBndrs, cons) <- case info of
       TyConI (DataD    c _ tyBndrs cons _) -> return (c, tyBndrs, cons)
       TyConI (NewtypeD c _ tyBndrs con  _) -> return (c, tyBndrs, [con])
       _ -> fail "mkElim: Name of data or newtype required"
     sequence
       [ sigD funName (mkElimType c tyName tyBndrs cons)
       , funD funName [mkElimClause cons]
       ]