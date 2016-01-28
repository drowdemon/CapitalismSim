module LangData
       where
import Util
import Data.Tree
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad

data Operator = Add
              | Subt
              | Div
              | Mul
              | Lt
              | Gt
              | Eq
              | Not
              | And
              | Or
              | If
              | Dfn
              | Call --takes a FuncId (function id, like func pointer) - id number of the function, and all the arguments to that function
              | Map  --also takes an fid, along with a list to map over
              | Fold  
              | MkList   
              | ConsList
              | AppLists
              | Ceil
              | Floor
              | ToDouble
              deriving (Show,Eq,Ord,Bounded,Enum)

data Datum = DatB Bool --takes a monad - the list monad or the identity. I think that works
           | DatI Integer
           | DatD Double
           | FuncArg Int --the integer is the 'name' of the argument. Types are inferred.
           | FuncId Int  --Must be compile time constant! --integer is 'name' of function
           | ListDat SpecificType [Datum]
           deriving (Show, Eq)

--Curently, GenVar must be lexicographically first. Ugly, but true. See execState call in getFuncDescFromNode in LangProc.hs
data GenericType = GenVar       --anything
                 | NumVar       --numeric: double or int
                 deriving (Show, Ord, Eq, Enum, Bounded)
                          
data SpecificType = DatBVar --no associated ints
                  | DatIVar
                  | DatDVar --note that since only this type can be made into a list, lists of functions are illegal (the type could be written, but the data can't be. I'm going to regret that.)
--                  | FuncIdVar--currently this one has to be lexicographically last, but that might be fixed - see instantiateType in LangProc.hs
                  deriving (Show, Enum, Bounded, Ord, Eq)

--data ContainerType = ListT TypeVar
               --  | Tree or Maybe or whatever

data TypeVar = GenType (GenericType, Int) --integer is an id, like type t0, t1
             | SpecType SpecificType
             | ListType TypeVar
          -- | ContType ListT  --replace listT
             | ExprType FuncType
             deriving (Show, Ord, Eq)
data FuncType = FuncType
  { retType :: TypeVar
  , argType :: Map.Map Int TypeVar
  } deriving (Show, Eq, Ord)
data FuncDesc = FuncDesc
  { body    :: Expression
  , fType   :: FuncType
  }
  deriving (Show,Eq)
  
--newtype Expression = Expr (Tree (Either OpDesc Datum)) deriving (Show, Eq)
type Expression = (Tree (Either Operator Datum))
type ExprNode = Either Operator Datum
type OpDesc = (Operator, FuncType)

{-class SpecificT a where
  convToDat :: a -> Datum

instance SpecificT SpecificType where
 convToDat DatBVar = (DatB False)
 convToDat DatIVar = (DatI 0)
 convToDat DatDVar = (DatD 0.0)
instance (SpecificT a) => SpecificT (ListType a) where-}
  

--if the arguments are equivalently specific it will return the Just the first one.
getMoreSpecific :: TypeVar -> TypeVar -> Maybe TypeVar
getMoreSpecific (ExprType _) _                               = Nothing --note that this implies that genvars do not generalize to functional types, which is ok - no lists of functions
getMoreSpecific _ (ExprType _)                               = Nothing
getMoreSpecific (SpecType t1) (SpecType t2)                  = if t1==t2 then Just $ SpecType t1 else Nothing
getMoreSpecific (SpecType t1) (GenType (GenVar, _))          = Just $ SpecType t1
getMoreSpecific (SpecType DatIVar) (GenType (NumVar, _))     = Just $ SpecType DatIVar
getMoreSpecific (SpecType DatDVar) (GenType (NumVar, _))     = Just $ SpecType DatDVar
getMoreSpecific (GenType (NumVar, t1)) (GenType (NumVar, _)) = Just $ GenType (NumVar, t1)
getMoreSpecific (ListType t1) (ListType t2)                  = fmap ListType $ getMoreSpecific t1 t2
getMoreSpecific t1 (GenType (GenVar, _))                     = Just t1
getMoreSpecific (GenType (GenVar, _)) t1                     = Just t1
getMoreSpecific (ListType _) (GenType (NumVar, _))           = Nothing
getMoreSpecific (GenType (NumVar, _)) (ListType _)           = Nothing
getMoreSpecific (SpecType _) _                               = Nothing
getMoreSpecific t2 (SpecType t1)                             = getMoreSpecific (SpecType t1) t2

datToType :: Datum -> SpecificType
datToType (DatB _) = DatBVar
datToType (DatI _) = DatIVar
datToType (DatD _) = DatDVar
--datToType (FuncId _) = FuncIdVar
datToType (ListDat spec _) = spec
--datToType d = error $ show d

genTypeIntBijection :: TypeVar -> Int
genTypeIntBijection t = bijection' 0 t
  where bijection' numList t0 =
          case t0 of
            (GenType (genT, genId)) -> bijection'' numList $ (fromEnum (maxBound::GenericType)+1) * genId + (fromEnum genT) + (fromEnum (maxBound::SpecificType)) + 1 --last 2 terms give room for the specific types at the beginning
            (SpecType x) -> bijection'' numList $ fromEnum x --made room in the positives for these
            (ListType t1) -> bijection' (numList+1) t1
            (ExprType _) -> -1 --simple. 
        bijection'' numList numEnum = ((numList+numEnum)*(numList+numEnum+1) `div` 2) + numEnum

genTypeIntBijectionInv :: Int -> TypeVar
genTypeIntBijectionInv (-1) = ExprType undefined 
genTypeIntBijectionInv z  =
  let w = floor $ ((sqrt $ (fromIntegral $ 8*z + 1)::Double) - 1) / 2
      t = (w*w + w) `div` 2
      numEnum = z - t
      numList = w - numEnum
  in getType' numList numEnum
  where getInnerType' :: Int -> TypeVar
        getInnerType' x | x > fromEnum (maxBound::SpecificType) =
                          GenType $ let (d, m) = divMod (x-fromEnum (maxBound::SpecificType)-1) $ 1+fromEnum (maxBound::GenericType) in
                                 (toEnum m, d)
                        | otherwise = SpecType $ toEnum x
        getType' :: Int -> Int -> TypeVar
        getType' 0 numEnum = getInnerType' numEnum
        getType' numList numEnum = ListType $ getType' (numList-1) numEnum

setListInnerDat :: TypeVar -> Datum -> Datum
setListInnerDat (ListType t) d@(ListDat s _) = setListInnerDat t $ ListDat s [d]
setListInnerDat (ListType t) d = setListInnerDat t $ ListDat (datToType d) [d] --error here with funcarg
setListInnerDat _ d = d

getListInnerType :: TypeVar -> TypeVar
getListInnerType (ListType t) = getListInnerType t
getListInnerType t = t

setActListInnerType :: TypeVar -> TypeVar -> TypeVar
setActListInnerType (ListType lst) t = ListType $ setActListInnerType lst t
setActListInnerType _ t = t

getListOneLayerType :: TypeVar -> TypeVar
getListOneLayerType (ListType t) = t
getListOneLayerType t = t

getShallowestInnerType :: TypeVar -> TypeVar -> (TypeVar, TypeVar, Bool)
getShallowestInnerType (ListType t1) (ListType t2) = getShallowestInnerType t1 t2
getShallowestInnerType (ListType t1) t2 = (ListType t1, t2, True)
getShallowestInnerType t1 (ListType t2) = (t1, ListType t2, False)
getShallowestInnerType t1 t2 = (t1, t2, False) --bool doesn't matter in this case

replaceT :: TypeVar -> TypeVar -> TypeVar -> TypeVar
replaceT orig spec t --replace t with spec iff t==orig, i.e. s/orig/spec; t is search term
           | t==orig   = spec
           | otherwise = case t of
                           ListType t1 -> ListType $ replaceT orig spec t1
                           t1 -> t1 --no replacement needed

canSpecifyFTo :: Bool -> Bool -> FuncType -> FuncType -> Bool
canSpecifyFTo useArgs requireSpecificity f1 f2 =
  isJust $ do
    guard $ (Map.size . argType $ f1) /= (Map.size . argType $ f2)
    let f' (t1,t2) = do
         temp <- getMoreSpecific t1 t2
         when requireSpecificity $ guard (temp==t1) --same or f1 is more specific. If it's f2, no good, but test only when requireSepcificity is true.
    f' (retType f1,retType f2)
    if useArgs then
       mapM f' $ zip (Map.elems $ argType f1) (Map.elems $ argType f2) --mapM fails when any one is wrong
       else Just []

compSameTListT :: TypeVar -> TypeVar -> DupsEqWhich
compSameTListT (ListType t1) (ListType t2) = compSameTListT t1 t2
compSameTListT (ListType t1) t2 | getListInnerType t1 == t2 = DupR
                                | otherwise                 = DupNEQ
compSameTListT t1 (ListType t2) | getListInnerType t2 == t1 = DupL
                                | otherwise                 = DupNEQ
--I don't know which, if any of the cases L or R is faster. I picked R, since it doesn't change the list
compSameTListT t1 t2 | t1==t2    = DupR
                     | otherwise = DupNEQ
