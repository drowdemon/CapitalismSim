module LangData
       where
import Data.Tree
import qualified Data.Map.Strict as Map

data Operator = Add --everything done has been tested in addNode, not in evalNode
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
              deriving (Show,Eq)

data Datum = DatB Bool --takes a monad - the list monad or the identity. I think that works
           | DatI Integer
           | DatD Double
           | FuncArg Int --the integer is the 'name' of the argument. Types are inferred.
           | FuncId Int  --Must be compile time constant! --integer is 'name' of function
           | ListDat SpecificType [Datum]
           deriving (Show, Eq)

--Curently, GenVar must be lexicographically first. Ugly, but true.
data GenericType = GenVar       --anything
                 | NumVar       --numeric: double or int
                 deriving (Show, Ord, Eq, Enum, Bounded)
                          
data SpecificType = DatBVar --no associated ints
                  | DatIVar
                  | DatDVar
                  | FuncIdVar
                  deriving (Show, Enum, Bounded, Ord, Eq)
                           
data TypeVar = GenType (GenericType, Int) --integer is an id, like type t0, t1
             | SpecType SpecificType
             | ListType TypeVar
             deriving (Show, Ord, Eq)

data FuncDesc = FuncDesc
  { body    :: Expression
  , retType :: TypeVar
  , argType :: Map.Map Int TypeVar
  }
  deriving (Show,Eq)
  
type Expression = (Tree (Either Operator Datum))

getMoreSpecific :: TypeVar -> TypeVar -> TypeVar -- assuming correctness
getMoreSpecific (SpecType t1) _                              = SpecType t1
getMoreSpecific _ (SpecType t1)                              = SpecType t1
getMoreSpecific (GenType (NumVar, t1)) (GenType (NumVar, _)) = GenType (NumVar, t1)
getMoreSpecific (ListType t1) (ListType t2)                  = ListType $ getMoreSpecific t1 t2
getMoreSpecific t1 (GenType (GenVar, _))                     = t1
getMoreSpecific (GenType (GenVar, _)) t1                     = t1

datToType :: Datum -> SpecificType
datToType (DatB _) = DatBVar
datToType (DatI _) = DatIVar
datToType (DatD _) = DatDVar
datToType (FuncId _) = FuncIdVar
--datToType (ListDat spec [d]) = ListType (datToType d)

genTypeIntBijection :: TypeVar -> Int
genTypeIntBijection t = bijection' 0 t
  where bijection' numList t0 =
          case t0 of
            (GenType (genT, genId)) -> bijection'' numList $ (fromEnum (maxBound::GenericType)+1) * genId + (fromEnum genT) + (fromEnum (maxBound::SpecificType)) + 1 --last 2 terms give room for the specific types at the beginning
            (SpecType x) -> bijection'' numList $ fromEnum x --made room in the positives for these
            (ListType t1) -> bijection' (numList+1) t1
        bijection'' numList numEnum = ((numList+numEnum)*(numList+numEnum+1) `div` 2) + numEnum

genTypeIntBijectionInv :: Int -> TypeVar
genTypeIntBijectionInv z =
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

getListInnerType :: TypeVar -> TypeVar
getListInnerType (ListType t) = getListInnerType t
getListInnerType t = t

setActListInnerType :: TypeVar -> TypeVar -> TypeVar
setActListInnerType (ListType lst) t = ListType $ setActListInnerType lst t
setActListInnerType _ t = t

getListOneLayerType :: TypeVar -> TypeVar
getListOneLayerType t = 
  case t of --it wants a return type of list, not anything else
         ListType t1            -> t1
         t1                     -> t1
         --(GenType (GenVar, t1)) -> GenType (GenVar, t1)

getShallowestInnerType :: TypeVar -> TypeVar -> (TypeVar, TypeVar, Bool)
getShallowestInnerType (ListType t1) (ListType t2) = getShallowestInnerType t1 t2
getShallowestInnerType (ListType t1) t2 = (ListType t1, t2, True)
getShallowestInnerType t1 (ListType t2) = (t1, ListType t2, False)
getShallowestInnerType t1 t2 = (t1, t2, False) --bool doesn't matter in this case