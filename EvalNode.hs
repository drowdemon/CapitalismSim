module EvalNode
       where
--import InternalProg
import Data.Tree
import qualified Data.Map.Strict as StrMap
import BasicData
import Control.Monad.Trans.State
--import Control.Monad
import qualified Data.IntDisjointSet as DisjointSet
import qualified Data.Traversable as Traversable

evalNode :: Expression -> State (StrMap.Map Int FuncDesc) Datum
evalNode (Node (Right dat) _) = return dat

evalNode (Node (Left Add) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  case (a1,a2) of
    (DatD a, DatD b) -> return $ DatD (a+b)
    (DatI a, DatI b) -> return $ DatI (a+b)

evalNode (Node (Left Subt) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  case (a1,a2) of
    (DatD a, DatD b) -> return $ DatD (a-b)
    (DatI a, DatI b) -> return $ DatI (a-b)

evalNode (Node (Left Div) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  case (a1,a2) of
    (DatD a, DatD b) -> return $ DatD (a/b)
    (DatI a, DatI b) -> return $ DatI (a `div` b)

evalNode (Node (Left Mul) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  case (a1,a2) of
    (DatD a, DatD b) -> return $ DatD (a*b)
    (DatI a, DatI b) -> return $ DatI (a*b)

evalNode (Node (Left Lt) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  case (a1,a2) of
    (DatD a, DatD b) -> return $ DatB (a<b)
    (DatI a, DatI b) -> return $ DatB (a<b)

evalNode (Node (Left Gt) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  case (a1,a2) of
    (DatD a, DatD b) -> return $ DatB (a>b)
    (DatI a, DatI b) -> return $ DatB (a>b)

evalNode (Node (Left Eq) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  case (a1,a2) of
    (DatD a, DatD b) -> return $ DatB (a==b)
    (DatI a, DatI b) -> return $ DatB (a==b)

evalNode (Node (Left Not) (arg1:[])) = do
  DatB a1 <- evalNode arg1
  return $ DatB $ not a1

evalNode (Node (Left And) (arg1:arg2:[])) = do
  DatB a1 <- evalNode arg1
  DatB a2 <- evalNode arg2
  return $ DatB $ a1 && a2      

evalNode (Node (Left Or) (arg1:arg2:[])) = do
  DatB a1 <- evalNode arg1
  DatB a2 <- evalNode arg2
  return $ DatB $ a1 || a2

evalNode (Node (Left If) (arg1:arg2:arg3:[])) = do
  DatB a1 <- evalNode arg1
  a2 <- evalNode arg2
  a3 <- evalNode arg3
  return $ if a1 then a2 else a3

evalNode (Node (Left Dfn) (arg1:[])) = do
  let (currTypes,currArgs) = addFunc (GenType (GenVar, 0)) arg1 (DisjointSet.singleton $ genTypeIntBijection (GenType (GenVar, 0))) StrMap.empty
      specTypes    = getSpecificest currTypes
      specArgs     = StrMap.mapMaybe id $ fst $ runState (Traversable.mapM findSpecificType currArgs) (currTypes, specTypes) --mapping id is an awful hack, runs an O(n) operation an extra time...
      Just specRet = fst $ runState (findSpecificType (GenType (GenVar, 0))) (currTypes, specTypes)
      retFunc      = FuncDesc{body=arg1, retType=specRet, argType=specArgs} --retrieve ret/arg types from currTypes
  addFunc' retFunc  
  return $ DatB True --TODO?
  where addFunc' :: FuncDesc -> State (StrMap.Map Int FuncDesc) ()
        addFunc' addF =
          state $ \fTable -> ((), StrMap.insert (StrMap.size fTable) addF fTable)
        getSpecificest :: DisjointSet.IntDisjointSet -> StrMap.Map TypeVar TypeVar
        getSpecificest currTypes =
          getSpecificest' (map (\(x,y) -> (genTypeIntBijectionInv x, genTypeIntBijectionInv y)) $
                               fst $ DisjointSet.toList currTypes) StrMap.empty
          where
            --associate representative with min element in set
            getSpecificest' :: [(TypeVar, TypeVar)] -> StrMap.Map TypeVar TypeVar -> StrMap.Map TypeVar TypeVar --state is best seen typevar
            getSpecificest' [] soFar =
              soFar
            getSpecificest' ((tp, rep):rst) soFar =
              case StrMap.lookup rep soFar of
                Just bestSoFar -> getSpecificest' rst $ StrMap.insert rep (getMoreSpecific bestSoFar tp) soFar
                Nothing        -> getSpecificest' rst $ StrMap.insert rep tp soFar
        findSpecificType :: TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map TypeVar TypeVar) (Maybe TypeVar)
        findSpecificType tp = do --looks up in specTypes. I need to look up in currTypes, and then look that up in specTypes
          (curr, spec) <- get
          return $ do
            let (resM,_) = DisjointSet.lookup (genTypeIntBijection tp) curr
            res <- resM
            StrMap.lookup (genTypeIntBijectionInv res) spec

--TODO
evalNode n@(Node (Left Call) (fID:args)) =
  evalFunc n

evalNode (Node (Left MkList) (arg1:[])) = do
  a1 <- evalNode arg1
  return $ case a1 of
       DatB a -> ListB [a]
       DatI a -> ListI [a]
       DatD a -> ListD [a]

evalNode (Node (Left ConsList) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  return $ case (a1,a2) of
       (DatB a, ListB b) -> ListB $ a:b
       (DatI a, ListI b) -> ListI $ a:b
       (DatD a, ListD b) -> ListD $ a:b

evalNode (Node (Left AppLists) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  return $ case (a1,a2) of
       (ListB a, ListB b) -> ListB $ a++b
       (ListI a, ListI b) -> ListI $ a++b
       (ListD a, ListD b) -> ListD $ a++b

--TODO
evalFunc :: Expression -> State (StrMap.Map Int FuncDesc) Datum
--evalFunc (Node (Left Dfn) args) =
evalFunc _ = 
  return $ DatB True

genTypeIntBijection :: TypeVar -> Int
genTypeIntBijection (GenType (genT, genId)) = fromEnum (maxBound::GenericType) * genId + (fromEnum genT)
genTypeIntBijection (SpecType x) = -(fromEnum x)

--genTypeIntBijectionArg :: Int -> Int
--genTypeIntBijectionArg :: genId = ((maxBound::GenericType)+1) * genId + (maxBound::GenericType) --arg is a funcarg

genTypeIntBijectionInv :: Int -> TypeVar
genTypeIntBijectionInv x | x>=0      = GenType  $
                                       let (d, m) = divMod x $ fromEnum (maxBound::GenericType) in
                                         (toEnum m, d)
                         | otherwise = SpecType $ toEnum (-x)

getMoreSpecific :: TypeVar -> TypeVar -> TypeVar -- assuming correctness
getMoreSpecific (SpecType t1) _                                    = SpecType t1
getMoreSpecific _ (SpecType t1)                                    = SpecType t1
getMoreSpecific (GenType (ListVar, t1)) (GenType (ListVar, _))     = GenType (ListVar, t1)
getMoreSpecific (GenType (ListVar, t1)) (GenType (ListNumVar, _))  = GenType (ListNumVar, t1)
getMoreSpecific (GenType (ListNumVar, t1)) (GenType (ListVar, _))  = GenType (ListNumVar, t1)
getMoreSpecific (GenType (SingletonVar, t1)) (GenType (NumVar, _)) = GenType (NumVar, t1)
getMoreSpecific (GenType (NumVar, t1)) (GenType (SingletonVar, _)) = GenType (NumVar, t1)
getMoreSpecific (GenType (GenVar, _)) (GenType t1)                 = GenType t1
getMoreSpecific (GenType t1) (GenType (GenVar, _))                 = GenType t1
                                       
--passed in typevars are types of arguments known so far, in order
addFunc :: TypeVar -> Expression -> DisjointSet.IntDisjointSet -> StrMap.Map Int TypeVar -> (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar)
addFunc currT (Node (Right dat) []) currTypes currArgs =
  case dat of
    FuncArg argId ->
      case argId `StrMap.lookup` currArgs of --already has a type
          Just argTp -> (DisjointSet.union (genTypeIntBijection currT) (genTypeIntBijection argTp) currTypes, currArgs) --They must be compatible, so no check
          Nothing    -> (currTypes, StrMap.insert argId currT currArgs) --now the argument argId has type currT
    d             -> (DisjointSet.union (genTypeIntBijection currT) (genTypeIntBijection $ SpecType $ datToType d) currTypes, currArgs)

--addFunc (currT:[]) (Node (Left Add) (arg1:arg2:[])) currTypes currArgs =
--  if getMoreSpecific (NumVar 0) currT == (NumVar 0) then
    --union currT with numVar 0
    --else do nothing
  --let (currTypes', currArgs') = addFunc 
  
{-getSpecificest' :: DisjointSet.IntDisjointSet -> StrMap.Map TypeVar TypeVar
getSpecificest' currTypes =
  getSpecificest'' (map (\(x,y) -> (genTypeIntBijectionInv x, genTypeIntBijectionInv y)) $
                       fst $ DisjointSet.toList currTypes) StrMap.empty
  where
    --associate representative with min element in set
    getSpecificest'' :: [(TypeVar, TypeVar)] -> StrMap.Map TypeVar TypeVar -> StrMap.Map TypeVar TypeVar --state is best seen typevar
    getSpecificest'' [] soFar =
      soFar
    getSpecificest'' ((tp, rep):rst) soFar =
      case StrMap.lookup rep soFar of
        Just bestSoFar -> getSpecificest'' rst $ StrMap.insert rep (getMoreSpecific bestSoFar tp) soFar
        Nothing        -> getSpecificest'' rst $ StrMap.insert rep tp soFar

findSpecificType' :: TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map TypeVar TypeVar) (Maybe TypeVar)
findSpecificType' tp = do --looks up in specTypes. I need to look up in currTypes, and then look that up in specTypes
  (curr, spec) <- get
  return $ do
    let (resM,_) = DisjointSet.lookup (genTypeIntBijection tp) curr
    res <- resM
    StrMap.lookup (genTypeIntBijectionInv res) spec-}
