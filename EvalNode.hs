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
import qualified Data.Sequence as Sq
--import qualified Debug.Trace as Tr

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
  return $ DatB (a1==a2)
  --case (a1,a2) of
  --  (DatD a, DatD b) -> return $ DatB (a==b)
  --  (DatI a, DatI b) -> return $ DatB (a==b)

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
  let typesInit = (genTypeIntBijection (GenType (GenVar, 0))):[genTypeIntBijection $ SpecType (minBound::SpecificType)..genTypeIntBijection $ SpecType (maxBound::SpecificType)]
      (currTypes,currArgs,_) = snd $ runState (addFunc (GenType (GenVar, 0)) arg1)
                                               (DisjointSet.fromList $ zip typesInit typesInit,
                                               --((DisjointSet.singleton $ genTypeIntBijection (GenType (GenVar, 0))), --need to put all SpecVars in there, or let those be placed as needed
                                               StrMap.empty,
                                               (1 Sq.<|(Sq.replicate (fromEnum (maxBound::GenericType)) 0)))
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
       DatB a -> ListDat DatBVar [DatB a]
       DatI a -> ListDat DatIVar [DatI a]
       DatD a -> ListDat DatDVar [DatD a]
       ListDat t a -> ListDat t $ [ListDat t a]

evalNode (Node (Left ConsList) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  return $ case (a1,a2) of
       (DatB a, ListDat DatBVar b) -> ListDat DatBVar $ (DatB a):b
       (DatI a, ListDat DatIVar b) -> ListDat DatIVar $ (DatI a):b
       (DatD a, ListDat DatDVar b) -> ListDat DatDVar $ (DatD a):b

evalNode (Node (Left AppLists) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  return $ case (a1,a2) of
       (ListDat t1 a, ListDat t2 b) -> ListDat t1 $ f t1 t2 a b --a++b
       --(ListDat a, ListDat b) -> ListDat $ a++b
       --(ListDat a, ListDat b) -> ListDat $ a++b
  where f t1 t2 a b | t1==t2 = a++b
--TODO
evalFunc :: Expression -> State (StrMap.Map Int FuncDesc) Datum
--evalFunc (Node (Left Dfn) args) =
evalFunc _ = 
  return $ DatB True

genTypeIntBijection :: TypeVar -> Int
genTypeIntBijection t = bijection' 0 t
  where bijection' numList t0 =
          case t0 of
            (GenType (genT, genId)) -> bijection'' numList $ fromEnum (maxBound::GenericType) * genId + (fromEnum genT) + (fromEnum (maxBound::SpecificType)) + 1 --last 2 terms give room for the specific types at the beginning
            (SpecType x) -> bijection'' numList $ fromEnum x --made room in the positives for these
            (ListType t1) -> bijection' (numList+1) t1
        bijection'' numList numEnum = ((numList+numEnum)*(numList+numEnum+1) `div` 2) + numEnum

--genTypeIntBijectionArg :: Int -> Int
--genTypeIntBijectionArg :: genId = ((maxBound::GenericType)+1) * genId + (maxBound::GenericType) --arg is a funcarg

{-genTypeIntBijectionInv :: Int -> TypeVar
genTypeIntBijectionInv x | x>=0      = GenType $
                                         let (d, m) = divMod x $ fromEnum (maxBound::GenericType) in
                                           (toEnum m, d)
                         | otherwise = SpecType $ toEnum $ (-x)-1 --0 is taken by the positives
-}
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

getMoreSpecific :: TypeVar -> TypeVar -> TypeVar -- assuming correctness
getMoreSpecific (SpecType t1) _                              = SpecType t1
getMoreSpecific _ (SpecType t1)                              = SpecType t1
getMoreSpecific (GenType (NumVar, t1)) (GenType (NumVar, _)) = GenType (NumVar, t1)
getMoreSpecific (ListType t1) (ListType t2)                  = ListType $ getMoreSpecific t1 t2
getMoreSpecific t1 (GenType (GenVar, _)) = t1
getMoreSpecific (GenType (GenVar, _)) t1 = t1
--getMoreSpecific (GenType (ListVar, t1)) (GenType (ListVar, _))           = GenType (ListVar, t1)
--getMoreSpecific (GenType (SingletonVar, t1)) (GenType (SingletonVar, _)) = GenType (SingletonVar, t1)
--getMoreSpecific (GenType (ListNumVar, t1)) (GenType (ListNumVar, _))     = GenType (ListNumVar, t1)
--getMoreSpecific (GenType (ListVar, t1)) (GenType (ListNumVar, _))        = GenType (ListNumVar, t1)
--getMoreSpecific (GenType (ListNumVar, t1)) (GenType (ListVar, _))        = GenType (ListNumVar, t1)
--getMoreSpecific (GenType (SingletonVar, t1)) (GenType (NumVar, _))       = GenType (NumVar, t1)
--getMoreSpecific (GenType (NumVar, t1)) (GenType (SingletonVar, _))       = GenType (NumVar, t1)

getListInnerType :: TypeVar -> TypeVar
getListInnerType (ListType t) = getListInnerType t
getListInnerType t = t

getListOneLayerType :: TypeVar -> TypeVar
getListOneLayerType t = 
  case t of --it wants a return type of list, not anything else
         ListType t1            -> t1
         (GenType (GenVar, t1)) -> GenType (GenVar, t1)
         
--passed in typevars are types of arguments known so far, in order
addFunc :: TypeVar -> Expression -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int) ()
addFunc currT (Node (Right dat) []) = do
  (currTypes, currArgs, validVals) <- get
  put $ case dat of
    FuncArg argId ->
      case argId `StrMap.lookup` currArgs of --already has a type
          Just argTp -> (DisjointSet.union (genTypeIntBijection currT) (genTypeIntBijection argTp) currTypes, currArgs, validVals) --They must be compatible, so no check
          Nothing    -> (currTypes, StrMap.insert argId currT currArgs, validVals) --now the argument argId has type currT
    d             -> (DisjointSet.union (genTypeIntBijection currT) (genTypeIntBijection $ SpecType $ datToType d) currTypes, currArgs, validVals)

addFunc currT (Node (Left Add) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Subt) (arg1:arg2:[])) = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Div) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Mul) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Lt) (arg1:arg2:[]))   = do
  --specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))  --this is used incorrectly. It will specify that it's *arguments* must be numvars, which it does, but it also transforms them into DatBVars - currently when the given return value in currT is DatBVar it mandates that it's arguments become DatBVars, since that applies in other cases
  specifyRetType currT (SpecType DatBVar)
  newGenVar <- getNextValidVal $ GenType (NumVar, 0)
  --(currTypes, currArgs, validVals) <- get
  --let newVarVal = validVals `Sq.index` fromEnum NumVar
  --let (newGenVar,validVals') = (GenType (NumVar, newVarVal), Sq.update (fromEnum NumVar) (newVarVal+1) validVals)
  --put (currTypes, currArgs, validVals')
  addFunc newGenVar arg1
  addFunc newGenVar arg2
addFunc currT (Node (Left Gt) (arg1:arg2:[]))   = do --specifyRetType currT [arg1,arg2] (GenType (NumVar, 0))
  specifyRetType currT (SpecType DatBVar)
  newGenVar <- getNextValidVal $ GenType (NumVar, 0)
  addFunc newGenVar arg1
  addFunc newGenVar arg2
addFunc currT (Node (Left Eq) (arg1:arg2:[]))   = do {addFunc currT arg1; addFunc currT arg2; }  --specRetAndApplyToArgs currT [arg1,arg2] (GenType (GenVar, 0)) --some extraneous code here, forcing it to be a genvar doesn't do much
addFunc currT (Node (Left Not) (arg1:[]))       = specRetAndApplyToArgs currT [arg1]      (SpecType DatBVar)
addFunc currT (Node (Left And) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (SpecType DatBVar)
addFunc currT (Node (Left Or) (arg1:arg2:[]))   = specRetAndApplyToArgs currT [arg1,arg2] (SpecType DatBVar)
addFunc currT (Node (Left If) (arg1:arg2:arg3:[])) = do
  --specRetAndApplyToArgs (SpecType DatBVar) [arg1] (SpecType DatBVar) --is the DatBVar instead of currT correct? I believe it is. Only because it's specific - cannot do a similar thing with a genvar
  addFunc (SpecType DatBVar) arg1 --can only specify the type like this because it's specific, need to get a number for a genvar
  --specRetAndApplyToArgs currT [arg2,arg3] (GenType (GenVar, 0)) --some extraneous code here, forcing it to be a genvar doesn't do much
  addFunc currT arg2 --no new restrictions on arg2, arg3
  addFunc currT arg3
addFunc currT (Node (Left AppLists) (arg1:arg2:[])) =
  specRetAndApplyToArgs currT [arg1,arg2] (ListType $ GenType $ (GenVar, 0)) --broken - doesn't specify a genvar to a list; leaves it a genvar.
--addFunc currT (Node (Left MkList) (arg1:[])) = addFunc (getListOneLayerType currT) arg1 --it wants a return type of list, not anything else
--  case currT of --it wants a return type of list, not anything else
--       ListType t            -> addFunc t arg1
--       (GenType (GenVar, t)) -> addFunc (GenType (GenVar, t)) arg1
--addFunc currT (Node (Left ConsList) (arg1:arg2:[])) = do
--  addFunc (getListOneLayerType currT) arg1
--  addFunc currT arg2
  
{-  let (newCurrT,currTypes') =
        if getMoreSpecific currInnerT newGenVar == newGenVar then --assumes correct types, so not worried about comparing list nesting
          (newGenVar,DisjointSet.union (genTypeIntBijection currInnerT) (genTypeIntBijection newGenVar) $ DisjointSet.insert (genTypeIntBijection newGenVar) currTypes)
          else (currT,currTypes) --could theoretically use old validVals in this case, the new number wasn't used
  put (currTypes', currArgs, validVals')
  _ <- mapM (addFunc newCurrT) args
  return ()-}

--currT is the return type, says that the arguments given must be a subclass of the return type; potentially more specific based on newSpecType
specifyRetType :: TypeVar -> TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int) TypeVar
specifyRetType currT specType = do
  (currTypes, currArgs, validVals) <- get
  let specInnerType = getListInnerType specType
  let currInnerT = getListInnerType currT
  newGenVar <- getNextValidVal specInnerType
  let (newCurrT,currTypes') =
        if getMoreSpecific currInnerT newGenVar == newGenVar then --assumes correct types, so not worried about comparing list nesting
          (newGenVar,DisjointSet.union (genTypeIntBijection currInnerT) (genTypeIntBijection newGenVar) $ DisjointSet.insert (genTypeIntBijection newGenVar) currTypes)
          else (currT,currTypes) --could theoretically use old validVals in this case, the new number wasn't used
  put (currTypes', currArgs, validVals)
  return newCurrT

specRetAndApplyToArgs :: TypeVar -> [Expression] -> TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int) ()
specRetAndApplyToArgs currT args specType = do
  newCurrT <- specifyRetType currT specType
  _ <- mapM (addFunc newCurrT) args
  return ()

getNextValidVal :: TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int) TypeVar
getNextValidVal whichT = do
  (currTypes, currArgs, validVals) <- get
  let (newGenVar,validVals') = case whichT of
        GenType (t,_) ->
          let newVarVal = validVals `Sq.index` fromEnum t in
            (GenType (t, newVarVal), Sq.update (fromEnum t) (newVarVal+1) validVals)
        SpecType t ->
          (SpecType t, validVals)
  put (currTypes, currArgs, validVals')
  return newGenVar

--setNewArgType :: TypeVar -> [Expression] -> State  (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int) ()
--setNewArgType newCurrT args = do
