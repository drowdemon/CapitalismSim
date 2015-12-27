module LangProc
       where
import Data.Tree
import qualified Data.Map.Strict as StrMap
import LangData
import Control.Monad.Trans.State
import Control.Monad
import qualified Data.IntDisjointSet as DisjointSet
--import qualified Data.Traversable as Traversable
import qualified Data.Sequence as Sq
--import qualified Data.List as DatLst
--import Data.Maybe
import qualified Data.Foldable as DatFold
--import qualified Debug.Trace as Tr

--state is the function table, and the arguments to the function i.e. current values in FuncArg 0, 1, ...
evalNode :: Expression -> State (StrMap.Map Int FuncDesc, StrMap.Map Int Datum) Datum
evalNode (Node (Right (FuncArg x)) []) = do
  (_, args) <- get
  return $ case x `StrMap.lookup` args of Just d -> d
evalNode (Node (Right dat) []) = return dat

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
  (fTable,_) <- get
  let typesInit = (genTypeIntBijection (GenType (GenVar, 0))):[genTypeIntBijection $ SpecType (minBound::SpecificType)..genTypeIntBijection $ SpecType (maxBound::SpecificType)]
      (currTypes,currArgs,_,_) = snd $ runState (addFunc (GenType (GenVar, 0)) arg1) --note that fTable ret is ignored
                                               (DisjointSet.fromList $ zip typesInit typesInit,
                                               StrMap.empty,
                                               (1 Sq.<|(Sq.replicate (fromEnum (maxBound::GenericType)) 0)),
                                               fTable)
      specTypes    = getSpecificest currTypes
        --Tr.trace ((show currTypes)++" args "++(show currArgs)) $ getSpecificest currTypes
      specArgs     = StrMap.map (findSpecificType' currTypes specTypes) currArgs
      specRet      = findSpecificType' currTypes specTypes (GenType (GenVar, 0))
      retFunc      = FuncDesc{body=arg1, retType=specRet, argType=specArgs} --retrieve ret/arg types from currTypes
  addFunc' retFunc 
  return $ DatB True --TODO? Anonymous functions would require changing this.
  where addFunc' :: FuncDesc -> State (StrMap.Map Int FuncDesc, StrMap.Map Int Datum) ()
        addFunc' addF = modify f'
          where f' (fTable,args) = (StrMap.insert (StrMap.size fTable) addF fTable, args)
        getSpecificest :: DisjointSet.IntDisjointSet -> StrMap.Map TypeVar TypeVar
        getSpecificest currTypes =
          getSpecificest' (map (\(x,y) -> (genTypeIntBijectionInv x, genTypeIntBijectionInv y)) $
                               fst $ DisjointSet.toList currTypes) StrMap.empty
          where
            --associate representative with min element in set
            getSpecificest' :: [(TypeVar, TypeVar)] -> StrMap.Map TypeVar TypeVar -> StrMap.Map TypeVar TypeVar --state is best seen typevar
            getSpecificest' [] soFar = soFar
            getSpecificest' ((tp, rep):rst) soFar =
              case StrMap.lookup rep soFar of
                Just bestSoFar -> getSpecificest' rst $ StrMap.insert rep (getMoreSpecific bestSoFar tp) soFar
                Nothing        -> getSpecificest' rst $ StrMap.insert rep tp soFar
        findSpecificType' :: DisjointSet.IntDisjointSet -> StrMap.Map TypeVar TypeVar -> TypeVar -> TypeVar
        findSpecificType' curr spec tp = --looks up in currTypes, and then looks that up in specTypes
          let Just ret =
               do --maybe monad
                 let (resM,_) = DisjointSet.lookup (genTypeIntBijection tp) curr
                 res <- resM
                 specT <- StrMap.lookup (genTypeIntBijectionInv res) spec
                 case specT of
                   ListType t -> Just . ListType $ findSpecificType' curr spec t
                   t -> return t
          in ret

evalNode (Node (Left Call) (fIDarg:args)) = do
  (fTable,_) <- get
  FuncId fid <- evalNode fIDarg
  let Just f = fid `StrMap.lookup` fTable
  argResults <- mapM evalNode args
  let argsMap = StrMap.fromList (zip [0..] argResults)
  return $ evalState (evalNode $ body f) (fTable, argsMap)

evalNode (Node (Left Map) (fIDarg:lstArg:[])) = do
  ListDat _ lst <- evalNode lstArg
  retLst <- mapM (\a -> evalNode (Node{rootLabel=Left Call, subForest=[fIDarg,Node{rootLabel=Right a, subForest=[]}]})) lst
  return $ ListDat (datToType . head $ retLst) retLst

evalNode (Node (Left Fold) (fIDarg:startArg:lstArg:[])) = do
  ListDat _ lst <- evalNode lstArg --ignored argument is result type. Assuming types are safe as usual
  startDat <- evalNode startArg
  foldM (\start arg -> evalNode (Node{rootLabel=Left Call, subForest=[fIDarg,
                                   Node{rootLabel=Right arg, subForest=[]},
                                   Node{rootLabel=Right start, subForest=[]}]})) startDat lst

evalNode (Node (Left MkList) (arg1:[])) = do
  a1 <- evalNode arg1
  return $ case a1 of
       DatB a      -> ListDat DatBVar [DatB a]
       DatI a      -> ListDat DatIVar [DatI a]
       DatD a      -> ListDat DatDVar [DatD a]
       FuncId a    -> ListDat FuncIdVar [FuncId a]
       ListDat t a -> ListDat t $ [ListDat t a]

evalNode (Node (Left ConsList) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  return $ case (a1,a2) of
       (DatB a, ListDat DatBVar b)     -> ListDat DatBVar $ (DatB a):b
       (DatI a, ListDat DatIVar b)     -> ListDat DatIVar $ (DatI a):b
       (DatD a, ListDat DatDVar b)     -> ListDat DatDVar $ (DatD a):b
       (FuncId a, ListDat FuncIdVar b) -> ListDat DatDVar $ (FuncId a):b

evalNode (Node (Left AppLists) (arg1:arg2:[])) = do
  a1 <- evalNode arg1
  a2 <- evalNode arg2
  return $ case (a1,a2) of
       (ListDat t1 a, ListDat t2 b) -> ListDat t1 $ f t1 t2 a b --a++b
       --(ListDat a, ListDat b) -> ListDat $ a++b
  where f t1 t2 a b | t1==t2 = a++b

evalNode (Node (Left Ceil) (arg1:[])) = do
  a1 <- evalNode arg1
  return $ case a1 of
             DatI a -> DatI a
             DatD a -> DatI $ ceiling a

evalNode (Node (Left Floor) (arg1:[])) = do
  a1 <- evalNode arg1
  return $ case a1 of
             DatI a -> DatI a
             DatD a -> DatI $ floor a

evalNode (Node (Left ToDouble) (arg1:[])) = do
  a1 <- evalNode arg1
  return $ case a1 of
             DatI a -> DatD $ fromIntegral a
             DatD a -> DatD a
             
--first TypeVar is the return type this expression must have. The expression is the expression whose type we are evaluating
--state monad has: a disjoint set containing all the typeswhich have been foiund to be equivalent
--                 a map from the function argument ids to their types
--                 a Sequence (list type) containing available new numbers for the generic types
--                 a function table
addFunc :: TypeVar -> Expression -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int, StrMap.Map Int FuncDesc) ()
addFunc currT (Node (Right dat) []) = do
  (currTypes, currArgs, validVals, fTable) <- get
  put $ case dat of
    FuncArg argId ->
      case argId `StrMap.lookup` currArgs of --already has a type
          Just argTp -> (DisjointSet.union (genTypeIntBijection currT) (genTypeIntBijection argTp) currTypes, currArgs, validVals, fTable) --They must be compatible, so no check
          Nothing    -> (currTypes, StrMap.insert argId currT currArgs, validVals, fTable) --now the argument argId has type currT
    d             -> (DisjointSet.union (genTypeIntBijection currT) (genTypeIntBijection $ SpecType $ datToType d) currTypes, currArgs, validVals,fTable)

addFunc currT (Node (Left Add) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Subt) (arg1:arg2:[])) = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Div) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Mul) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (GenType (NumVar, 0))
addFunc currT (Node (Left Lt) (arg1:arg2:[]))   = do
  _ <- specifyRetType currT (SpecType DatBVar) --I know the return type, and don't care about it
  newGenVar <- getNextValidVal $ GenType (NumVar, 0)
  addFunc newGenVar arg1
  addFunc newGenVar arg2
addFunc currT (Node (Left Gt) (arg1:arg2:[]))   = do
  _ <- specifyRetType currT (SpecType DatBVar)
  newGenVar <- getNextValidVal $ GenType (NumVar, 0)
  addFunc newGenVar arg1
  addFunc newGenVar arg2
addFunc currT (Node (Left Eq) (arg1:arg2:[]))   = do ----do {addFunc currT arg1; addFunc currT arg2; }
  _ <- specifyRetType currT (SpecType DatBVar)
  newGenVar <- getNextValidVal $ GenType (GenVar, 0)
  addFunc newGenVar arg1
  addFunc newGenVar arg2
addFunc currT (Node (Left Not) (arg1:[]))       = specRetAndApplyToArgs currT [arg1]      (SpecType DatBVar)
addFunc currT (Node (Left And) (arg1:arg2:[]))  = specRetAndApplyToArgs currT [arg1,arg2] (SpecType DatBVar)
addFunc currT (Node (Left Or) (arg1:arg2:[]))   = specRetAndApplyToArgs currT [arg1,arg2] (SpecType DatBVar)
addFunc currT (Node (Left If) (arg1:arg2:arg3:[])) = do
  addFunc (SpecType DatBVar) arg1 --can only specify the type like this because it's specific, need to get a number for a genvar
  addFunc currT arg2 --no new restrictions on return type
  addFunc currT arg3

addFunc currT (Node (Left Call) (fidArg:args)) = do
  (newValsArgMap,fDesc) <- validateFunctionTypes fidArg
  let Just newRetInnerType = (getListInnerType $ retType fDesc) `StrMap.lookup` newValsArgMap
  unifyTypes currT $ setActListInnerType (retType fDesc) newRetInnerType --need this, not specifyRetType. That treats second arg as template, gets a valid val for it. We already did that.
  let addWithNewArg' (ind, arg) =
        let Just ret =
              do oldT <- StrMap.lookup ind (argType fDesc)
                 newArgT <- StrMap.lookup (getListInnerType oldT) newValsArgMap
                 Just $ addFunc (setActListInnerType oldT newArgT) arg
        in ret
  _ <- mapM addWithNewArg' $ (zip [(0::Int)..] args)
  return ()

addFunc currT (Node (Left Map) (arg1:arg2:[])) = do
  specCurrT <- specifyRetType currT (ListType $ GenType $ (GenVar, 0))
  (newValsArgMap,fDesc) <- validateFunctionTypes arg1
  let Just newRetInnerType = (getListInnerType $ retType fDesc) `StrMap.lookup` newValsArgMap
  (unifyTypes specCurrT) $ ListType $ setActListInnerType (retType fDesc) newRetInnerType
  --return type now correct
  let Just argT = --these lines copy pasted in addFunc for Fold
        do --maybe monad
          oldArgT <- StrMap.lookup 0 (argType fDesc) --look up the 0th argument to the passed in function
          newInnerArgT <- StrMap.lookup (getListInnerType oldArgT) newValsArgMap
          Just $ (setActListInnerType oldArgT newInnerArgT) --we get the type of the argument
  --(currTypes, currArgs, validVals, fTable) <- get
  --let currTypes' = DisjointSet.insert (genTypeIntBijection $ ListType argT) currTypes
  --put (currTypes', currArgs, validVals, fTable)
  addFunc (ListType argT) arg2 --arg2 has to be a list of the functions arg type

addFunc currT (Node (Left Fold) (fIdArg:initArg:lstArg:[])) = do
  (newValsArgMap,fDesc) <- validateFunctionTypes fIdArg --we don't actually constrain the second argument of the fuction, but it must be the same as the ret type: a -> b -> b
  let Just newRetInnerType = (getListInnerType $ retType fDesc) `StrMap.lookup` newValsArgMap
  let newRetType = (setActListInnerType (retType fDesc) newRetInnerType)
  unifyTypes currT newRetType --ret type correct
  addFunc newRetType initArg --initArg has to be the same type as the return type
  let Just argT = --these lines copy pasted from addFunc for Map
        do --maybe monad
          oldArgT <- StrMap.lookup 0 (argType fDesc) --look up the 0th argument to the passed in function
          newInnerArgT <- StrMap.lookup (getListInnerType oldArgT) newValsArgMap
          Just $ (setActListInnerType oldArgT newInnerArgT) --we get the type of the argument
  addFunc (ListType argT) lstArg

addFunc currT (Node (Left MkList) (arg1:[])) = do
  specT <- specifyRetType currT (ListType $ GenType $ (GenVar, 0))
  addFunc (getListOneLayerType specT) arg1
addFunc currT (Node (Left ConsList) (arg1:arg2:[])) = do
  specT <- specifyRetType currT (ListType $ GenType $ (GenVar, 0))
  addFunc (getListOneLayerType specT) arg1
  addFunc specT arg2
addFunc currT (Node (Left AppLists) (arg1:arg2:[])) =
  specRetAndApplyToArgs currT [arg1,arg2] (ListType $ GenType $ (GenVar, 0)) --broken - doesn't specify a genvar to a list; leaves it a genvar. fixed?
addFunc currT (Node (Left Ceil) (arg1:[]))   = do
  _ <- specifyRetType currT (SpecType DatIVar)
  newGenVar <- getNextValidVal $ GenType (NumVar, 0)
  addFunc newGenVar arg1
addFunc currT (Node (Left Floor) (arg1:[]))   = do
  _ <- specifyRetType currT (SpecType DatIVar)
  newGenVar <- getNextValidVal $ GenType (NumVar, 0)
  addFunc newGenVar arg1
addFunc currT (Node (Left ToDouble) (arg1:[]))   = do
  _ <- specifyRetType currT (SpecType DatDVar)
  newGenVar <- getNextValidVal $ GenType (NumVar, 0)
  addFunc newGenVar arg1

--currT is the return type, says that the arguments given must be a subclass of the return type; potentially more specific based on newSpecType
specifyRetType :: TypeVar -> TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int, StrMap.Map Int FuncDesc) TypeVar
specifyRetType currT specType = do
  let (matchInnerCurr, matchInnerSpec, firstDeepest) = getShallowestInnerType currT specType
  newGenVar <- case (if firstDeepest then matchInnerCurr else matchInnerSpec) of
                    ListType t -> setValidListInnerType (ListType t) (getListInnerType matchInnerSpec)
                    _          -> getNextValidVal matchInnerSpec
  (currTypes, currArgs, validVals, fTable) <- get
  let (newCurrT,currTypes') =
        if getMoreSpecific matchInnerCurr newGenVar == newGenVar then --assumes correct types, so not worried about comparing list nesting
          (newGenVar,DisjointSet.union (genTypeIntBijection matchInnerCurr) (genTypeIntBijection newGenVar) $ DisjointSet.insert (genTypeIntBijection newGenVar) currTypes)
          else (currT,currTypes) --could theoretically use old validVals in this case, the new number wasn't used
  put (currTypes', currArgs, validVals, fTable)
  return newCurrT

specRetAndApplyToArgs :: TypeVar -> [Expression] -> TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int, StrMap.Map Int FuncDesc) ()
specRetAndApplyToArgs currT args specType = do
  newCurrT <- specifyRetType currT specType
  _ <- mapM (addFunc newCurrT) args
  return ()

getNextValidVal :: TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int, StrMap.Map Int FuncDesc) TypeVar
getNextValidVal whichT = do
  (currTypes, currArgs, validVals, fTable) <- get
  let (newGenVar,validVals') = case whichT of
        GenType (t,_) ->
          let newVarVal = validVals `Sq.index` fromEnum t in
            (GenType (t, newVarVal), Sq.update (fromEnum t) (newVarVal+1) validVals)
        SpecType t ->
          (SpecType t, validVals)
  put (DisjointSet.insert (genTypeIntBijection newGenVar) currTypes, currArgs, validVals', fTable)
  return newGenVar

--first a list, then a non-list inner type
setValidListInnerType :: TypeVar -> TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int, StrMap.Map Int FuncDesc) TypeVar
setValidListInnerType (ListType lst) t = liftM ListType $ setValidListInnerType lst t
setValidListInnerType _ t = getNextValidVal t

unifyTypes :: TypeVar -> TypeVar -> State (DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int, StrMap.Map Int FuncDesc) ()
unifyTypes a b =
  case (a,b) of
    (ListType t1,ListType t2) -> unifyTypes t1 t2 --do { modify $ unify' (ListType t1) (ListType t2); unifyTypes t1 t2; }
    (t1, t2)                     -> modify $ unify' t1 t2
    {-(GenType g, ListType t2)  -> modify (unify' (GenType g) (ListType t2))
    (ListType t1, GenType g)  -> modify (unify' (GenType g) (ListType t1))
    (GenType g, SpecType s)   -> modify (unify' (GenType g) (SpecType s))
    (SpecType s, GenType g)   -> modify (unify' (GenType g) (SpecType s))
    (GenType g1, GenType g2)  -> modify (unify' (GenType g1) (GenType g2))-}
  where unify' alph bet (currTypes, args, v, ft) =
          let currTypes' = DisjointSet.insert (genTypeIntBijection bet) $ DisjointSet.insert (genTypeIntBijection alph) currTypes
          in (DisjointSet.union (genTypeIntBijection alph) (genTypeIntBijection bet) currTypes', args, v, ft)
          
validateFunctionTypes :: Expression -> State(DisjointSet.IntDisjointSet, StrMap.Map Int TypeVar, Sq.Seq Int, StrMap.Map Int FuncDesc) (StrMap.Map TypeVar TypeVar, FuncDesc)
validateFunctionTypes fidArg = do
  let fid = case fidArg of (Node (Right (FuncId a)) []) -> a --has to be compile time constant, just a straight id, no expressions. The same way this doesn't handle 5+[5], this doesn't handle call (2+2), because it has to be a constant FuncId
  (_, _, _, fTable) <- get
  let Just fDesc = fid `StrMap.lookup` fTable
  let makeValidArgVal' accumMap oldArgT =
        case getListInnerType oldArgT `StrMap.lookup` accumMap of
          Just _ -> return accumMap
          Nothing -> do
            newArgT <- getNextValidVal $ getListInnerType oldArgT
            return $ StrMap.insert (getListInnerType oldArgT) newArgT accumMap
  newValsArgMapPreRet <- DatFold.foldlM makeValidArgVal' StrMap.empty (argType fDesc)
  newValsArgMap <- makeValidArgVal' newValsArgMapPreRet (retType fDesc)
  return (newValsArgMap, fDesc)
