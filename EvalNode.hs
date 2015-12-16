module EvalNode
       where
--import InternalProg
import Data.Tree
import qualified Data.Map.Strict as StrMap
import BasicData
import Control.Monad.Trans.State

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
  let (newRetType,newFunc) = runState (addFunc arg1) FuncDesc{body=arg1,retType=GenVar (-1), argType=StrMap.empty}
      retFunc = newFunc{retType=newRetType}
  addFunc' retFunc  
  return $ DatB True --TODO?
  where addFunc' :: FuncDesc -> State (StrMap.Map Int FuncDesc) ()
        addFunc' addF = state $ \fTable ->
          ((), StrMap.insert (StrMap.size fTable) addF fTable)

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

addFunc :: Expression -> State FuncDesc TypeVar
addFunc (Node (Right dat) []) = 
  state $ \currFunc ->
            let argsCurr = argType currFunc
                argsSize = StrMap.size argsCurr
              in case isFid(dat) of
                  Left argId ->
                    case argId `StrMap.lookup` argsCurr of
                         Just tp -> (tp, currFunc)
                         --Nothing adds a new arg variable to the current function
                         Nothing -> (GenVar (argsSize),
                                     currFunc{argType=(StrMap.insert argsSize (GenVar argsSize) argsCurr)})
                  Right d  -> (d, currFunc)
  where isFid :: Datum -> Either Int TypeVar
        {-isFid (DatB _)    = Right $ SpecVar $ DatB False --default values
        isFid (DatI _)    = Right $ SpecVar $ DatI 0
        isFid (DatD _)    = Right $ SpecVar $ DatD 0.0
        isFid (ListI _)   = Right $ SpecVar $ ListI [0]
        isFid (ListB _)   = Right $ SpecVar $ ListB [False]
        isFid (ListD _)   = Right $ SpecVar $ ListD [0.0]
        isFid (FuncId _)  = Right $ SpecVar $ FuncId 0-}
        isFid (FuncArg x) = Left x
        isFid d           = Right $ SpecVar d
        
addFunc (Node (Left Add) (arg1:arg2:[])) = do
    t1 <- addFunc arg1
    t2 <- addFunc arg2
    let (mt1,mt2) = specifyVars (makeNum t1) (makeNum t2)
    currFunc <- get
    case mt1 of
      SpecVar d -> currFunc
       
      
  where
    makeNum :: TypeVar -> TypeVar
    makeNum (NumVar x)         = NumVar x
    makeNum (GenVar x)         = NumVar x
    makeNum (SingletonVar x)   = NumVar x
    makeNum (SpecVar (DatI x)) = SpecVar (DatI x)
    makeNum (SpecVar (DatD x)) = SpecVar (DatD x)
    --makeNum _                = Nothing
    specifyVars :: TypeVar -> TypeVar -> (TypeVar, TypeVar)
    --specifyVars (SpecVar (DatI x)) (SpecVar (DatI y)) = ((SpecVar (DatI x)), (SpecVar (DatI y)))
    --specifyVars (SpecVar (DatD x)) (SpecVar (DatD y)) = ((SpecVar (DatD x)), (SpecVar (DatD y)))
    specifyVars (SpecVar d1) (SpecVar d2) = (SpecVar d1, SpecVar d2) --assuming valid, covers above
    specifyVars t1 (SpecVar d2) = specifyVars (SpecVar d2) t1 --swap, specvar always first now
    specifyVars (SpecVar (DatI x1)) (GenVar t2) = (SpecVar (DatI x1), SpecVar (DatI 0))
    specifyVars (SpecVar (DatD x1)) (GenVar t2) = (SpecVar (DatD x1), SpecVar (DatD 0.0))
    specifyVars (SpecVar (DatI x1)) (NumVar t2) = (SpecVar (DatI x1), SpecVar (DatI 0))
    specifyVars (SpecVar (DatD x1)) (NumVar t2) = (SpecVar (DatD x1), SpecVar (DatD 0.0))
    specifyVars (NumVar t1) (NumVar t2) = (NumVar t1, NumVar t1)
