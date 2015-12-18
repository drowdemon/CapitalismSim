module BasicData
       where
import Data.Tree
import qualified Data.Map.Strict as Map

type IdeaKey = Integer
type GoodKey = Integer

data Person = Person
  { job        :: Company
  , owns       :: [Company]
  , pIdeas     :: [IdeaKey]
  , pGood      :: [(GoodKey, Integer)]
  , mind       :: Mind
  , pContracts :: [Contract]
  } 

data OweOwed a = OweOwed
  { owe  :: a
  , owed :: a
  }

data ItmQty a = ItmQty
  { item :: a
  , qty  :: Integer
  }

data Good = Good --have a map/set of these
  { gkey              :: GoodKey
  , desirability      :: Integer
  , dropDesire        :: Integer --need this to somehow represent a drop function based on amnt owned
  , scalability       :: Integer --need this to somehow represent a scalability function based on amnt producing and cost
  , costToManufacture :: Double
  , depreciation      :: Integer
  , stability         :: Integer
    --include a function (partial TM?) to change all of these.
  }

data Idea = Idea
  { ikey :: IdeaKey
  }

data Contract = Contract
  { cGoods            :: OweOwed (ItmQty Good)
  , goodsPTime        :: OweOwed (ItmQty Good, Integer) --Selection will greatly favor using prebuilt things to TM contracts, b/c more successful. But these units don't model things like interest well.
  , money             :: OweOwed Double
  , moneyPTime        :: OweOwed (Double, Integer)
  , percentMoney      :: OweOwed Double
  , percentMoneyPTime :: OweOwed (Double, Integer)
  , cIdeas            :: OweOwed Idea
  }
  
data Company = Company
  { workers :: [Person]
  , good    :: Good
  }

data Mind = Mind
  { mainProg    :: Expression
  , bargainProg :: Expression
  , funcTable   :: Map.Map Int FuncDesc
  }

data FuncDesc = FuncDesc
  { body :: Expression
  , retType :: TypeVar
  , argType :: Map.Map Int TypeVar
  }
  deriving (Show)

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
              | Call  --todo --takes a FuncId (function id, like func pointer) - id number of the function, and all the arguments to that function
              | Map   --todo --also takes an fid, along with a list to map over
              | Fold  --todo 
              | MkList   --todo add
              | ConsList --todo add
              | AppLists
              deriving (Show)

data Datum = DatB Bool --takes a monad - the list monad or the identity. I think that works
           | DatI Integer
           | DatD Double
--               | ListB [Bool]
--               | ListI [Integer]
--               | ListD [Double]
           | FuncArg Int --the integer is the 'name' of the argument. Types are inferred.
           | FuncId Int  --integer is 'name' of function
           | ListDat SpecificType [Datum]
           deriving (Show, Eq)

--Curently, GenVar must be lexicographically first. Ugly, but true.
data GenericType = GenVar       --anything
                 | NumVar       --numeric: double or int
--                 | SingletonVar --not a list. Don't put it in a ListType --I think I don't actually need this
--                 | ListVar      --list of anything
--                 | ListNumVar   --list of numerics
                 deriving (Show, Ord, Eq, Enum, Bounded)
                          
data SpecificType = DatBVar --no associated ints
                  | DatIVar
                  | DatDVar
--                  | ListBVar
--                  | ListIVar
--                  | ListDVar
                  | FuncIdVar
                  deriving (Show, Enum, Bounded, Ord, Eq)
                           
datToType :: Datum -> SpecificType
datToType (DatB _) = DatBVar
datToType (DatI _) = DatIVar
datToType (DatD _) = DatDVar
--datToType (ListB _) = ListBVar
--datToType (ListI _) = ListIVar
--datToType (ListD _) = ListDVar
datToType (FuncId _) = FuncIdVar
                    
data TypeVar = GenType (GenericType, Int) --integer is an id, like type t0, t1
             | SpecType SpecificType
             | ListType TypeVar
             deriving (Show, Ord, Eq)

data Action = MkContract Person Contract
            | MkCompany Good

--class Operator opT argsT retT where
--  runOp :: opT -> argsT -> retT
--  runOp op args = op args
  
type Expression = (Tree (Either Operator Datum))
