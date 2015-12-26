module EconData
       where
import LangData
import qualified Data.Map.Strict as StrMap

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
  , funcTable   :: StrMap.Map Int FuncDesc
  }

data Action = MkContract Person Contract
            | MkCompany Good
