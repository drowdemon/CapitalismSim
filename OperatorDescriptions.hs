module OperatorDescriptions
       where
import qualified Data.Map.Strict as StrMap
import LangData

addDesc :: OpDesc
addDesc = (Add,FuncType{retType=GenType (NumVar, 0),
                        argType=StrMap.fromAscList [(0,GenType (NumVar, 0)),(1,GenType (NumVar, 0))]})
subtDesc :: OpDesc
subtDesc = (Subt,FuncType{retType=GenType (NumVar, 0),
                          argType=StrMap.fromAscList [(0,GenType (NumVar, 0)),(1,GenType (NumVar, 0))]})
divDesc :: OpDesc
divDesc = (Div,FuncType{retType=GenType (NumVar, 0),
                        argType=StrMap.fromAscList [(0,GenType (NumVar, 0)),(1,GenType (NumVar, 0))]})
mulDesc :: OpDesc
mulDesc = (Mul,FuncType{retType=GenType (NumVar, 0),
                        argType=StrMap.fromAscList [(0,GenType (NumVar, 0)),(1,GenType (NumVar, 0))]})
ltDesc :: OpDesc
ltDesc = (Lt,FuncType{retType=SpecType DatBVar,
                      argType=StrMap.fromAscList [(0,GenType (NumVar, 0)),(1,GenType (NumVar, 0))]})
gtDesc :: OpDesc
gtDesc = (Gt,FuncType{retType=SpecType DatBVar,
                      argType=StrMap.fromAscList [(0,GenType (NumVar, 0)),(1,GenType (NumVar, 0))]})
eqDesc :: OpDesc
eqDesc = (Eq,FuncType{retType=SpecType DatBVar,
                      argType=StrMap.fromAscList [(0,GenType (GenVar, 0)),(1,GenType (GenVar, 0))]})
notDesc :: OpDesc
notDesc = (Not,FuncType{retType=SpecType DatBVar,
                        argType=StrMap.fromAscList [(0,SpecType DatBVar)]})
andDesc :: OpDesc
andDesc = (And,FuncType{retType=SpecType DatBVar,
                        argType=StrMap.fromAscList [(0,SpecType DatBVar),(1,SpecType DatBVar)]})
orDesc :: OpDesc
orDesc = (Or,FuncType{retType=SpecType DatBVar,
                      argType=StrMap.fromAscList [(0,SpecType DatBVar),(1,SpecType DatBVar)]})
ifDesc :: OpDesc
ifDesc = (If,FuncType{retType=GenType (GenVar, 0),
                      argType=StrMap.fromAscList [(0,SpecType DatBVar),(1,GenType (GenVar, 0)),(2,GenType (GenVar, 0))]})
mapDesc :: OpDesc
mapDesc = (Map,FuncType{retType=ListType (GenType (GenVar, 1)),
                        argType=StrMap.fromAscList [(0,ExprType FuncType{
                                                        retType=GenType (GenVar, 1),
                                                        argType=StrMap.fromAscList [(0,GenType (GenVar, 0))]})
                                                   ,(1,ListType (GenType (GenVar, 0)))]})
foldDesc :: OpDesc
foldDesc = (Fold,FuncType{retType=GenType (GenVar, 1),
                          argType=StrMap.fromAscList [(0,ExprType FuncType{
                                                          retType=GenType (GenVar, 1),
                                                          argType=StrMap.fromAscList [(0,GenType (GenVar, 0))
                                                                                     ,(1,GenType (GenVar, 1))]})
                                                     ,(1,GenType (GenVar, 1))
                                                     ,(2,ListType (GenType (GenVar, 0)))]})
mkListDesc :: OpDesc
mkListDesc = (MkList,FuncType{retType=ListType (GenType (GenVar, 0)),
                              argType=StrMap.fromAscList [(0,GenType (GenVar,0))]})
consListDesc :: OpDesc
consListDesc = (ConsList,FuncType{retType=ListType (GenType (GenVar, 0)),
                                  argType=StrMap.fromAscList [(0,GenType (GenVar, 0)),(1,ListType (GenType (GenVar, 0)))]})
appListsDesc :: OpDesc
appListsDesc = (AppLists,FuncType{retType=ListType (GenType (GenVar, 0)),
                                  argType=StrMap.fromAscList [(0,ListType (GenType (GenVar, 0))),(1,ListType (GenType (GenVar, 0)))]})
ceilDesc :: OpDesc
ceilDesc = (Ceil,FuncType{retType=SpecType DatIVar,
                          argType=StrMap.fromAscList [(0,GenType (NumVar, 0))]})
floorDesc :: OpDesc
floorDesc = (Floor,FuncType{retType=SpecType DatIVar,
                            argType=StrMap.fromAscList [(0,GenType (NumVar, 0))]})
toDoubleDesc :: OpDesc
toDoubleDesc = (ToDouble,FuncType{retType=SpecType DatDVar,
                                  argType=StrMap.fromAscList [(0,GenType (NumVar, 0))]})

opMap :: StrMap.Map Operator FuncType
opMap = StrMap.fromList [addDesc,subtDesc,divDesc,mulDesc,ltDesc,gtDesc,eqDesc,notDesc,andDesc,orDesc,ifDesc,mapDesc,foldDesc,mkListDesc,consListDesc,appListsDesc,ceilDesc,floorDesc,toDoubleDesc]
