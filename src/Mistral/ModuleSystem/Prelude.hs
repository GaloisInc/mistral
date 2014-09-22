module Mistral.ModuleSystem.Prelude where

import Mistral.ModuleSystem.Interface
import Mistral.ModuleSystem.Name
import Mistral.TypeCheck.AST
import Mistral.Utils.SCC

import qualified Data.Map as Map


prelude :: Iface
prelude  = Iface { ifaceModName = mkGlobal [] "Prelude"
                 , ifaceDeps    = []
                 , ifaceDatas   = preludeDatas
                 , ifaceBinds   = preludeBinds
                 , ifaceTySyns  = preludeSyns
                 , ifaceTypes   = preludeTypes
                 , ifaceInsts   = preludeInsts }

preludeDatas :: [Group Data]
preludeDatas  =
  [ dat "Maybe" ["a"] $ \[a] -> [con "Just" [a], con "None" []]
  ]
 where
  dat str ps f = NonRecursive $ Data (mkGlobal ["Prelude"] str) params cons
    where
    params = zipWith toParam [0..] ps
    toParam i n = TParam { tpUserName = Just (mkLocal n), tpIndex = i }
    cons   = f (map (TVar . TVBound) params)
  con str ty = Constr (mkGlobal ["Prelude"] str) ty

preludeBinds :: Map.Map Name IfaceBind
preludeBinds  = Map.fromList
  [ prim "pure" ["a"] $ \ [a] ->
         ([], a ~> taction a, PPure)

  , prim "log" [] $ \ [] ->
         ([], stringCon ~> taction unitCon, PLog)

  , prim "send" ["a","b"] $ \ [a,b] ->
         ([sendTargetProp a, serializableProp b], a ~> b ~> taction unitCon, PSend)

  , prim "goto" [] $ \ [] ->
         ([], stateCon ~> transitionCon, PGoto)

  , prim "addSchedule" [] $ \ [] ->
         ([], scheduleCon ~> taction unitCon, PAddSchedule)

  , prim "done" [] $ \ [] ->
         ([], transitionCon, PDone)

  , prim "seconds" [] $ \ [] ->
         ([], timeCon ~> intCon, PSeconds)

  , prim "async" [] $ \ [] ->
         ([], taction unitCon ~> taction unitCon, PAsync)

  , prim "uptime" [] $ \ [] ->
         ([], taction timeCon, PUptime)

  , prim "resetTimer" [] $ \[] ->
         ([], taction unitCon, PResetTimer)

  , prim "myTaskHandle" [] $ \[] ->
         ([], taction taskHandleCon, PMyTaskHandle)


    -- arithmetic
  , prim "add" ["a"] $ \ [a] -> ([arithProp a], a ~> a ~> a, PAdd)
  , prim "sub" ["a"] $ \ [a] -> ([arithProp a], a ~> a ~> a, PSub)
  , prim "neg" ["a"] $ \ [a] -> ([arithProp a], a ~> a,      PNeg)
  , prim "mul" ["a"] $ \ [a] -> ([arithProp a], a ~> a ~> a, PMul)
  , prim "div" ["a"] $ \ [a] -> ([arithProp a], a ~> a ~> a, PDiv)
  , prim "mod" ["a"] $ \ [a] -> ([arithProp a], a ~> a ~> a, PMod)
  , prim "pow" ["a"] $ \ [a] -> ([arithProp a], a ~> intCon ~> a, PPow)

    -- equality
  , prim "equals" ["a"] $ \ [a] -> ([eqProp a], a ~> a ~> boolCon, PEq )

    -- comparisons
  , prim "lt"  ["a"] $ \ [a] -> ([cmpProp a], a ~> a ~> boolCon, PLt )
  , prim "gt"  ["a"] $ \ [a] -> ([cmpProp a], a ~> a ~> boolCon, PGt )
  , prim "le"  ["a"] $ \ [a] -> ([cmpProp a], a ~> a ~> boolCon, PLe )
  , prim "ge"  ["a"] $ \ [a] -> ([cmpProp a], a ~> a ~> boolCon, PGe )

    -- bitwise operations
  , prim "ls"  ["a"] $ \ [a] -> ([bitsProp a], a ~> a ~> a,       PLS )
  , prim "rs"  ["a"] $ \ [a] -> ([bitsProp a], a ~> a ~> a,       PRS )
  , prim "and" ["a"] $ \ [a] -> ([bitsProp a], a ~> a ~> a,       PAnd)
  , prim "xor" ["a"] $ \ [a] -> ([bitsProp a], a ~> a ~> a,       PXor)
  , prim "or"  ["a"] $ \ [a] -> ([bitsProp a], a ~> a ~> a,       POr )
  , prim "not" ["a"] $ \ [a] -> ([bitsProp a], a ~> a,            PNot)

  -- string functions
  , prim "stringToAtom" [] $ \ [] ->
         ([], stringCon ~> atomCon, PStr2Atom)
  , prim "string" ["a"] $ \ [a] ->
         ([stringableProp a], a ~> stringCon, PString)

  -- FromString function
  -- (type aliases of string, not things that can be parsed / read)
  , prim "fromString" ["a"] $ \ [a] ->
        ([fromStringProp a], stringCon ~> a, PFromString)

  , con "True"  boolCon
  , con "False" boolCon
  ]
  where

  infixr 1 ~>
  (~>) = tarrow

  prim str ps f =
    (mkGlobal ["Prelude"] str, IfacePrim schema p)
    where

    params      = zipWith toParam [0 ..] ps
    toParam i n = TParam { tpUserName = Just (mkLocal n), tpIndex = i }

    (cxt,ty,p) = f (map (TVar . TVBound) params)

    schema = Forall params cxt ty

  con str ty = (mkGlobal ["Prelude"] str, IfaceBind (mkSchema ty))


preludeSyns :: Map.Map Name TySyn
preludeSyns  = Map.fromList
  [ syn "Action" ["a"] $ \ [a] ->
      -- type Action a   = Stmts Open a
      tstmts openCon a

  , syn "Transition" [] $ \ [] ->
      -- type Transition = Stmts Closed State
      tstmts closedCon stateCon
  ]
  where
  syn str ps f = (name, TySyn { synName = name, synType = schema })
    where
    name = mkGlobal ["Prelude"] str

    params      = zipWith toParam [0 ..] ps
    toParam i n = TParam { tpUserName = Just (mkLocal n), tpIndex = i }

    ty = f (map (TVar . TVBound) params)

    schema = Forall params [] ty


preludeTypes :: Map.Map Name IfaceType
preludeTypes  = Map.fromList
  [ ty "Int"
  , ty "Bool"
  , ty "TaskHandle"
  , ty "IPv4"
  , ty "Atom"
  , ty "Topology"
  , ty "TaskSet"
  , ty "Task"
  , ty "Time"
  , ty "Schedule"
  , ty "String"
  , ty "Time"
  , ty "State"
  , ty "Stmts"
  , ty "Maybe"
  , ty "Node"

    -- builtin constraints
  , ty "Serializable"
  , ty "SendTarget"
  , ty "LinkTarget"
  , ty "TaskTarget"
  , ty "Eq"
  , ty "Cmp"
  , ty "Arith"
  , ty "Bits"
  , ty "Stringable"
  ]
  where
  ty str = (name, IfaceType name)
    where
    name = mkGlobal ["Prelude"] str


preludeInsts :: [Inst]
preludeInsts  =
    -- Serializable instances
  [ inst [] (\ _ -> [] ==> serializableProp taskHandleCon)
  , inst [] (\ _ -> [] ==> serializableProp stringCon)
  , inst [] (\ _ -> [] ==> serializableProp atomCon)
  , inst [] (\ _ -> [] ==> serializableProp intCon)
  , inst [] (\ _ -> [] ==> serializableProp boolCon)
  , inst [] (\ _ -> [] ==> serializableProp timeCon)
  ] ++ tupleInsts serializableProp serializableProp ++

    -- TaskTarget instances
  [ inst [] (\ _ -> [] ==> taskTargetProp atomCon)
  ] ++ tupleInsts (atomCon ~~) taskTargetProp ++

    -- LinkTarget instances
  [ inst [] (\ _ -> [] ==> linkTargetProp nodeCon)

    -- SendTarget instances
  , inst [] (\ _ -> [] ==> sendTargetProp atomCon)
  , inst [] (\ _ -> [] ==> sendTargetProp taskCon)
  , inst [] (\ _ -> [] ==> sendTargetProp taskHandleCon)
  ] ++ tupleInsts (atomCon ~~) sendTargetProp ++

  -- NodeSpec things
  [ inst [] (\_ -> [] ==> nodeSpecProp ipv4Con)
  , inst [] (\_ -> [] ==> nodeSpecProp unitCon)

    -- Arith instances
  , inst [] (\ _ -> [] ==> arithProp intCon)
  , inst [] (\ _ -> [] ==> arithProp timeCon)

    -- Eq instances
  , inst [] (\ _ -> [] ==> eqProp intCon)
  , inst [] (\ _ -> [] ==> eqProp timeCon)
  , inst [] (\ _ -> [] ==> eqProp atomCon)
  , inst [] (\ _ -> [] ==> eqProp stringCon)
  , inst [] (\ _ -> [] ==> eqProp ipv4Con)

    -- Cmp instances
    -- NOTE: when adding Cmp instances, make sure that the type has a
    -- corresponding Eq constraint.  We don't check that "Eq a => Cmp a" here,
    -- as we're writing an interface by hand.
  , inst [] (\ _ -> [] ==> cmpProp intCon)
  , inst [] (\ _ -> [] ==> cmpProp timeCon)
  , inst [] (\ _ -> [] ==> cmpProp atomCon)
  , inst [] (\ _ -> [] ==> cmpProp stringCon)
  , inst [] (\ _ -> [] ==> cmpProp ipv4Con)

    -- Bits instances
  , inst [] (\ _ -> [] ==> bitsProp intCon)
  , inst [] (\ _ -> [] ==> bitsProp boolCon)

    -- Stringable instances
  , inst []    (\ _   -> [] ==> stringableProp stringCon)
  , inst []    (\ _   -> [] ==> stringableProp intCon)
  , inst []    (\ _   -> [] ==> stringableProp ipv4Con)
  , inst []    (\ _   -> [] ==> stringableProp atomCon)

    -- FromString instances
  , inst []    (\ _   -> [] ==> fromStringProp ipv4Con)
  , inst []    (\ _   -> [] ==> fromStringProp atomCon)
  ]
  where

  cxt ==> ty = (cxt,ty)

  inst ps k = Inst { iProp = schema }
    where
    params      = zipWith toParam [0 ..] ps
    toParam i n = TParam { tpUserName = Just (mkLocal n), tpIndex = i }

    (cxt,ty) = k [ TVar (TVBound p) | p <- params ]
    schema   = Forall params cxt ty

  tupVars :: [String]
  tupVars = map return [ 'a' .. 'z' ]

  -- generate instances for 2-9 tuples
  tupleInsts cxt prop =
    [ inst (take len tupVars) (\tys -> map cxt tys ==> prop (ttuple tys))
    | len <- [ 2 .. 9 ] ]
