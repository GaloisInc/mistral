{-# LANGUAGE DeriveGeneric #-}

module Mistral.ModuleSystem.Name where

import Mistral.Utils.Panic ( panic )
import Mistral.Utils.Source ( NoSource(..) )

import Data.Function ( on )
import Data.List ( intercalate )
import Data.Serialize ( Serialize )
import Data.String ( IsString(fromString) )
import GHC.Generics ( Generic )
import System.FilePath ( (</>), (<.>) )


-- Names -----------------------------------------------------------------------

type Namespace = [String]

data Pass = TC -- ^ The typechecking pass
          | CG -- ^ The code generation pass
          | INT -- ^ The interpreter
          | Spec Name -- ^ The specialization pass
            deriving (Show,Eq,Ord,Generic)

instance Serialize Pass

data Name = Parsed String RealName
            -- ^ Names whose origin is a program
          | Generated RealName
            -- ^ Names generated during compilation
            deriving (Show,Generic)

instance Serialize Name

instance Eq Name where
  {-# INLINE (==) #-}
  (==) = (==) `on` nReal

  {-# INLINE (/=) #-}
  (/=) = (/=) `on` nReal

instance Ord Name where
  {-# INLINE compare #-}
  compare = compare `on` nReal

nReal :: Name -> RealName
nReal n = case n of
  Parsed _ rn  -> rn
  Generated rn -> rn

mapReal :: (RealName -> RealName) -> (Name -> Name)
mapReal f n = case n of
  Parsed s rn  -> Parsed s (f rn)
  Generated rn -> Generated (f rn)

-- | Real names are ones that the compiler will use when referring to things.
data RealName = Local String
                -- ^ A local name, from a let-binding or function argument.
                -- These names should not escape the module boundary.
              | Global Namespace String
                -- ^ A name with a qualified prefix, expected to be visible
                -- outside of the current module.
              | Fresh Pass Int
                -- ^ A name introduced to not clash with any source names.
                deriving (Show,Eq,Ord,Generic)

instance Serialize RealName

mkLocal :: String -> Name
mkLocal str = Generated (Local str)

isLocal :: Name -> Bool
isLocal n = case nReal n of
  Local{} -> True
  _       -> False

mkGlobal :: Namespace -> String -> Name
mkGlobal ns str = Generated (Global ns str)

isGlobal :: Name -> Bool
isGlobal n = case nReal n of
  Global{} -> True
  _        -> False

mkFresh :: Pass -> Int -> Name
mkFresh p i = Generated (Fresh p i)

unqual :: Name -> Name
unqual  = mapReal $ \ n -> case n of
  Local {}    -> n
  Global _ n' -> Local n'
  Fresh {}    -> n

-- | Promote a local name to a global name.
qual :: Namespace -> Name -> Name
qual ns = mapReal $ \ n -> case n of
  Local str -> Global ns str
  Global{}  -> n
  Fresh{}   -> n

nameStr :: Name -> String
nameStr n = case nReal n of
  Local s     -> s
  Global ns s -> intercalate "." (ns ++ [s])
  Fresh p i   -> "(" ++ show p ++ ":" ++ show i ++ ")"

nameNamespace :: Name -> Namespace
nameNamespace n = case nReal n of
  Local{}     -> []
  Global ns _ -> ns
  Fresh{}     -> []

modNamespace :: Name -> Namespace
modNamespace n = case nReal n of
  Local str   -> [str]
  Global ns s -> ns ++ [s]
  Fresh _ _   -> []


instance NoSource Name where
  {-# INLINE noSource #-}
  noSource n = n


modPath :: Name -> String
modPath n = case nReal n of
  Local str   -> str
  Global ns s -> foldr (</>) s ns
  Fresh _ _   -> panic "Mistral.ModuleSystem.Name"
                       [ "unexpected fresh variable for module name" ]

ifaceName :: IsString string => Name -> string
ifaceName n = fromString (modPath n <.> "mi")

objectName :: IsString string => Name -> string
objectName n = fromString (modPath n <.> "mo")

manglePass :: Pass -> String
manglePass p = case p of
  TC        -> "tc"
  CG        -> "cg"
  INT       -> "int"
  Spec orig -> "spec_" ++ mangleName orig



-- | Mangle the name, putting it into the given namespace.
mangleName :: IsString string => Name -> string
mangleName n = fromString $ case nReal n of
  Local str     -> "_"  ++ escape str
  Fresh p i     -> manglePass p ++ "_" ++ show i
  Global ns str -> intercalate "_" $ map escape $ ns ++ [str]
  where
  escape = concatMap escape1
  escape1 '_'  = "__"
  escape1 'z'  = "zz"
  escape1 '\'' = "zp"
  escape1 x    = [x]
