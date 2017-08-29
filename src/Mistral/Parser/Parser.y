-- vim: ft=haskell

{

-- happy parsers produce a lot of warnings
{-# OPTIONS_GHC -w #-}

module Mistral.Parser.Parser where

import           Mistral.ModuleSystem.Name
import           Mistral.Parser.AST hiding (Atom)
import qualified Mistral.Parser.AST as P
import           Mistral.Parser.Lexer
import           Mistral.Parser.LexerCore
                     ( Lexeme, Token(..), Keyword(..), Symbol(..), Virt(..) )
import           Mistral.Parser.ParserCore
import           Mistral.Utils.Source

import Data.Monoid ( mempty, mappend, mconcat )

}

%token

  IDENT      { $$@(Located _ Ident {}) }
  OPERATOR   { $$@(Located _ Operator {}) }
  CIDENT     { $$@(Located _ CIdent{}) }
  ATOM       { $$@(Located _ Atom  {}) }
  DOTLIT     { $$@(Located _ DotT   {}) }
  COLONLIT   { $$@(Located _ ColonT {}) }
  NUMLIT     { $$@(Located _ Num   {}) }
  STRINGLIT  { $$@(Located _ String{}) }
  DOTSLASHLIT   { $$@(Located _ DotSlashT {}) }
  COLONSLASHLIT { $$@(Located _ ColonSlashT {}) }
  COLONDOTLIT   { $$@(Located _ ColonDotT {}) }

  'transitions' { Located $$ (KW KW_transitions) }

  'actions'  { Located $$ (KW KW_actions)  }
  'topology' { Located $$ (KW KW_topology) }
  'node'     { Located $$ (KW KW_node)     }
  'timeout'  { Located $$ (KW KW_timeout)  }
  'at'       { Located $$ (KW KW_at)       }
  'match'    { Located $$ (KW KW_match)    }
  'tagged'   { Located $$ (KW KW_tagged)   }

  'taskSet'  { Located $$ (KW KW_taskSet)  }
  'task'     { Located $$ (KW KW_task)     }
  'after'    { Located $$ (KW KW_after)    }
  'on'       { Located $$ (KW KW_on)       }
  'start'    { Located $$ (KW KW_start)    }
  'end'      { Located $$ (KW KW_end)      }

  'if'       { Located $$ (KW KW_if)       }
  'then'     { Located $$ (KW KW_then)     }
  'else'     { Located $$ (KW KW_else)     }

  'schedule' { Located $$ (KW KW_schedule) }
  'using'    { Located $$ (KW KW_using)    }

  'module'   { Located $$ (KW KW_module)   }
  'where'    { Located $$ (KW KW_where)    }
  'import'   { Located $$ (KW KW_import)   }

  'data'     { Located $$ (KW KW_data)     }
  'case'     { Located $$ (KW KW_case)     }
  'of'       { Located $$ (KW KW_of)       }

  'let'      { Located $$ (KW KW_let)      }
  'in'       { Located $$ (KW KW_in)       }

  '=>'  { Located $$ (Sym FArrowR) }
  '->'  { Located $$ (Sym ArrowR)  }
  '<-'  { Located $$ (Sym ArrowL)  }
  '<->' { Located $$ (Sym ArrowBi) }
  '='   { Located $$ (Sym Assign)  }
  ':'   { Located $$ (Sym Colon)   }
  ','   { Located $$ (Sym Comma)   }
  '('   { Located $$ (Sym ParenL)  }
  ')'   { Located $$ (Sym ParenR)  }
  '@'   { Located $$ (Sym AtSym)   }
  '?'   { Located $$ (Sym Question)}

  '['   { Located $$ (Sym BracketL)}
  ']'   { Located $$ (Sym BracketR)}

  '_'   { Located $$ (Sym Underscore) }

  '{'   { Located $$ (Sym BraceL)   }
  '}'   { Located $$ (Sym BraceR)   }
  ';'   { Located $$ (Sym Semi)     }
  '|'   { Located $$ (Sym Pipe)     }
  '||'  { Located $$ (Sym PipePipe) }
  '&&'  { Located $$ (Sym AndAnd)   }

  '\\'  { Located $$ (Sym Lambda)  }

  'v{'  { Located $$ (Virt VOpen)  }
  'v}'  { Located $$ (Virt VClose) }
  'v;'  { Located $$ (Virt VSep)   }


%name parseModule module

%tokentype { Lexeme }
%monad     { Parser }
%lexer     { lexerP } { Located _ Eof }

%right '||'
%right '&&'
%%

-- Names -----------------------------------------------------------------------

ident :: { Located Name }
  : IDENT { let Ident n = locThing $1
             in fmap (const (Parsed n (Local n))) $1 }

operator :: { Located Name }
  : OPERATOR { let Operator n = locThing $1
                in fmap (const (Parsed n (Local n))) $1 }

cident :: { Located Name }
  : CIDENT { let CIdent n = locThing $1
              in fmap (const (Parsed n (Local n))) $1 }

mod_name :: { Located Name }
  : cident { $1 }

opt_name :: { Maybe (Located Name) }
  : ident '='   { Just $1 }
  | {- empty -} { Nothing }


-- Modules ---------------------------------------------------------------------

module :: { Module }
  : 'module' mod_name 'where' mod_body
    { Module { modName    = $2
             , modImports = fst $4
             , modDecls   = snd $4
             }
    }

mod_body :: { ([Import],[TopDecl]) }
  : 'v{' mod_contents('v;') 'v}' { $2 }
  | '{'  mod_contents(';')  '}'  { $2 }

-- NOTE: use sep1_body for the import list here, so that happy can see through
-- the production, and notice when the import list will end
mod_contents(sep) :: { ([Import], [TopDecl]) }
  : sep1_body(sep, import) sep sep1(sep, top_decl) { (reverse $1,$3) }
  | sep1(sep, import)                              { ($1,[]) }
  | sep1(sep, top_decl)                            { ([],$1) }
  | {- empty -}                                    { ([],[]) }

import :: { Import }
  : 'import' mod_name
    { Import { impModule = locThing $2
             , impSource = getSource $1 `mappend` getSource $2 } }

top_decl :: { TopDecl }
  : decl(expr) { TDecl $1 }
  | data       { TData $1 }

decl(rhs) :: { Decl rhs }
  : bind(rhs) { mkBind $1 }
  | sig       { mkSig  $1 }


-- Declarations ----------------------------------------------------------------

decls :: { [Decl Expr] }
  : layout_block1(decl) { $1 }

decl :: { Decl Expr }
  : bind(expr) { mkBind $1 }
  | sig        { mkSig $1  }

-- bindings, parameterized over their RHS
bind(rhs) :: { Located (Bind rhs) }
  : ident list(ident) '=' rhs
    { Bind { bName   = $1
           , bSig    = Nothing
           , bParams = $2
           , bBody   = $4
           } `at` mconcat [getSource $1, $3, getSource $4] }

sig :: { Located Signature }
  : sep1(',', ident) ':' schema
    { Signature { sigNames  = $1
                , sigSchema = $3
                } `at` mconcat [getSource $1, $2, getSource $3] }


-- Types -----------------------------------------------------------------------

schema :: { Schema }
  : type '=>' type { mkSchema (mkContext $1) $3 }
  |           type { mkSchema []             $1 }

type :: { Type }
  : sep1('->', app_type) { mkTFuns $1 }

app_type :: { Type }
  : list1(atype) { mkTApp $1 }

atype :: { Type }
  : ident        { TSource (getSource $1) (TVar (locThing $1)) }
  | conident     { TSource (getSource $1) (TCon (locThing $1)) }
  | '(' sep(',', type) ')'
                 { TSource (mappend $1 $3) (mkTTuple $2)       }
  | '[' type ']' { TSource (mappend $1 $3) (TList $2)          }

-- The identifier for a constructor.
conident :: { Located Name }
  : cident { $1 }


-- Data Types ------------------------------------------------------------------

data :: { Data }
  : 'data' cident list(ident) '=' sep1('|', constr)
    { Data { dName    = $2
           , dParams  = map (TParam . locThing) $3
           , dConstrs = $5
           , dSource  = mconcat [$1,$4,getSource $5] } }

constr :: { Constr }
  : cident list(atype)
    { Constr { cName   = $1
             , cParams = $2
             , cSource = mappend (getSource $1) (getSource $2) } }


-- Expressions -----------------------------------------------------------------

expr :: { Expr }
  : actions      { $1 }
  | transitions  { $1 }
  | let_expr     { $1 }
  | lam_expr     { $1 }
  | if_then_else { $1 }
  | case_expr    { $1 }
  | app_expr     { $1 }
  | topology     { ESource (getSource $1) (ETopology (locThing $1)) }
  | task_set     { ESource (getSource $1) (ETaskSet  (locThing $1)) }
  | schedule     { ESource (getSource $1) (ESchedule (locThing $1)) }

let_expr :: { Expr }
  : 'let' decls 'in' expr
    { ESource (mconcat [$1, $3, getSource $4]) (ELet $2 $4) }

lam_expr :: { Expr }
  : '\\' list(ident) '->' expr
    { ESource (mconcat [ $1, $3, getSource $4 ]) (ELam $2 $4) }

if_then_else :: { Expr }
  : 'if' app_expr 'then' expr 'else' expr
    { ESource (mconcat [$1,$5,getSource $6]) (EIf $2 $4 $6) }

case_expr :: { Expr }
  : 'case' expr 'of' layout_block(case_arm)
    { ESource (mconcat [$1,$3,getSource $4]) (ECase $2 $4) }

case_arm :: { CaseArm }
  : apat '->' expr { CaseArm { cPat = $1, cBody = $3 } }

app_expr :: { Expr }
  : list1(aexpr) { mkEApp $1 }
  | list1(aexpr) lam_expr { mkEApp ($1 ++ [$2]) }

aexpr :: { Expr }
  : ident        { ESource (getSource $1) (EVar (locThing $1))  }
  | conident     { ESource (getSource $1) (ECon (locThing $1))  }
  | literal      { ESource (getSource $1) (ELit (locThing $1))  }
  | '_'          { ESource (getSource $1) EWildPat              }
  | '[' sep(',', expr) ']'
                 { ESource (mappend $1 $3) (EList $2) }
  | '(' sep(',', expr) ')'
                 { ESource (mappend $1 $3) (mkTuple $2)  }
  | '{' sep(';', stmt) '}'
                 { ESource (mappend $1 $3) (EActions $2) }

literal :: { Located Literal }
  : dotLit        { $1 }
  | colonLit      { $1 }
  | colonDotLit   { $1 }
  | colonSlashLit { $1 }
  | dotSlashLit   { $1 }
  | numLit        { $1 }
  | atom          { fmap LAtom $1 }
  | STRINGLIT     { let String s = locThing $1 in LString s `at` getSource $1 }

numLit :: { Located Literal }
  : NUMLIT       { let Num b n = locThing $1 in LNum n b `at` getSource $1 }

dotLit :: { Located Literal }
  : DOTLIT       { let DotT s = locThing $1 in LDot s `at` getSource $1 }

colonLit :: { Located Literal }
  : COLONLIT     { let ColonT s = locThing $1 in fmap (const (LTime (mkTime s))) $1 }

colonDotLit :: { Located Literal }
  : COLONDOTLIT     { let ColonDotT s = locThing $1 in fmap (const (LTime (mkTime s))) $1 }

colonSlashLit :: { Located Literal }
  : COLONSLASHLIT     { let ColonSlashT s = locThing $1 in fmap (const (LColonSlash s)) $1 }

dotSlashLit :: { Located Literal }
  : DOTSLASHLIT { let DotSlashT s = locThing $1 in fmap (const (LDotSlash s)) $1 }

atom :: { Located P.Atom }
  : ATOM { let Atom s = locThing $1 in fmap (const (P.Atom s)) $1 }


-- Action Statements -----------------------------------------------------------

actions :: { Expr }
  : 'actions' '{' '}'             { EActions [] }
  | 'actions' layout_block1(stmt) { EActions $2 }

-- action statements
stmt :: { Stmt }
  : ident '<-' expr
    { StSource (mconcat [getSource $1, $2, getSource $3]) (SBind $1 $3) }

    -- XXX should we remove this?  it doesn't always behave as you'd expect
  | ident '=' expr
    { StSource (mconcat [getSource $1, $2, getSource $3]) (SAssign $1 $3) }

  | expr { SSeq $1 }

  | let_stmt { $1 }

let_stmt :: { Stmt }
  : 'let' decls { StSource (mappend $1 (getSource $2)) (SLet $2) }


-- Patterns --------------------------------------------------------------------

pat :: { Pattern }
  : aexpr {% mkPat $1            }

apat :: { Pattern }
  : app_expr {% mkPat $1 }


-- State Transition ------------------------------------------------------------

transitions :: { Expr }
  : 'transitions'  layout_block1(tcase)
    { ESource (mappend $1 (getSource $2)) (ETransitions $2) }

tcase :: { Transition }
   : tpat '->' expr { Transition { trPattern = $1
                                 , trBody    = $3 } }

tpat :: { TPattern }
  : spat '?' apat
    { TPSource (mconcat [getSource $1, $2, getSource $3]) (Receive $1 $3) }

  | '?' apat
    { TPSource (mappend $1 (getSource $2)) (Receive SrcAny $2) }

  | 'at' expr
    { TPSource ($1 `mappend` getSource $2) (At $2) }

  | 'timeout' expr
    { TPSource ($1 `mappend` getSource $2) (Timeout $2) }

spat :: { SourcePred }
  : ident                       { SrcBind (locThing $1) }
  | 'match' ident               { SrcTaskHandle (locThing $2) }
  | 'tagged' tagOps             { SrcTags $2 }
  | '_'                         { SrcAny }


-- Comprehensions --------------------------------------------------------------

comp(result)
  : '[' result list1(comp_arm) ']' opt_tags
    { Comp { compResult = $2
           , compArms   = $3
           , compTags   = $5
           , compSource = mconcat [$1,$4,getSource $5] } }

comp_arm :: { CompArm }
  : '|' sep1(',', comp_stmt) { $2 }

comp_stmt :: { CompStmt }
  : pat '<-' expr  { CSSource (mconcat [getSource $1, $2, getSource $3])
                              (CSGen $1 $3) }

  | expr           { CSSource (getSource $1) (CSGuard $1) }


-- Topology --------------------------------------------------------------------

topology :: { Located Topology }
  : 'topology' layout_block1(topo_stmt)
    { Topology $2 `at` mappend $1 (getSource $2) }

topo_stmt :: { TopoStmt }
  : atopo_stmt       { $1                                  }
  | comp(atopo_stmt) { TSSource (getSource $1) (TSComp $1) }

atopo_stmt :: { TopoStmt }
  : node                  { $1 }
  | link                  { TSSource (getSource $1) (TSLink (locThing $1)) }
  | 'using' expr opt_tags { TSSource (mconcat [$1, getSource $2, getSource $3])
                                     (TSUsing $2 $3) }


node :: { TopoStmt }
  : opt_name 'node' aexpr opt_tags
    { TSSource (mconcat [getSource $1, getSource $3, getSource $4])
               (TSNode $1 (Node { nSpec = $3
                                , nTags = $4
                                })) }

link :: { Located Link }
  : link_target link_type link_target opt_tags { mkLink $1 $2 $3 $4 }

link_type :: { LinkType }
  : '<->' { Both }
  | '<-'  { In   }
  | '->'  { Out  }

link_target :: { LinkTarget }
  : ident  { LTName $1 }
  | atoms  { LTTag  $1 }

atoms :: { [Located P.Tag] }
  : '(' sep1(',', atom) ')' { map (fmap TagAtom) $2 }
  | atom                    { [fmap TagAtom $1]     }


-- TaskSets --------------------------------------------------------------------

task_set :: { Located TaskSet }
  : 'taskSet' layout_block1(task_stmt)
    { TaskSet { tsTasks  = $2 } `at` mappend $1 (getSource $2) }

task_stmt :: { TaskStmt }
  : atask_stmt       { $1                                    }
  | comp(atask_stmt) { TStSource (getSource $1) (TStComp $1) }

atask_stmt :: { TaskStmt }
  : task { $1 }

task :: { TaskStmt }
  : opt_name 'task' aexpr list(task_constraint) opt_tags
    { TStSource (mconcat [getSource $1, getSource $3, getSource $4, getSource $5])
                (TStTask $1 (Task { tBody        = $3
                                  , tConstraints = $4
                                  , tTags        = $5
                                  })) }

task_constraint :: { TaskConstraint }
  : 'after' task_constraint_seq ident
    { TCSource (mappend $1 (getSource $3)) (After $2 (locThing $3)) }
  | 'on' aexpr
    { TCSource (mappend $1 (getSource $2)) (On $2) }

task_constraint_seq :: { TaskConstraintSeq }
  : 'start'  { Start  }
  | 'end'    { End    }


-- Schedules -------------------------------------------------------------------

schedule :: { Located Schedule }
  : 'schedule' layout_block1(sched_stmt)
    { Schedule { sStmts = $2 } `at` mappend $1 (getSource $2) }

sched_stmt :: { SchedStmt }
  : expr 'using' expr
    { SSource (mconcat [getSource $1, $2, getSource $3]) (Using $1 $3) }
  | expr
    { SSource (getSource $1) (SExpr $1) }
  | sched_stmt '@' tags
    { SSource (mconcat [getSource $1, $2, getSource $3])
              (STag $1 (map locThing $3)) }


-- Tags ------------------------------------------------------------------------

opt_tags :: { [Located P.Tag] }
  : '@' tags    { $2 }
  | {- empty -} { [] }

tagOps :: { P.TagOp }
  : tagOps '&&' tagOps          { TOIntersect $1 $3 }
  | tagOps '||' tagOps          { TOUnion     $1 $3 }
  | tag                         { TOTag $1  }
  | '(' tagOps ')'              { $2 }

tags :: { [Located P.Tag] }
  : '(' sep1(',', tag) ')'  { $2   }
  | tag                     { [$1] }

tag :: { Located P.Tag }
  : atom  { fmap TagAtom $1 }
  | ident { fmap TagVar  $1 }

-- Utilities -------------------------------------------------------------------

list(p) :: { [p] }
  : list1(p)    { $1 }
  | {- empty -} { [] }

list1(p) :: { [p] }
  : list1_body(p) { reverse $1 }

list1_body(p) :: { [p] }
  : list1_body(p) p { $2 : $1 }
  | p               { [$1]    }


sep(punc,p) :: { [p] }
  : sep1(punc,p) { $1 }
  | {- empty -}  { [] }

sep1(punc,p) :: { [p] }
  : sep1_body(punc,p) { reverse $1 }

sep1_body(punc,p) :: { [p] }
  : sep1_body(punc,p) punc p { $3 : $1 }
  | p                        { [$1]    }

layout_block1(e) :: { e }
  : 'v{' sep1('v;', e) 'v}' { $2 }
  | '{'  sep1(';',  e) '}'  { $2 }

layout_block(e) :: { e }
  : 'v{' sep('v;', e) 'v}' { $2 }
  | '{'  sep(';',  e) '}'  { $2 }


{

}
