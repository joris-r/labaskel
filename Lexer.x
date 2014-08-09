 
{
module Lexer
  ( Token(..)
  , scan
  ) where
}

%wrapper "basic"


$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-
  
  -- Component and clauses
  
  MACHINE             { \s -> Machine }
  REFINEMENT          { \s -> Refinement }
  IMPLEMENTATION      { \s -> Implementation }
  REFINES             { \s -> Refines }
  SEES                { \s -> Sees }
  IMPORTS             { \s -> Imports }

  PROMOTES            { \s -> Promotes }

  SETS                { \s -> Sets }
  
  CONSTANTS           { \s -> Constants }
  ABSTRACT_CONSTANTS  { \s -> Abstract_constants }
  CONCRETE_CONSTANTS  { \s -> Concrete_constants }
  PROPERTIES          { \s -> Properties }
  VALUES              { \s -> Values }
  
  VARIABLES           { \s -> Variables }
  ABSTRACT_VARIABLES  { \s -> Abstract_variables }
  CONCRETE_VARIABLES  { \s -> Concrete_variables }
  INVARIANT           { \s -> Invariant }
  
  ASSERTIONS          { \s -> Assertions }
  
  INITIALISATION      { \s -> Initialisation }
  OPERATIONS          { \s -> Operations }
  LOCAL_OPERATIONS    { \s -> Local_operations }
  
  -- Various usage and misc
  
  "$0"                { \s -> PreviousValue }
  "="                 { \s -> Equality }
  ","                 { \s -> Comma }
  "("                 { \s -> Lpar }
  ")"                 { \s -> Rpar }
  ";"                 { \s -> Semicolon }
  "||"                { \s -> Parallel }
  "*"                 { \s -> Asterisk }
  "-"                 { \s -> Minus }
  "."                 { \s -> Dot }
  
  -- TODO experimental operators for LALR grammar
  
  ";;"  { \s -> LALR_Operation_separator }
  ";;;" { \s -> LALR_relation_composition }
  "|||" { \s -> LALR_parallel_product }
  
  -- Substitutions and operation
  
  "<--"               { \s -> Arrow_operation }
  
  skip                { \s -> Skip }
  
  "::"                { \s -> Subst_in }
  ":="                { \s -> Subst_assign }
  ":("                { \s -> Such_that }
  
  BEGIN               { \s -> Begin }
  END                 { \s -> End }
  
  IF                  { \s -> If }
  THEN                { \s -> Then }
  ELSE                { \s -> Else }
  ELSIF               { \s -> Elsif }
  
  PRE                 { \s -> Pre }
  ASSERT              { \s -> Assert }
  WHEN                { \s -> When }
  WHERE               { \s -> Where }
  SELECT              { \s -> Select }
  CASE                { \s -> Case }
  OF                  { \s -> Of }
  EITHER              { \s -> Either }
  VAR                 { \s -> Var }
  ANY                 { \s -> Any }
  CHOICE              { \s -> Choice }
  OR                  { \s -> Or }
  LET                 { \s -> Let }
  BE                  { \s -> Be }
  IN                  { \s -> In }
  WHILE               { \s -> While }
  DO                  { \s -> Do }
  VARIANT             { \s -> Variant }
  
  -- Predicate
  
  not                 { \s -> Not }
  "&"                 { \s -> Conjunction }
  or                  { \s -> Or }
  "!"                 { \s -> Forall }
  "#"                 { \s -> Exist }
  "=>"                { \s -> Implication }
  "<=>"               { \s -> Equivalence }

  -- Term operator
  
  "/="                { \s -> Not_equal }
  ":"                 { \s -> Colon }
  "/:"                { \s -> Not_in }
  "<:"                { \s -> Included_large }
  "/<:"               { \s -> Not_included_large }
  "<<:"               { \s -> Included }
  "/<<:"              { \s -> Not_included }
  "<"                 { \s -> Lt }
  "<="                { \s -> Lte }
  ">"                 { \s -> Gt }
  ">="                { \s -> Gte }

  
  -- Expression
  
  BOOL                { \s -> Set_bool }
  TRUE                { \s -> Token_true }
  FALSE               { \s -> Token_false }
  bool                { \s -> Ope_bool }
  
  INTEGER             { \s -> Integer }
  INT                 { \s -> Int }
  NATURAL             { \s -> Natural }
  NAT                 { \s -> Nat }
  NATURAL1            { \s -> Natural1 }
  NAT1                { \s -> Nat1 }
  MAXINT              { \s -> Maxint }
  MININT              { \s -> Minint }
  "+"                 { \s -> Plus }
  "/"                 { \s -> Div }
  mod                 { \s -> Mod }
  "**"                { \s -> Power }
  
  max                 { \s -> Max }
  min                 { \s -> Min }
  card                { \s -> Card }
  SIGMA               { \s -> Sigma }
  PI                  { \s -> Pi }
  
  "|->"               { \s -> Maplet }
  
  "{}"                { \s -> Empty_set }

  POW                 { \s -> Pow }
  POW1                { \s -> Pow1 }
  FIN                 { \s -> Fin }
  FIN1                { \s -> Fin1 }
  "{"                 { \s -> Lbrace }
  "}"                 { \s -> Rbrace }
  "|"                 { \s -> Vbar }
  ".."                { \s -> Interval }
  
  "\/"               { \s -> Union }
  "/\"               { \s -> Intersection }
  union               { \s -> Generalised_Union }
  inter               { \s -> Generalised_Inter }
  UNION               { \s -> Quantified_Union }
  INTER               { \s -> Quantified_Inter }
  
  "<->"               { \s -> Relation }
  
  id                  { \s -> Id }
  "~"                 { \s -> Inverse }
  prj1                { \s -> Prj1 }
  prj2                { \s -> Prj2 }
  "><"                { \s -> Direct_product }
  
  iterate             { \s -> Iterate }
  closure             { \s -> Closure }
  closure1            { \s -> Closure1 }
  
  dom                 { \s -> Dom }
  ran                 { \s -> Ran }
  "["                 { \s -> Lbracket }
  "]"                 { \s -> Rbracket }
  
  "<|"                { \s -> Restriction }
  "<<|"               { \s -> Anti_restriction }
  "|>"                { \s -> Co_restriction }
  "|>>"               { \s -> Anti_co_restriction }
  "<+"                { \s -> Override }
  
  "+->"               { \s -> Function_partial }
  "-->"               { \s -> Function_total }
  ">+>"               { \s -> Injection_partial }
  ">->"               { \s -> Injection_total }
  "+->>"              { \s -> Surjection_partial }
  "-->>"              { \s -> Surjection_total }
  ">->>"              { \s -> Bijection }
  
  "%"                 { \s -> Lambda }
  fnc                 { \s -> Fnc }
  rel                 { \s -> Rel }
  
  seq                 { \s -> Seq }
  seq1                { \s -> Seq1 }
  iseq                { \s -> Iseq }
  iseq1               { \s -> Iseq1 }
  perm                { \s -> Permutation }
  "[]"                { \s -> Empty_sequence }

  size                { \s -> Size }
  first               { \s -> First }
  last                { \s -> Last }
  front               { \s -> Front }
  tail                { \s -> Tail }
  rev                 { \s -> Rev }
  
  "^"                 { \s -> Concatenation }
  "->"                { \s -> Insertion_head }
  "<-"                { \s -> Insertion_queue }
  "/|\"               { \s -> Seq_keep }
  "\|/"               { \s -> Seq_elim }
  conc                { \s -> Conc }

  
  -- complex token
  
  -- TODO comment //... and /* ... */
  $white+                           ;
  $digit+                           { \s -> Number (read s) }
  [$alpha \_ ][$alpha $digit \_]*   { \s -> Identifier s }

  -- I'm not going to implement that right now
  
  -- USES                { \s -> Uses }
  -- EXTENDS             { \s -> Extends }
  -- INCLUDES            { \s -> Includes }
  -- DEFINITIONS         { \s -> Definitions }
  -- CONSTRAINTS         { \s -> Constraints }
  
  -- TODO string litterate
  -- STRING
  
  -- succ
  -- pred
  
  -- struct              { \s -> Struct }
  -- rec                 { \s -> Rec }
  -- "'"
  
  -- ">+>>"              { \s -> Bijection_partial }
  
  -- tree
  -- const
  -- top
  -- sons
  -- prefix
  -- postfix
  -- sizet
  -- mirror
  -- rank
  -- father
  -- son
  -- subtree
  -- arity
  -- bin
  -- left
  -- right
  -- infix


{
data Token =
  Machine |
  Refinement |
  Refines |
  Implementation |
  Constraints |
  Sets |
  Constants |
  Abstract_variables |
  Concrete_constants |
  Properties |
  Values |
  Promotes |
  Sees |
  Imports |
  Variables |
  Abstract_constants |
  Concrete_variables |
  Invariant |
  Initialisation |
  Assertions |
  Operations |
  Local_operations |
  Begin |
  End |
  Skip |
  If |
  Then |
  Pre |
  Assert |
  Else |
  Elsif |
  When |
  Where |
  Select |
  Case |
  Of |
  Either |
  Var |
  Any |
  Choice |
  Let |
  Be |
  In |
  While |
  Do |
  Variant |
  Ope_bool |
  Set_bool |
  Maxint |
  Minint |
  Conjunction |
  Semicolon |
  Parallel |
  Implication |
  Equivalence |
  Maplet |
  Not_in |
  Not_included_large |
  Not_included |
  Included |
  Included_large |
  Not_equal |
  Subst_in |
  Subst_assign |
  Arrow_operation |
  Relation |
  Union |
  Intersection |
  Empty_set |
  Empty_sequence |
  Gt |
  Lt |
  Gte |
  Lte |
  Interval |
  Anti_restriction |
  Anti_co_restriction |
  Restriction |
  Co_restriction |
  Override |
  Direct_product |
  Surjection_partial |
  Surjection_total |
  Bijection |
  Function_partial |
  Function_total |
  Injection_partial |
  Injection_total |
  Insertion_head |
  Insertion_queue |
  Seq_keep |
  Seq_elim |
  Not |
  Forall |
  Or |
  Exist |
  Equality |
  Comma |
  Colon |
  Vbar |
  Power |
  Asterisk |
  Pow |
  Pow1 |
  Fin |
  Fin1 |
  Generalised_Union |
  Generalised_Inter |
  Quantified_Union |
  Quantified_Inter |
  Fnc |
  Rel |
  Natural |
  Nat |
  Natural1 |
  Nat1 |
  Integer |
  Int |
  Min |
  Max |
  Div |
  Mod |
  Card |
  Sigma |
  Pi |
  Dom |
  Ran |
  Id |
  Inverse |
  Iterate |
  Closure |
  Closure1 |
  Prj1 |
  Prj2 |
  Lambda |
  Seq1 |
  Seq |
  Iseq |
  Iseq1 |
  Permutation |
  Concatenation |
  Size |
  Rev |
  First |
  Last |
  Tail |
  Front |
  Conc |
  Minus |
  Plus |
  Token_true |
  Token_false |
  Lpar |
  Rpar |
  Lbrace |
  Rbrace |
  Lbracket |
  Rbracket |
  Such_that |
  PreviousValue |
  Dot |

  Identifier String |
  Number Integer |
  
  -- TODO experimental operators for LALR grammar
  LALR_Operation_separator |
  LALR_relation_composition |
  LALR_parallel_product

  -- I'm not going to implement that right now
  -- Uses |
  -- Extends |
  -- Includes |
  -- Definitions |
  -- Struct |
  -- Rec |
  -- Bijection_partial |

  deriving (Eq,Show)

scan = alexScanTokens
}