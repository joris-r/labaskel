
{
module Parser (parse) where
import Lexer
import BTree
} 

-- TODO try to do a more complex grammar than make the difference between
--      expression of set, boolean and number

{-

     Notes on the grammar (as specified in the AtelierB documentation)


# Conflict in the use of the comma

In the places listed below, there is a notion of list of expression separated
by comma. But the pair expression already define list of expression, so it's a
problem because that's not possible to choose between the comma of
the list of expression and the comma of the pair.

 - in "Substitution : VariablesList SUBST_ASSIGN ExpressionList"
   which is the simple substitution "become equals to"
   
 - in "Substitution : VariablesList ARROW_OPERATION Ident LPAR ExpressionList RPAR"
   which is an operation call
 - in "Substitution : Ident LPAR ExpressionList RPAR"
   which is an operation call
   
 - in "Expression : LBRACE ExpressionList RBRACE"
   which is an expression of a set by extension
   
 - in "Expression : LBRACKET ExpressionList RBRACKET"
   which is a sequence by extension

Possible solutions:
 - Remove the notion of list of expression and just use an expression instead.
   In this way the accepted language remains the same.
   What is the impact of this on sets or sequences defined by extension?
 - only use "|->" as pair syntax, thus the comma can be use in expression list
 - add parenthesis around to pair or tuple in order to separate the two
   contexts (not sure it's enough)
   
-}

%name parse
%tokentype {Token}
%error {parseError}

%left LBRACKET LPAR -- TODO remove me
%left PARALLEL SEMICOLON LALR_RELATION_COMPOSITION LALR_PARALLEL_PRODUCT -- 20
%left IMPLICATION -- 30
%left CONJUNCTION OR -- 40
%left COMMA -- 115
%left RELATION FUNCTION_PARTIAL FUNCTION_TOTAL INJECTION_PARTIAL INJECTION_TOTAL SURJECTION_PARTIAL SURJECTION_TOTAL BIJECTION -- 125
%left EQUIVALENCE MAPLET UNION INTERSECTION DIRECT_PRODUCT RESTRICTION ANTI_RESTRICTION CO_RESTRICTION ANTI_CO_RESTRICTION OVERRIDE CONCATENATION INSERTION_HEAD INSERTION_QUEUE SEQ_KEEP SEQ_ELIM -- 160
%left INTERVAL -- 170
%left PLUS MINUS -- 180
%left ASTERISK MOD DIV -- 190
%right POWER -- 200
%left INVERSE -- 230

%token

MACHINE { Machine }
REFINEMENT { Refinement }
REFINES { Refines }
IMPLEMENTATION { Implementation }
SETS { Sets }
CONSTANTS { Constants }
ABSTRACT_VARIABLES { Abstract_variables }
CONCRETE_CONSTANTS { Concrete_constants }
PROPERTIES { Properties }
VALUES { Values }
PROMOTES { Promotes }
SEES { Sees }
IMPORTS { Imports }
VARIABLES { Variables }
ABSTRACT_CONSTANTS { Abstract_constants }
CONCRETE_VARIABLES { Concrete_variables }
INVARIANT { Invariant }
INITIALISATION { Initialisation }
ASSERTIONS { Assertions }
OPERATIONS { Operations }
LOCAL_OPERATIONS { Local_operations }
BEGIN { Begin }
END { End }
SKIP { Skip }
IF { If }
THEN { Then }
PRE { Pre }
ASSERT { Assert }
ELSE { Else }
ELSIF { Elsif }
WHEN { When }
WHERE { Where }
SELECT { Select }
CASE { Case }
OF { Of }
EITHER { Either }
VAR { Var }
ANY { Any }
CHOICE { Choice }
LET { Let }
BE { Be }
IN { In }
WHILE { While }
DO { Do }
VARIANT { Variant }
OPE_BOOL { Ope_bool }
SET_BOOL { Set_bool }
MAXINT { Maxint }
MININT { Minint }
CONJUNCTION { Conjunction }
SEMICOLON { Semicolon }
PARALLEL { Parallel }
IMPLICATION { Implication }
EQUIVALENCE { Equivalence }
MAPLET { Maplet }
NOT_IN { Not_in }
NOT_INCLUDED_LARGE { Not_included_large }
NOT_INCLUDED { Not_included }
INCLUDED { Included }
INCLUDED_LARGE { Included_large }
NOT_EQUAL { Not_equal }
SUBST_IN { Subst_in }
SUBST_ASSIGN { Subst_assign }
SUCH_THAT { Such_that }
ARROW_OPERATION { Arrow_operation }
RELATION { Relation }
UNION { Union }
INTERSECTION { Intersection }
EMPTY_SET { Empty_set }
EMPTY_SEQUENCE { Empty_sequence }
GT { Gt }
LT { Lt }
GTE { Gte }
LTE { Lte }
INTERVAL { Interval }
ANTI_RESTRICTION { Anti_restriction }
ANTI_CO_RESTRICTION { Anti_co_restriction }
RESTRICTION { Restriction }
CO_RESTRICTION { Co_restriction }
OVERRIDE { Override }
DIRECT_PRODUCT { Direct_product }
SURJECTION_PARTIAL { Surjection_partial }
SURJECTION_TOTAL { Surjection_total }
BIJECTION { Bijection }
FUNCTION_PARTIAL { Function_partial }
FUNCTION_TOTAL { Function_total }
INJECTION_PARTIAL { Injection_partial }
INJECTION_TOTAL { Injection_total }
INSERTION_HEAD { Insertion_head }
INSERTION_QUEUE { Insertion_queue }
SEQ_KEEP { Seq_keep }
SEQ_ELIM { Seq_elim }
NOT { Not }
FORALL { Forall }
OR { Or }
EXIST { Exist }
EQUALITY { Equality }
COMMA { Comma }
COLON { Colon }
VBAR { Vbar }
POWER { Power }
ASTERISK { Asterisk }
POW { Pow }
POW1 { Pow1 }
FIN { Fin }
FIN1 { Fin1 }
GENERALISED_UNION { Generalised_Union }
GENERALISED_INTER { Generalised_Inter }
QUANTIFIED_UNION { Quantified_Union }
QUANTIFIED_INTER { Quantified_Inter }
FNC { Fnc }
REL { Rel }
NATURAL { Natural }
NAT { Nat }
NATURAL1 { Natural1 }
NAT1 { Nat1 }
INTEGER { Integer }
INT { Int }
MIN { Min }
MAX { Max }
DIV { Div }
MOD { Mod }
CARD { Card }
SIGMA { Sigma }
PI { Pi }
DOM { Dom }
RAN { Ran }
ID { Id }
INVERSE { Inverse }
ITERATE { Iterate }
CLOSURE { Closure }
CLOSURE1 { Closure1 }
PRJ1 { Prj1 }
PRJ2 { Prj2 }
LAMBDA { Lambda }
SEQ1 { Seq1 }
SEQ { Seq }
ISEQ { Iseq }
ISEQ1 { Iseq1 }
PERMUTATION { Permutation }
CONCATENATION { Concatenation }
SIZE { Size }
REV { Rev }
FIRST { First }
LAST { Last }
TAIL { Tail }
FRONT { Front }
CONC { Conc }
MINUS { Minus }
PLUS { Plus }
TOKEN_TRUE { Token_true }
TOKEN_FALSE { Token_false }
LPAR { Lpar }
RPAR { Rpar }
LBRACE { Lbrace }
RBRACE { Rbrace }
LBRACKET { Lbracket }
RBRACKET { Rbracket }
IDENTIFIER { Identifier $$ }
NUMBER { Number $$ }
DOT { Dot }
PREVIOUSVALUE { PreviousValue }

-- TODO experimental operators for LALR grammar
LALR_OPERATION_SEPARATOR { LALR_Operation_separator }
LALR_RELATION_COMPOSITION { LALR_relation_composition }
LALR_PARALLEL_PRODUCT { LALR_parallel_product }

{- I'm not going to implement that right now
USES { Uses }
EXTENDS { Extends }
INCLUDES { Includes }
CONSTRAINTS { Constraints }
DEFINITIONS { Definitions }
STRUCT { Struct }
REC { Rec }
BIJECTION_PARTIAL { Bijection_partial }
-}

%%

Component
  : MACHINE Ident ComponentClauses END
    { BMachine $2 $3 }
  | REFINEMENT Ident ComponentClauses END
    { BRefinement $2 $3 }
  | IMPLEMENTATION Ident ComponentClauses END
    { BImplementation $2 $3 }
    
ComponentClauses
  : {- empty -}
    { [] }
  | ComponentClauses REFINES Ident -- TODO mandatory for refinement and implementation
    { $1 ++ [BRefines $3] }
  | ComponentClauses IMPORTS Ident
    { $1 ++ [BImports $3] }
  | ComponentClauses SEES Ident
    { $1 ++ [BSees $3] }
  | ComponentClauses SETS SetDeclaration
    { $1 ++ [BSetClause $3] }
  | ComponentClauses CONSTANTS VariablesList
    { $1 ++ [BConcreteConstants $3] }
  | ComponentClauses CONCRETE_CONSTANTS VariablesList
    { $1 ++ [BConcreteConstants $3] }
  | ComponentClauses ABSTRACT_CONSTANTS VariablesList
    { $1 ++ [BAbstractConstants $3] }
  | ComponentClauses VARIABLES VariablesList
    { $1 ++ [BAbstractVariables $3] }
  | ComponentClauses ABSTRACT_VARIABLES VariablesList
    { $1 ++ [BAbstractVariables $3] }
  | ComponentClauses CONCRETE_VARIABLES VariablesList
    { $1 ++ [BConcreteVariables $3] }
  | ComponentClauses PROMOTES VariablesList
    { $1 ++ [BPromotes $3] }
  | ComponentClauses PROPERTIES Predicate
    { $1 ++ [BProperties $3] }
  | ComponentClauses INVARIANT Predicate
    { $1 ++ [BInvariant $3] }
  | ComponentClauses ASSERTIONS PredicateList
    { $1 ++ [BAssertions $3] }
  | ComponentClauses VALUES PredicateList
    { $1 ++ [BValues $3] }
  | ComponentClauses INITIALISATION Substitution
    { $1 ++ [BInitialisation $3] }
  | ComponentClauses OPERATIONS OperationsList
    { $1 ++ [BOperations $3] }
  | ComponentClauses LOCAL_OPERATIONS OperationsList
    { $1 ++ [BLocalOperations $3] }

Ident : IDENTIFIER { BIdent $1 }
    
PredicateList
  : Predicate
    { [$1] }
  | PredicateList SEMICOLON Predicate
    { $1 ++ [$3] }

SetDeclaration
  : Ident
    { [BCarrierSet $1] }
  | LBRACE VariablesList RBRACE
    { [BEnumeratedSet $2] }
  | SetDeclaration SEMICOLON Ident
    { $1 ++ [BCarrierSet $3] }
  | SetDeclaration SEMICOLON LBRACE VariablesList RBRACE
    { $1 ++ [BEnumeratedSet $4] }
    
VariablesList
  : Ident
    { [$1] }
  | VariablesList COMMA Ident
    { $1 ++ [$3] }

VariablesListProtected
  : Ident
    { [$1] }
  | LPAR VariablesList RPAR
    { $2 }

OperationsList
  : Operation
    { [$1] }
  | OperationsList LALR_OPERATION_SEPARATOR Operation -- TODO conflict
     { $1 ++ [$3] }

Operation
  : VariablesList ARROW_OPERATION Ident LPAR VariablesList RPAR EQUALITY Substitution
    { BOperation $1 $3 $5 $8 }
  | VariablesList ARROW_OPERATION Ident EQUALITY Substitution
    { BOperation $1 $3 [] $5 }
  | Ident LPAR VariablesList RPAR EQUALITY Substitution
    { BOperation [] $1 $3 $6 }
  | Ident EQUALITY Substitution
    { BOperation [] $1 [] $3 }
     
Substitution
  : BEGIN Substitution END
    { BSubstitutionBlock $2 }
  | SKIP
    { BSubstitutionSkip }
  | VariablesList SUBST_ASSIGN ExpressionList
    { BSubstitutionSimple $1 $3 }
  | PRE Predicate THEN Substitution END
    { BSubstitutionPreCondition $2 $4 }
  | ASSERT Predicate THEN Substitution END
    { BSubstitutionAssert $2 $4 }
  | CHOICE SubstitutionListWithOr END
    { BSubstitutionChoice $2 }
  | IF Predicate THEN Substitution Elsif END
    { BSubstitutionCond BIf (($2,$4):$5) Nothing }
  | IF Predicate THEN Substitution Elsif ELSE Substitution END
    { BSubstitutionCond BIf (($2,$4):$5) (Just $7) }
  | SELECT Predicate THEN Substitution WhenSelect END
    { BSubstitutionCond BSelect (($2,$4):$5) Nothing }
  | SELECT Predicate THEN Substitution WhenSelect ELSE Substitution END
    { BSubstitutionCond BSelect (($2,$4):$5) (Just $7) }
  | CASE Expression OF
      EITHER Expression THEN Substitution 
      OrThen
      END END
    { BSubstitutionCase $2 (($5,$7):$8) Nothing }
  | CASE Expression OF
      EITHER Expression THEN Substitution
      OrThen
      ELSE Substitution
      END END
    { BSubstitutionCase $2 (($5,$7):$8) (Just $10) }
  | ANY VariablesList WHERE Predicate THEN Substitution END
    { BSubstitutionSpecVar BAny $2 $4 $6 }
  | LET VariablesList BE Predicate IN Substitution END  -- TODO check Predicate restriction
    { BSubstitutionSpecVar BLet $2 $4 $6 }
  | VariablesList SUBST_IN Expression
    { BSubstitutionBecomeIn $1 $3 }
  | VariablesList SUCH_THAT Predicate RPAR
    { BSubstitutionSuchThat $1 $3 }
  | VAR VariablesList IN Substitution END
    { BSubstitutionVar $2 $4 }
  | Substitution SEMICOLON Substitution
    { BSubstitutionCompo BOpSubSeq $1 $3 }
  | Substitution PARALLEL Substitution
    { BSubstitutionCompo BOpSubParal $1 $3 }
  | VariablesList ARROW_OPERATION Ident LPAR ExpressionList RPAR
    { BSubstitutionOpeCall $1 $3 $5 }
  | VariablesList ARROW_OPERATION Ident
    { BSubstitutionOpeCall $1 $3 [] }
  | Ident LPAR ExpressionList RPAR
    { BSubstitutionOpeCall [] $1 $3 }
  | Ident
    { BSubstitutionOpeCall [] $1 [] }
  | WHILE Predicate DO Substitution INVARIANT Predicate VARIANT Expression END
    { BSubstitutionWhile $2 $4 $6 $8 }
    
OrThen
  : {- empty -}
    { [] }
  | OrThen OR Expression THEN Substitution
    { $1 ++ [($3,$5)] }
    
Elsif
  : {- empty -}
    { [] }
  | Elsif ELSIF Predicate THEN Substitution
    { $1 ++ [($3,$5)] }
    
WhenSelect
  : {- empty -}
    { [] }
  | WhenSelect WHEN Predicate THEN Substitution
    { $1 ++ [($3,$5)] }
    
SubstitutionListWithOr
  : Substitution
    { [$1] }
  | SubstitutionListWithOr OR Substitution
    { $1 ++ [$3] }

ExpressionList
  : Expression
    { [$1] }
  | ExpressionList COMMA Expression
    { $1 ++ [$3] }

Predicate
  : LPAR Predicate RPAR
    { $2 }
  | NOT LPAR Predicate RPAR
    { BUnaryPredicate BNegation $3 }
  | Predicate CONJUNCTION Predicate
    { BBinaryPredicate BConjunction $1 $3 }
  | Predicate OR Predicate
    { BBinaryPredicate BDisjunction $1 $3 }
  | Predicate IMPLICATION Predicate
    { BBinaryPredicate BImplication $1 $3 }
  | Predicate EQUIVALENCE Predicate
    { BBinaryPredicate BEquivalence $1 $3 }
    
  | FORALL VariablesListProtected DOT LPAR Predicate RPAR
    { BQuantifiedPredicate BUniversal $2 $5 }
  | EXIST VariablesListProtected DOT LPAR Predicate RPAR
    { BQuantifiedPredicate BExistential $2 $5 }
    
  | Expression EQUALITY Expression
    { BComparisonPredicate BEquality $1 $3 }
  | Expression NOT_EQUAL Expression
    { BComparisonPredicate BNonEquality $1 $3 }
    
  | Expression COLON Expression
    { BComparisonPredicate BMembership $1 $3 }
  | Expression NOT_IN Expression
    { BComparisonPredicate BNonMembership $1 $3 }
    
  | Expression INCLUDED_LARGE Expression
    { BComparisonPredicate BInclusion $1 $3 }
  | Expression INCLUDED Expression
    { BComparisonPredicate BStrictInclusion $1 $3 }
  | Expression NOT_INCLUDED_LARGE Expression
    { BComparisonPredicate BNonInclusion $1 $3 }
  | Expression NOT_INCLUDED Expression
    { BComparisonPredicate BNonStrictInclusion $1 $3 }
    
  | Expression LTE Expression
    { BComparisonPredicate BInequality $1 $3 }
  | Expression LT Expression
    { BComparisonPredicate BStrictInequality $1 $3 }
  | Expression GTE Expression
    { BComparisonPredicate BReverseInequality $1 $3 }
  | Expression GT Expression
    { BComparisonPredicate BStrictReverseInequality $1 $3 }

Expression
  : Ident
    { BIdentifier $1 BCurrent }
  | Ident PREVIOUSVALUE
    { BIdentifier $1 BPrevious }
  | LPAR Expression RPAR
    { $2 }
    
  | OPE_BOOL LPAR Predicate RPAR
    { BBoolConversion $3 }
    
  | NUMBER
    { BNumber $1 }
    
  | MINUS Expression
    { BUnaryExpression BOpposite $2 }
  | Expression PLUS Expression
    { BBinaryExpression BAddition $1 $3 }
  | Expression MINUS Expression
    { BBinaryExpression BSubstration $1 $3 }
    
  | Expression ASTERISK Expression
    { BBinaryExpression BAsterisk $1 $3 }
  | Expression DIV Expression
    { BBinaryExpression BDivision $1 $3 }
  | Expression MOD Expression
    { BBinaryExpression BModulo $1 $3 }
  | Expression POWER Expression
    { BBinaryExpression BPower $1 $3 }
    
  | MAX LPAR Expression RPAR
    { BUnaryExpression BMaximum $3 }
  | MIN LPAR Expression RPAR
    { BUnaryExpression BMinimum $3 }
  | CARD LPAR Expression RPAR
    { BUnaryExpression BCardinality $3 }
    
  | SIGMA VariablesListProtected DOT LPAR Predicate VBAR Expression RPAR
    { BQuantifiedExpression BSum $2 $5 $7 }
  | PI VariablesListProtected DOT LPAR Predicate VBAR Expression RPAR
    { BQuantifiedExpression BProduct $2 $5 $7 }
    
  | Expression MAPLET Expression
    { BBinaryExpression BPair $1 $3 }
    
-- TODO this rule make a conflict (with the COMMA of ExpressionList ?)
--  | Expression COMMA Expression
--    { BBinaryExpression BPair $1 $3 }
    
  | POW LPAR Expression RPAR
    { BUnaryExpression BPowerSet $3 }
  | POW1 LPAR Expression RPAR
    { BUnaryExpression BNonEmptyPowerSet $3 }
  | FIN LPAR Expression RPAR
    { BUnaryExpression BFinitePowerSet $3 }
  | FIN1 LPAR Expression RPAR
    { BUnaryExpression BNonEmptyFinitePowerSet $3 }
    
  | LBRACE VariablesList VBAR Predicate RBRACE
    { BSetComprehension $2 $4 }
  | LBRACE LBRACE ExpressionList RBRACE RBRACE -- TODO this rule make a conflict
    { BSetExtension $3 }
  
  | Expression INTERVAL Expression
    { BBinaryExpression BInterval $1 $3 }
    
  | Expression UNION Expression
    { BBinaryExpression BUnion $1 $3 }
  | Expression INTERSECTION Expression
    { BBinaryExpression BIntersection $1 $3 }
  | GENERALISED_UNION LPAR Expression RPAR
    { BUnaryExpression BGeneralizedUnion $3 }
  | GENERALISED_INTER LPAR Expression RPAR
    { BUnaryExpression BGeneralizedIntersection $3 }
  | QUANTIFIED_UNION VariablesListProtected DOT LPAR Predicate VBAR Expression RPAR
    { BQuantifiedExpression BQuantifiedUnion $2 $5 $7 }
  | QUANTIFIED_INTER VariablesListProtected DOT LPAR Predicate VBAR Expression RPAR
    { BQuantifiedExpression BQuantifiedIntersection $2 $5 $7 }
  
  | Expression RELATION Expression
    { BBinaryExpression BRelation $1 $3 }
    
  | ID LPAR Expression RPAR
    { BUnaryExpression BIdentity $3 }
  | Expression INVERSE
    { BUnaryExpression BInverse $1 }  
  | PRJ1 LPAR Expression COMMA Expression RPAR
    { BBinaryExpression BLeftProjection $3 $5 }
  | PRJ2 LPAR Expression COMMA Expression RPAR
    { BBinaryExpression BRightProjection $3 $5 }
  | Expression LALR_RELATION_COMPOSITION Expression -- TODO conflict
    { BBinaryExpression BComposition $1 $3 }
  | Expression DIRECT_PRODUCT Expression
    { BBinaryExpression BDirectProduct $1 $3 }
  | Expression LALR_PARALLEL_PRODUCT Expression  -- TODO conflict
    { BBinaryExpression BParallelProduct $1 $3 }
  | ITERATE LPAR Expression COMMA Expression RPAR
    { BBinaryExpression BIteration $3 $5 }
  | CLOSURE LPAR Expression RPAR
    { BUnaryExpression BClosure $3 }
  | CLOSURE1 LPAR Expression RPAR
    { BUnaryExpression BNonReflexiveClosure $3 }
  
  | DOM LPAR Expression RPAR
    { BUnaryExpression BDomain $3 }
  | RAN LPAR Expression RPAR
    { BUnaryExpression BRange $3 }
  | Expression LBRACKET Expression RBRACKET  -- TODO conflict (precedence added)
    { BBinaryExpression BImage $1 $3 }
  
  | Expression RESTRICTION Expression
    { BBinaryExpression BDomainRestriction $1 $3 }
  | Expression ANTI_RESTRICTION Expression
    { BBinaryExpression BDomainSubstraction $1 $3 }
  | Expression CO_RESTRICTION Expression
    { BBinaryExpression BRangeRestriction $1 $3 }
  | Expression ANTI_CO_RESTRICTION Expression
    { BBinaryExpression BRangeSubstraction $1 $3 }
  | Expression OVERRIDE Expression
    { BBinaryExpression BOverloading $1 $3 }
  
  | Expression FUNCTION_PARTIAL Expression
    { BBinaryExpression BPartialFunction $1 $3 }
  | Expression FUNCTION_TOTAL Expression
    { BBinaryExpression BTotalFunction $1 $3 }
  | Expression INJECTION_PARTIAL Expression
    { BBinaryExpression BPartialInjection $1 $3 }
  | Expression INJECTION_TOTAL Expression
    { BBinaryExpression BTotalInjection $1 $3 }
  | Expression SURJECTION_PARTIAL Expression
    { BBinaryExpression BPartialSurjection $1 $3 }
  | Expression SURJECTION_TOTAL Expression
    { BBinaryExpression BTotalSurjection $1 $3 }
  | Expression BIJECTION Expression
    { BBinaryExpression BTotalBijection $1 $3 }
    
  | LAMBDA VariablesListProtected DOT LPAR Predicate VBAR Expression RPAR
    { BQuantifiedExpression BLambdaExpression $2 $5 $7 }
  | Expression LPAR Expression RPAR  -- TODO conflict (precedence added)
    { BBinaryExpression BApplication $1 $3 }
  | FNC LPAR Expression RPAR
    { BUnaryExpression BFunctionTransformation $3 }
  | REL LPAR Expression RPAR
    { BUnaryExpression BRelationTransformation $3 }
  
  | SEQ LPAR Expression RPAR
    { BUnaryExpression BSequence $3 }
  | SEQ1 LPAR Expression RPAR
    { BUnaryExpression BNonEmptySequence $3 }
  | ISEQ LPAR Expression RPAR
    { BUnaryExpression BInjectiveSequence $3 }
  | ISEQ1 LPAR Expression RPAR
    { BUnaryExpression BNonEmptyInjectiveSequence $3 }
  | PERMUTATION LPAR Expression RPAR
    { BUnaryExpression BPermutation $3 }
  | LBRACKET ExpressionList RBRACKET
    { BSequenceExtension $2 }
    
  | SIZE LPAR Expression RPAR
    { BUnaryExpression BSize $3 }
  | FIRST LPAR Expression RPAR
    { BUnaryExpression BFirst $3 }
  | LAST LPAR Expression RPAR
    { BUnaryExpression BLast $3 }
  | FRONT LPAR Expression RPAR
    { BUnaryExpression BFront $3 }
  | TAIL LPAR Expression RPAR
    { BUnaryExpression BTail $3 }
  | REV LPAR Expression RPAR
    { BUnaryExpression BRev $3 }
  
  | Expression CONCATENATION Expression
    { BBinaryExpression BConcatenation $1 $3 }
  | Expression INSERTION_HEAD Expression
    { BBinaryExpression BHeadInsertion $1 $3 }
  | Expression INSERTION_QUEUE Expression
    { BBinaryExpression BTailInsertion $1 $3 }
  | Expression SEQ_KEEP Expression
    { BBinaryExpression BHeadRestriction $1 $3 }
  | Expression SEQ_ELIM Expression
    { BBinaryExpression BTailRestriction $1 $3 }
  | CONC LPAR Expression RPAR
    { BUnaryExpression BGeneralizedConcatenation $3 }
  
  | TOKEN_TRUE
    { BIdentifier (BIdent "TRUE") BCurrent }
  | TOKEN_FALSE
    { BIdentifier (BIdent "FALSE") BCurrent }
  | MAXINT
    { BIdentifier (BIdent "MAXINT") BCurrent }
  | MININT
    { BIdentifier (BIdent "MININT") BCurrent }
    
  | EMPTY_SEQUENCE
    { BIdentifier (BIdent "{}") BCurrent }  -- TODO do something else with me
  | EMPTY_SET
    { BIdentifier (BIdent "[]") BCurrent }  -- TODO do something else with me
  | INTEGER
    { BIdentifier (BIdent "INTEGER") BCurrent }
  | NATURAL
    { BIdentifier (BIdent "NATURAL") BCurrent }
  | NATURAL1
    { BIdentifier (BIdent "NATURAL1") BCurrent }
  | NAT
    { BIdentifier (BIdent "NAT") BCurrent }
  | NAT1
    { BIdentifier (BIdent "NAT1") BCurrent }
  | INT
    { BIdentifier (BIdent "INT") BCurrent }
  | SET_BOOL
    { BIdentifier (BIdent "BOOL") BCurrent }

{
parseError :: [Token] -> a
parseError _ = error "Syntax error : not a B component (or not implemented)."
}

