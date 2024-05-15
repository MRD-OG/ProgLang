{
module LangParser where
import LangTokens (LangToken(..))
import LangAst (
    LangAstProgram
  , LangAstInstruction(..)
  , LangElseIfBlock(..)
  , LangAstDeclaration(..)
  , LangAstAssignment(..)
  , LangAstVariableType(..)
  , LangAstSetField(..)
  , LangAstBooleanLogic(..)
  , LangAstComparrison(..)
  , LangAstExp(..)
  , LangAstDotMethod(..)
  , LangAstAtomicValue(..)
  , LangAstBufferOperation(..))
}

%name parseLangTokens
%tokentype {LangToken}
%error {langParseError}

%token

  "(" {LangTokenLParen}
  ")" {LangTokenRParen}
  "[" {LangTokenLSquare}
  "]" {LangTokenRSquare}
  "{" {LangTokenLCurly}
  "}" {LangTokenRCurly}
  
  ";" {LangTokenSemiColon}
  "," {LangTokenComma}

  "." {LangTokenDot}

  "+" {LangTokenAddition}
  "-" {LangTokenSubtraction}
  "*" {LangTokenMultiplication}
  "/" {LangTokenDividision}
  "%" {LangTokenModulo}
  "^" {LangTokenPower}

  "==" {LangTokenEqualityE}
  "!=" {LangTokenEqualityNE}
  ">=" {LangTokenEqualityGTE}
  "<=" {LangTokenEqualityLTE}
  ">" {LangTokenEqualityGT}
  "<" {LangTokenEqualityLT}
  
  "&&" {LangTokenLogicalAnd}
  "||" {LangTokenLogicalOr}
  "!" {LangTokenLogicalNot}

  "=" {LangTokenAssign}

  "int type" {LangTokenIntType}
  "string type" {LangTokenStringType}
  "bool type" {LangTokenBoolType}
  "nodeset type" {LangTokenNodesetType}
  "relationset type" {LangTokenRelationsetType}
  "node type" {LangTokenNodeType}
  "relation type" {LangTokenRelationType}

  "labels" {LangTokenLabelOption}
  "clearBuffer" {LangTokenClearBufferCommand}
  "pushHeader" {LangTokenPushHeaderCommand}
  "pushData" {LangTokenPushDataCommand}
  
  "for" {LangTokenFor}
  "from" {LangTokenFrom}
  "to" {LangTokenTo}
  "step" {LangTokenStep}

  "if" {LangTokenIf}
  "elseif" {LangTokenElseif}
  "else" {LangTokenElse}

  "import" {LangTokenImport}

  "identifier" {LangTokenIdentifier $$}
  "string value" {LangTokenString $$}
  "integer value" {LangTokenNum $$}
  "boolean value" {LangTokenBool $$}

  %nonassoc "="

  %left "||"
  %left "&&"

  %nonassoc "!"

  %nonassoc "==" "!=" ">=" "<=" ">" "<"
  
  %left "-"
  %left "+"
  %left "*"
  %left "/" "%"
  %left "^"
  %left "."

  %%

  Program : InstructionList {$1}

  InstructionList : {[]}
                  | InstructionList Instruction {$1 ++ [$2]}

  Instruction : Declaration ";" {Declaration $1}
              | Assignment ";" {Assignment $1}
              | BufferOperation ";" {BufferOperation $1}
              | "import" "identifier" ";" {Import $2}
              | ForLoop {$1}
              | ConditionBlock {$1}

  ForLoop : "for" "identifier" "from" Exp "{" InstructionList "}" {EnhancedFor $2 $4 $6}
          | "for" "identifier" "from" Exp "to" Exp "step" Exp "{" InstructionList "}" {ForXIn $2 $4 $6 $8 $10}
          | "for" "identifier" "from" Exp "to" Exp "{" InstructionList "}" {ForXIn $2 $4 $6 (Atomic(AtomicInt 1)) $8}

  ConditionBlock : "if" "(" BooleanLogic ")" "{" InstructionList "}" ElseifBlockList "else" "{" InstructionList "}" {If $3 $6 $8 (Just $11)}
                 | "if" "(" BooleanLogic ")" "{" InstructionList "}" ElseifBlockList {If $3 $6 $8 Nothing}
                 
  ElseifBlockList : {[]}
                  | ElseifBlockList ElseifBlock {$1 ++ [$2]}

  ElseifBlock : "elseif" "(" BooleanLogic ")" "{" InstructionList "}" {ElseIf $3 $6}

  Declaration : "int type" "identifier" "=" BooleanLogic {ValueDeclaration IntegerType $2 $4}
              | "string type" "identifier" "=" BooleanLogic {ValueDeclaration StringType $2 $4}
              | "bool type" "identifier" "=" BooleanLogic {ValueDeclaration BooleanType $2 $4}
              | "node type" "identifier" "=" BooleanLogic {ValueDeclaration NodeType $2 $4}

              | "relation type" "identifier" "=" BooleanLogic {ValueDeclaration RelationType $2 $4}
              | "nodeset type" "identifier" "=" "[" SetFieldList "]" "labels" "=" "boolean value" {NodeSetDeclaration $2 $5 $9}
              | "nodeset type" "identifier" "=" "[" SetFieldList "]" {NodeSetDeclaration $2 $5 False}
              | "relationset type" "identifier" "=" "[" SetFieldList "]" {RelationSetDeclaration $2 $5}

  SetFieldList : {[]}
               | SetFieldList SetField "," {$1 ++ [$2]}
               | SetFieldList SetField {$1 ++ [$2]}

  SetField : "int type" "identifier" {IntegerField $2}
           | "string type" "identifier" {StringField $2}
           | "bool type" "identifier" {BooleanField $2}

  Assignment : Exp "=" BooleanLogic {ValueAssignment $1 $3}

  BooleanLogic : "(" BooleanLogic ")" {$2}
               | BooleanLogic "&&" BooleanLogic {And $1 $3}
               | BooleanLogic "||" BooleanLogic {Or $1 $3}
               | Comparrison {Comparrison $1}
               | "!" BooleanLogic {Not $2}

  Comparrison : "(" Comparrison ")" {$2}
              | Comparrison "==" Comparrison {E $1 $3}
              | Comparrison "!=" Comparrison {NE $1 $3}
              | Exp ">=" Exp {GTE $1 $3}
              | Exp "<=" Exp {LTE $1 $3}
              | Exp ">" Exp {LangAst.GT $1 $3}
              | Exp "<" Exp {LangAst.LT $1 $3}
              | Exp {Expression $1}

  Exp : "(" Exp ")" {$2}
      | Exp "-" Exp {Subtraction $1 $3}
      | Exp "+" Exp {Addition $1 $3}
      | Exp "*" Exp {Multiplication $1 $3}
      | Exp "/" Exp {Division $1 $3}
      | Exp "%" Exp {Modulo $1 $3}
      | Exp "^" Exp {Power $1 $3}
      | Exp "." DotMethod {Dot $1 $3}
      | "-" Exp {Negative $2}
      | "!" Exp {BooleanNot $2}
      | AtomicValue {Atomic $1}

  DotMethod : "identifier" "(" ArgumentList ")" {Method $1 $3} -- add method argument list here
            | "identifier" {Attribute $1}

  ArgumentList : {[]}
               | ArgumentList BooleanLogic "," {$1 ++ [$2]}
               | ArgumentList BooleanLogic {$1 ++ [$2]}

  AtomicValue : "integer value" {AtomicInt $1}
              | "string value" {AtomicString $1}
              | "boolean value" {AtomicBool $1}
              | "identifier" {AtomicVariable $1}

  BufferOperation : "clearBuffer" "(" ")" {ClearBuffer}
                  | "pushHeader" "(" ArgumentList ")" {PushHeader $3}
                  | "pushData" "(" ArgumentList ")" {PushData $3}
  
{
langParseError :: [LangToken] -> a
langParseError ts = error $ "Parse error at LangToken " ++ (show t)
  where t = head ts
}