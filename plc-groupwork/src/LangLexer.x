{
module LangLexer (alexScanTokens) where
import LangTokens (LangToken(..))
}
%wrapper "basic"

tokens :-

  $white+ ;
  "--".* ;

  "(" {\_ -> LangTokenLParen}
  ")" {\_ -> LangTokenRParen}
  "[" {\_ -> LangTokenLSquare}
  "]" {\_ -> LangTokenRSquare}
  
  ";" {\_ -> LangTokenSemiColon}
  "," {\_ -> LangTokenComma}

  "." {\_ -> LangTokenDot}

  "+" {\_ -> LangTokenAddition}
  "-" {\_ -> LangTokenSubtraction}
  "*" {\_ -> LangTokenMultiplication}
  "/" {\_ -> LangTokenDividision}
  "%" {\_ -> LangTokenModulo}
  "^" {\_ -> LangTokenPower}

  "==" {\_ -> LangTokenEqualityE}
  "!=" {\_ -> LangTokenEqualityNE}
  ">=" {\_ -> LangTokenEqualityGTE}
  "<=" {\_ -> LangTokenEqualityLTE}
  ">" {\_ -> LangTokenEqualityGT}
  "<" {\_ -> LangTokenEqualityLT}
  
  "&&" {\_ -> LangTokenLogicalAnd}
  "||" {\_ -> LangTokenLogicalOr}
  "!" {\_ -> LangTokenLogicalNot}
  
  "=" {\_ -> LangTokenAssign}

  "int" {\_ -> LangTokenIntType}
  "str" {\_ -> LangTokenStringType}
  "bool" {\_ -> LangTokenBoolType}
  "nodeset" {\_ -> LangTokenNodesetType}
  "relationset" {\_ -> LangTokenRelationsetType}
  "node" {\_ -> LangTokenNodeType}
  "relation" {\_ -> LangTokenRelationType}

  "true" {\_ -> LangTokenBool True}
  "false" {\_ -> LangTokenBool False}

  "labels" {\_ -> LangTokenLabelOption}
  "clearBuffer" {\_ -> LangTokenClearBufferCommand}
  "pushHeader" {\_ -> LangTokenPushHeaderCommand}
  "pushData" {\_ -> LangTokenPushDataCommand}

  "import" {\_ -> LangTokenImport}

  [a-zA-Z]([a-zA-Z0-9])* {\s -> LangTokenIdentifier s}
  \"[a-zA-Z0-9]*\" {\s -> LangTokenString (init (tail s))}
  [0-9]+ {\s -> LangTokenNum (read s)}