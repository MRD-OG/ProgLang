{
module N4jLexer (alexScanTokens) where
import N4jTokens (N4jToken(..))
}
%wrapper "basic"

tokens :-

  $white+ ;

  ":ID" {\_ -> N4jTokenID}
  ":START_ID" {\_ -> N4jTokenStartID}
  ":END_ID" {\_ -> N4jTokenEndID}
  ":LABEL" {\_ -> N4jTokenLabel}
  ":TYPE" {\_ -> N4jTokenType}

  "integer" {\_ -> N4jTokenIntegerTypeMarker}
  "string" {\_ -> N4jTokenStringTypeMarker}
  "boolean" {\_ -> N4jTokenBooleanTypeMarker}

  "," {\_ -> N4jTokenComma}
  ":" {\_ -> N4jTokenColon}
  ";" {\_ -> N4jTokenSemicolon}

  "true" {\_ -> N4jTokenBool True}
  "false" {\_ -> N4jTokenBool False}

  [a-zA-Z]([a-zA-Z0-9])* {\s -> N4jTokenIdentifier s}
  \"[a-zA-Z0-9]*\" {\s -> N4jTokenString (init (tail s))}
  \(\+[0-9]+\) {\s -> N4jTokenNum (read s)}
  \+ {\_ -> N4jTokenPositive}
  \- {\_ -> N4jTokenNegative}
  [0-9]+ {\s -> N4jTokenNum (read s)}
