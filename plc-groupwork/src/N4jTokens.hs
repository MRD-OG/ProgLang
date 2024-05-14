module N4jTokens (N4jToken(..)) where

data N4jToken =
    N4jTokenID
  | N4jTokenStartID
  | N4jTokenEndID
  | N4jTokenLabel
  | N4jTokenType

  | N4jTokenIntegerTypeMarker
  | N4jTokenStringTypeMarker
  | N4jTokenBooleanTypeMarker

  | N4jTokenComma
  | N4jTokenColon
  | N4jTokenSemicolon
  
  | N4jTokenBool Bool

  | N4jTokenIdentifier String
  | N4jTokenString String
  | N4jTokenNegative
  | N4jTokenPositive
  | N4jTokenNum Int
  deriving (Eq, Show)