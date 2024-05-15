module LangTokens (LangToken(..)) where

data LangToken =
    LangTokenLParen
  | LangTokenRParen
  | LangTokenLSquare
  | LangTokenRSquare
  | LangTokenLCurly
  | LangTokenRCurly
  
  | LangTokenSemiColon
  | LangTokenComma

  | LangTokenDot

  | LangTokenAddition
  | LangTokenSubtraction
  | LangTokenMultiplication
  | LangTokenDividision
  | LangTokenModulo
  | LangTokenPower

  | LangTokenEqualityE
  | LangTokenEqualityNE
  | LangTokenEqualityGTE
  | LangTokenEqualityLTE
  | LangTokenEqualityGT
  | LangTokenEqualityLT
  
  | LangTokenLogicalAnd
  | LangTokenLogicalOr
  | LangTokenLogicalNot

  | LangTokenAssign

  | LangTokenIntType
  | LangTokenStringType
  | LangTokenBoolType
  | LangTokenNodesetType
  | LangTokenRelationsetType
  | LangTokenNodeType
  | LangTokenRelationType

  | LangTokenBool Bool

  | LangTokenLabelOption
  | LangTokenClearBufferCommand
  | LangTokenPushHeaderCommand
  | LangTokenPushDataCommand

  | LangTokenFor
  | LangTokenFrom
  | LangTokenTo
  | LangTokenStep

  | LangTokenIf
  | LangTokenElseif
  | LangTokenElse

  | LangTokenImport

  | LangTokenIdentifier String
  | LangTokenString String
  | LangTokenNum Int

  deriving (Eq, Show)

