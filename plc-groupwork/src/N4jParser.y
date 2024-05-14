{
module N4jParser where
import N4jTokens (N4jToken(..))
import N4jAst (
    N4jAstGraphData(..)
  , N4jAstDataComponent(..)
  , N4jAstGraphNodeHeader(..)
  , N4jAstHeaderField(..)
  , N4jAstHeaderFieldType(..)
  , N4jAstRelation(..)
  , N4jAstValue(..)
  , N4jAstNodeData(..))
}

%name parseN4jTokens
%tokentype {N4jToken}
%error {n4jParseError}

%token
  ":ID" {N4jTokenID}
  ":START_ID" {N4jTokenStartID}
  ":END_ID" {N4jTokenEndID}
  ":LABEL" {N4jTokenLabel}
  ":TYPE" {N4jTokenType}

  "int" {N4jTokenIntegerTypeMarker}
  "str" {N4jTokenStringTypeMarker}
  "bool" {N4jTokenBooleanTypeMarker}

  "," {N4jTokenComma}
  ":" {N4jTokenColon}
  ";" {N4jTokenSemicolon}

  "boolean value" {N4jTokenBool $$}

  "identifier" {N4jTokenIdentifier $$}
  "string value" {N4jTokenString $$}
  "integer value" {N4jTokenNum $$}

  "+" {N4jTokenPositive}
  "-" {N4jTokenNegative}

  %%

  GraphData : DataComponentList {GraphData $1}

  DataComponentList : {[]}
                    | DataComponentList DataComponent {$1 ++ [$2]}

  DataComponent : GraphNodeHeader NodeDataList {NodesetConstruct $1 $2}
                | RelationSet RelationList {RelationSetConstruct $1 $2}

  GraphNodeHeader : ":ID" "," HeaderFieldList ":LABEL" {HeaderDeclaration $3 True}
                  | ":ID" "," HeaderFieldList {HeaderDeclaration $3 False}

  HeaderFieldList : {[]}
                  | HeaderFieldList HeaderField "," {$1 ++ [$2]}
                  | HeaderFieldList HeaderField {$1 ++ [$2]}

  HeaderField : "identifier" ":" "str" {HeaderFieldDeclaration $1 StringType}
              | "identifier" ":" "int" {HeaderFieldDeclaration $1 IntegerType}
              | "identifier" ":" "bool" {HeaderFieldDeclaration $1 BooleanType}

  NodeDataList : {[]}
               | NodeDataList NodeData {$1 ++ [$2]}

  NodeData : "identifier" "," N4jValueList LabelList {NodeDataConstructor $1 $3 $4}

  LabelList : "identifier" {[$1]}
            | LabelList ";" "identifier" {$1 ++ [$3]}

  RelationSet : ":START_ID" "," HeaderFieldList ":END_ID" "," ":TYPE" {$3}

  RelationList : {[]}
               | RelationList Relation {$1 ++ [$2]}

  -- :START_ID, [VALUES], :END_ID, :TYPE
  Relation : "identifier" "," N4jValueList "identifier" "," "identifier" {RelationDeclaration $1 $3 $4 $6}

  N4jValueList : {[]}
               | N4jValueList N4jValue "," {$1 ++ [$2]}
               | N4jValueList N4jValue {$1 ++ [$2]}

  N4jValue : "string value" {StringValue $1}
           | "integer value" {IntegerValue $1}
           | "-" "integer value" {NegativeIntegerValue $2}
           | "+" "integer value" {IntegerValue $2}
           | "boolean value" {BooleanValue $1}

{
n4jParseError :: [N4jToken] -> a
n4jParseError ts = error $ "Parse error at N4jToken " ++ (show t)
  where t = head ts
}