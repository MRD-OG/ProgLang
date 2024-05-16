module ExpressionEvaluator (
    evaluateObjectAttribute
  , evaluateBooleanCondition
  , evaluateComparrison
  , evaluateExpression
  , findMatchingNodesetIndex
  , findMatchingRelationsetIndex) where

import DataStructure (
    DataEnvironment(..)
  , DataOutputBuffer(..)
  , DataVariableList(..)
  , DataDataPoint(..)
  , DataVariable(..)
  , DataType(..)
  , DataValue(..))

import LangAst (
    LangAstProgram
  , LangAstInstruction(..)
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

import N4jAst (
    N4jAstGraphData(..)
  , N4jAstDataComponent(..)
  , N4jAstGraphNodeHeader(..)
  , N4jAstHeaderField(..)
  , N4jAstHeaderFieldType(..)
  , N4jAstRelation(..)
  , N4jAstValue(..)
  , N4jAstNodeData(..))

import Debug.Trace (trace)

evaluateObjectAttribute :: DataEnvironment -> DataVariable -> Maybe DataDataPoint
evaluateObjectAttribute (Environment dataComponents _ _) (ObjectAttribute setName attr (setIndex, dataIndex))
  | valueIndex == -1 = error ("FieldDoesNotExist")
  | otherwise = resultValue
  where
    NodesetConstruct (HeaderDeclaration header _) nodeData = dataComponents!!setIndex
    valueIndex = getFieldIndex header attr
    NodeDataConstructor id fieldValues labels = nodeData!!dataIndex
    resultValue = n4jValueToDataData (fieldValues!!valueIndex)

getFieldIndex :: [N4jAstHeaderField] -> String -> Int
getFieldIndex [] _ = -1
getFieldIndex fields attr = getFieldIndex' (fields, 0) attr
  where
    getFieldIndex' :: ([N4jAstHeaderField], Int) -> String -> Int
    getFieldIndex' ([], _) _ = -1
    getFieldIndex' ((HeaderFieldDeclaration name _):attrs, n) attr
      | attr == name = n
      | otherwise = getFieldIndex' (attrs, n + 1) attr

n4jValueToDataData :: N4jAstValue -> Maybe DataDataPoint
n4jValueToDataData (N4jAst.StringValue s) = Just (Val (DataStructure.StringValue s))
n4jValueToDataData (N4jAst.IntegerValue i) = Just (Val (DataStructure.IntValue i))
n4jValueToDataData (N4jAst.NegativeIntegerValue i) = Just (Val (DataStructure.IntValue i))
n4jValueToDataData (N4jAst.BooleanValue b) = Just (Val (DataStructure.BoolValue b))

evaluateBooleanCondition :: DataEnvironment -> LangAstBooleanLogic -> Maybe DataDataPoint
evaluateBooleanCondition env (And b1 b2) = returnValue
  where
    maybeVal1 = evaluateBooleanCondition env b1
    maybeVal2 = evaluateBooleanCondition env b2
    returnValue = case (maybeVal1, maybeVal2) of
      (Just (Val (BoolValue bool1)), Just (Val (BoolValue bool2))) -> Just (Val (BoolValue (bool1 && bool2)))
      (Just (Val (BoolValue bool1)), Just (Var var2)) ->
        case fetchVariableValue env var2 of
          Just (Val (BoolValue bool2)) -> Just (Val (BoolValue (bool1 && bool2)))
          _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " AND " ++ show b2)
      (Just (Var var1), Just (Val (BoolValue bool2))) ->
        case fetchVariableValue env var1 of
          Just (Val (BoolValue bool1)) -> Just (Val (BoolValue (bool1 && bool2)))
          _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " AND " ++ show b2)
      (Just (Var var1), Just (Var var2)) ->
        case (fetchVariableValue env var1, fetchVariableValue env var2) of
          (Just (Val (BoolValue bool1)), Just (Val (BoolValue bool2))) -> Just (Val (BoolValue (bool1 && bool2)))
          _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " AND " ++ show b2)
      _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " AND " ++ show b2)

evaluateBooleanCondition env (Or b1 b2) = returnValue
  where
    maybeVal1 = evaluateBooleanCondition env b1
    maybeVal2 = evaluateBooleanCondition env b2
    returnValue = case (maybeVal1, maybeVal2) of
      (Just (Val (BoolValue bool1)), Just (Val (BoolValue bool2))) -> Just (Val (BoolValue (bool1 || bool2)))
      (Just (Val (BoolValue bool1)), Just (Var var2)) ->
        case fetchVariableValue env var2 of
          Just (Val (BoolValue bool2)) -> Just (Val (BoolValue (bool1 || bool2)))
          _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " OR " ++ show b2)
      (Just (Var var1), Just (Val (BoolValue bool2))) ->
        case fetchVariableValue env var1 of
          Just (Val (BoolValue bool1)) -> Just (Val (BoolValue (bool1 || bool2)))
          _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " OR " ++ show b2)
      (Just (Var var1), Just (Var var2)) ->
        case (fetchVariableValue env var1, fetchVariableValue env var2) of
          (Just (Val (BoolValue bool1)), Just (Val (BoolValue bool2))) -> Just (Val (BoolValue (bool1 || bool2)))
          _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " OR " ++ show b2)
      _ -> error ("Mismatch types for boolean logic: " ++ show b1 ++ " OR " ++ show b2)
evaluateBooleanCondition env (Not b1) = returnValue
  where
    maybeVal = evaluateBooleanCondition env b1
    returnValue = case maybeVal of
      Just (Val (BoolValue bool1)) -> Just (Val (BoolValue (not bool1)))
      Just (Var var1) ->
        case fetchVariableValue env var1 of
          Just (Val (BoolValue bool1)) -> Just (Val (BoolValue (not bool1)))
          _ -> error ("Mismatch types for boolean logic: NOT " ++ show b1)
      _ -> error ("Mismatch types for boolean logic: NOT " ++ show b1)
evaluateBooleanCondition env (Comparrison e1) = evaluateComparrison env e1

evaluateComparrison :: DataEnvironment -> LangAstComparrison -> Maybe DataDataPoint
evaluateComparrison env (LangAst.E c1 c2) = evaluateEqualityOperation env c1 c2
evaluateComparrison env (LangAst.NE c1 c2) = returnValue
  where
    Just (Val (BoolValue b1)) = evaluateComparrison env (LangAst.E c1 c2)
    returnValue = Just (Val (BoolValue (not b1)))

evaluateComparrison env (LangAst.GTE e1 e2) = evaluateComparrisonOperation env (>=) e1 e2
evaluateComparrison env (LangAst.LTE e1 e2) = evaluateComparrisonOperation env (<=) e1 e2
evaluateComparrison env (LangAst.GT e1 e2) = evaluateComparrisonOperation env (>) e1 e2
evaluateComparrison env (LangAst.LT e1 e2) = evaluateComparrisonOperation env (<) e1 e2
evaluateComparrison env (LangAst.Expression e) = returnValue
  where
    returnValue = evaluateExpression env e

evaluateExpression :: DataEnvironment -> LangAstExp -> Maybe DataDataPoint
evaluateExpression env (Subtraction e1 e2) = evaluateArithmeticOperation env (-) e1 e2
evaluateExpression env (Addition e1 e2) = evaluateArithmeticOperation env (+) e1 e2
evaluateExpression env (Multiplication e1 e2) = evaluateArithmeticOperation env (*) e1 e2
evaluateExpression env (Division e1 e2) = evaluateArithmeticOperation env (div) e1 e2
evaluateExpression env (Modulo e1 e2) = evaluateArithmeticOperation env (mod) e1 e2
evaluateExpression env (Power e1 e2) = evaluateArithmeticOperation env (^) e1 e2
evaluateExpression env (Negative e1) = returnValue
  where
    Just (Val v1) = evaluateExpression env e1
    returnValue = case (v1) of
      (IntValue i1) -> Just (Val (IntValue (-i1)))
      _ -> error ("Mismatching types in expression -" ++ show e1)
evaluateExpression env (BooleanNot e1) = returnValue
  where
    maybeValue = evaluateExpression env e1
    returnValue = case maybeValue of
      Just (Val (BoolValue b1)) -> Just (Val (BoolValue (not b1)))
      Just (Var var1) ->
        case fetchVariableValue env var1 of
          Just (Val (BoolValue b)) -> Just (Val (BoolValue (not b)))
          _ -> error ("Variable has mismatching types in expression !" ++ show e1)
      _ -> error ("Mismatching types in expression !" ++ show e1)
evaluateExpression env (Dot b1 dotMethod) = returnValue
  where
    maybeValue = evaluateBooleanCondition env b1
    returnValue = case maybeValue of

      Just (Var (NodesetVar name nodesetIndex)) ->
        case dotMethod of
          Attribute attr -> 
            case attr of
              "labels" -> Just (Var (HeaderFieldVar "" LabelType))
              n -> Just (Var (HeaderFieldVar attr DataStructure.IntegerType)) -- replace this with fieldVars
              _ -> error ("Invalid Attribute for type nodeset")
          Method meth args ->
            case meth of
              "get" -> builtinNodesetGet env (evaluateArgs env args) nodesetIndex
              _ -> error ("Invalid Method for type nodeset: " ++ show meth)

      Just (Var (RelationsetVar name relationsetIndex)) ->
        case dotMethod of
          Attribute attr -> 
            case attr of
              "type" -> Just (Var (HeaderFieldVar "" TypeType))
              n -> Just (Var (HeaderFieldVar attr DataStructure.IntegerType)) -- replace this with fieldVars
              _ -> error ("Invalid Attribute for type relationset: " ++ show attr)
          Method meth args ->
            case meth of
              "get" -> error ("Undefined Method get") -- -> RelationValueDec
              "hasRelation" -> error ("Undefined meethod hasRelation") -- -> Bool
              n -> error ("Invalid Method for type relationset: " ++ show meth)

      Just (Var (NodeVar name indices)) ->
        case dotMethod of 
          Attribute attr -> 
            case attr of
              n -> Just (Var (ObjectAttribute name attr indices)) -- replace this with fieldVars
              _ -> error ("Invalid Attribute for type node: " ++ show attr)
          Method meth args ->
            case meth of
              "hasLabel" -> error ("Undefined Method hasLabel")
              "hasLabelsOr" -> error ("Undefined Method hasLabelsOr")
              "hasLabelsAnd" -> error ("Undefined Method hasLabelsAnd")
              "matchLabels" -> error ("Undefined Method matchLabels")
              _ -> error ("Invalid Method for type node: " ++ show meth)
              
      Just (Var (RelationVar name indices)) ->
        case dotMethod of 
          Attribute attr -> 
            case attr of
              n -> Just (Var (ObjectAttribute name attr indices)) -- replace this with fieldVars
              _ -> error ("Invalid Attribute for type relation: " ++ show attr)
          Method meth args ->
            case meth of
              "hasType" -> error ("Undefined Method hasType")
              _ -> error ("Invalid Method for type relation: " ++ show meth)

      Just (Var (NodeValueDec name indices)) ->
        case dotMethod of 
          Attribute attr -> 
            case attr of
              n -> Just (Var (ObjectAttribute name attr indices))
              _ -> error ("Invalid Attribute for type node reference: " ++ show attr)
          Method meth args ->
            case meth of
              _ -> error ("Invalid Method for type node reference: " ++ show meth)

      Just (Val (DataStructure.StringValue s)) ->
        case dotMethod of 
          Attribute attr -> 
            case attr of
              _ -> error ("Invalid Attribute for type string: " ++ show attr)
          Method meth args ->
            case meth of
              "toInt" -> error ("Undefined method toInt") -- Int
              "toBool" -> error ("Undefined method toInt") -- Bool
              "reverse" -> error ("Undefined method toInt") -- String
              "toLower" -> error ("Undefined method toInt") -- String
              "toUpper" -> error ("Undefined method toInt") -- String
              "startsWith" -> error ("Undefined method toInt") -- Bool
              "endsWith" -> error ("Undefined method toInt") -- Bool
              "contains" -> error ("Undefined method toInt") -- Bool
              "append" -> error ("Undefined method toInt") -- String
              "subString" -> error ("Undefined method toInt") -- String
              "charAt" -> error ("Undefined method toInt") -- String
              "indexOf" -> error ("Undefined method toInt") -- Int
              "isEmpty" -> error ("Undefined method toInt") -- Bool
              "replace" -> error ("Undefined method toInt") -- String
              "replaceAll" -> error ("Undefined method toInt") -- String
              "replaceFirst" -> error ("Undefined method toInt") -- String
              "getLength" -> error ("Undefined method toInt") -- Int
              _ -> error ("Invalid Method for type node string: " ++ show meth)

      Just (Val (IntValue i)) ->
        case dotMethod of 
          Attribute attr -> 
            case attr of
              _ -> error ("Invalid Attribute for type integer: " ++ show attr)
          Method meth args ->
            case meth of
              "toString" -> error ("Undefined method toString") -- String
              "max" -> error ("Undefined method max") -- Int
              "min" -> error ("Undefined method min") -- Int
              "abs" -> error ("Undefined method abs") -- Int
              _ -> error ("Invalid Method for type node integer: " ++ show meth)
              
      Just (Val (BoolValue b)) ->
        case dotMethod of 
          Attribute attr -> 
            case attr of
              _ -> error ("Invalid Attribute for type boolean: " ++ show attr)
          Method meth args ->
            case meth of
              "toString" -> error ("Undefined method toString") -- String
              _ -> error ("Invalid Method for type node boolean: " ++ show meth)


      Just (Var (ObjectAttribute name attribute (setIndex, dataIndex))) -> evaluateExpression env (Dot expressionAtomic dotMethod)
        where
          -- convert the expr to an atomic value
          value = evaluateObjectAttribute env (ObjectAttribute name attribute (setIndex, dataIndex))
          expressionAtomic = case value of
            Just (Val (DataStructure.StringValue s)) -> Comparrison (Expression (Atomic (AtomicString s)))
            Just (Val (IntValue s)) -> Comparrison (Expression (Atomic (AtomicInt s)))
            Just (Val (BoolValue s)) -> Comparrison (Expression (Atomic (AtomicBool s)))

      _ -> error ("Unsupported type" ++ show (maybeValue))


      -- evaluate node va

evaluateExpression env (Atomic a) = returnValue
  where
    returnValue = evaluateAtomic env a

evaluateAtomic :: DataEnvironment -> LangAstAtomicValue -> Maybe DataDataPoint
evaluateAtomic env (AtomicInt i) = returnValue
  where
    returnValue = Just (Val (IntValue i))
evaluateAtomic env (AtomicString s) = returnValue
  where
    returnValue = Just (Val (DataStructure.StringValue s))
evaluateAtomic env (AtomicBool b) = returnValue
  where
    returnValue = Just (Val (BoolValue b))
evaluateAtomic env (AtomicVariable s) = returnValue
  where
    returnValue = evaluateAtomicVariable env s

evaluateAtomicVariable :: DataEnvironment -> String -> Maybe DataDataPoint
evaluateAtomicVariable (Environment _ (VariableList variableList) _) name = returnValue
  where
    var = fetchVariableDefinition variableList name
    returnValue = case (var) of
      (Just (Var v1)) -> Just (Var v1)
      Nothing -> error ("Variable not found: " ++ name)

fetchVariableDefinition :: [DataVariable] -> String -> Maybe DataDataPoint
fetchVariableDefinition [] _ = Nothing  -- Base case: empty list, return Nothing
fetchVariableDefinition (var:vars) name =
  case var of
    NodesetVar n _ | n == name -> Just (Var var)
    RelationsetVar n _ | n == name -> Just (Var var)
    NodeVar n _ | n == name -> Just (Var var)
    ObjectAttribute n _ _  | n == name -> Just (Var var)
    RelationVar n _ | n == name -> Just (Var var)
    IntVar n _ | n == name -> Just (Var var)
    StrVar n _ | n == name -> Just (Var var)
    BoolVar n _ | n == name -> Just (Var var)
    NodeValueDec n _ | n == name -> Just (Var var)
    RelationValueDec n _ | n == name -> Just (Var var)
    _ -> fetchVariableDefinition vars name

fetchVariableValue :: DataEnvironment -> DataVariable -> Maybe DataDataPoint
fetchVariableValue (Environment n4jData (VariableList variableList) _) var = loop variableList n4jData
  where
    loop [] _ = Nothing
    loop (v:vs) nData =
      case (var, v) of
        (IntVar n1 _, IntVar n2 i)
          | n1 == n2 -> Just (Val (IntValue i))
        (StrVar n1 _, StrVar n2 s)
          | n1 == n2 -> Just (Val (DataStructure.StringValue s))
        (BoolVar n1 _, BoolVar n2 b)
          | n1 == n2 -> Just (Val (BoolValue b))
        (RelationsetVar n1 _, RelationsetVar n2 index)
          | n1 == n2 -> Just (Var (RelationsetVar n2 index))
        (NodesetVar n1 _, NodesetVar n2 index)
          | n1 == n2 -> Just (Var (NodesetVar n2 index))
        (NodeVar n1 _, NodeVar n2 indices)
          | n1 == n2 -> Just (Var (NodeVar n2 indices))
        (RelationVar n1 _, RelationVar n2 indices)
          | n1 == n2 -> Just (Var (RelationVar n2 indices))
        (ObjectAttribute n1 f1 _, ObjectAttribute n2 f2 indices)
          | ((n1 == n2) && (f1 == f2)) -> Just (Var (ObjectAttribute n2 f2 indices))
        (NodeValueDec n1 _ , NodeValueDec n2 indices)
          | n1 == n2 -> Just (Var (NodeValueDec n2 indices))
        _ -> loop vs nData
        -- this shoudl definitely return values for nodeset fields for assignment
        -- declaration is fine being NodesetVar etc

findMatchingNodesetIndex :: [N4jAstDataComponent] -> [LangAstSetField] -> Bool -> Int
findMatchingNodesetIndex x y f = findMatchingNodesetIndex' (x, 0) y f
  where
    findMatchingNodesetIndex' :: ([N4jAstDataComponent], Int) -> [LangAstSetField] -> Bool -> Int
    findMatchingNodesetIndex' ([], _) _ _ = -1
    findMatchingNodesetIndex' ((NodesetConstruct (HeaderDeclaration fields f2) _):dataComponents, n) declarationFields f1
      | compareFieldLists fields declarationFields && f2 == f1 = n
      | otherwise = findMatchingNodesetIndex' (dataComponents, n + 1) declarationFields f1
    findMatchingNodesetIndex' (_:dataComponents, n) declarationFields f1 = findMatchingNodesetIndex' (dataComponents, n+1) declarationFields f1

findMatchingRelationsetIndex :: [N4jAstDataComponent] -> [LangAstSetField] -> Int
findMatchingRelationsetIndex x y = findMatchingRelationsetIndex' (x, 0) y
  where
    findMatchingRelationsetIndex' :: ([N4jAstDataComponent], Int) -> [LangAstSetField] -> Int
    findMatchingRelationsetIndex' ([], _) _ = -1
    findMatchingRelationsetIndex' ((RelationSetConstruct fields _):dataComponents, n) declarationFields
      | compareFieldLists fields declarationFields = n
      | otherwise = findMatchingRelationsetIndex' (dataComponents, n + 1) declarationFields
    findMatchingRelationsetIndex' (_:dataComponents, n) declarationFields = findMatchingRelationsetIndex' (dataComponents, n+1) declarationFields

compareFieldLists :: [N4jAstHeaderField] -> [LangAstSetField] -> Bool
compareFieldLists [] [] = True
compareFieldLists ((HeaderFieldDeclaration n N4jAst.StringType ):n4jFs) ((StringField  n2):langFs) = n == n2 && compareFieldLists n4jFs langFs
compareFieldLists ((HeaderFieldDeclaration n N4jAst.BooleanType):n4jFs) ((BooleanField n2):langFs) = n == n2 && compareFieldLists n4jFs langFs
compareFieldLists ((HeaderFieldDeclaration n N4jAst.IntegerType):n4jFs) ((IntegerField n2):langFs) = n == n2 && compareFieldLists n4jFs langFs
compareFieldLists _ _ = False

evaluateComparrisonOperation :: DataEnvironment -> (Int -> Int -> Bool) -> LangAstExp -> LangAstExp -> Maybe DataDataPoint
evaluateComparrisonOperation env op e1 e2 = returnValue
  where
    maybeVal1 = evaluateExpression env e1
    maybeVal2 = evaluateExpression env e2
    returnValue = case (maybeVal1, maybeVal2) of
      (Just (Val (IntValue i1)), Just (Val (IntValue i2))) -> Just (Val (BoolValue (i1 `op` i2)))
      (Just (Val (IntValue i1)), Just (Var var2)) ->
        case fetchVariableValue env var2 of
          Just (Val (IntValue i2)) -> Just (Val (BoolValue (i1 `op` i2)))
          _ -> error ("Mismatching types in expression " ++ show e1 ++ showCompOperator op ++ show e2)
      (Just (Var var1), Just (Val (IntValue i2))) ->
        case fetchVariableValue env var1 of
          Just (Val (IntValue i1)) -> Just (Val (BoolValue (i1 `op` i2)))
          _ -> error ("Mismatching types in expression " ++ show e1 ++ showCompOperator op ++ show e2)
      (Just (Var var1), Just (Var var2)) ->
        case (fetchVariableValue env var1, fetchVariableValue env var2) of
          (Just (Val (IntValue i1)), Just (Val (IntValue i2))) -> Just (Val (BoolValue (i1 `op` i2)))
          _ -> error ("Mismatching types in expression " ++ show e1 ++ showCompOperator op ++ show e2)
      _ -> error ("Mismatching types in expression " ++ show e1 ++ showCompOperator op ++ show e2)

evaluateEqualityOperation :: DataEnvironment -> LangAstComparrison -> LangAstComparrison -> Maybe DataDataPoint
evaluateEqualityOperation env c1 c2 = returnValue
  where
    maybeVal1 = evaluateComparrison env c1
    maybeVal2 = evaluateComparrison env c2
    returnValue = case (maybeVal1, maybeVal2) of
      (Just (Val (IntValue i1)), Just (Val (IntValue i2))) -> Just (Val (BoolValue (i1 == i2)))
      (Just (Val (BoolValue b1)), Just (Val (BoolValue b2))) -> Just (Val (BoolValue (b1 == b2)))
      (Just (Val (DataStructure.StringValue s1)), Just (Val (DataStructure.StringValue s2))) -> Just (Val (BoolValue (s1 == s2)))

      (Just (Val (IntValue i1)), Just (Var var2)) ->
        case fetchVariableValue env var2 of
          Just (Val (IntValue i2)) -> Just (Val (BoolValue (i1 == i2)))
          _ -> Just (Val (BoolValue False))
      (Just (Var var1), Just (Val (IntValue i2))) ->
        case fetchVariableValue env var1 of
          Just (Val (IntValue i1)) -> Just (Val (BoolValue (i1 == i2)))
          _ -> Just (Val (BoolValue False))

      (Just (Val (BoolValue b1)), Just (Var var2)) ->
        case fetchVariableValue env var2 of
          Just (Val (BoolValue b2)) -> Just (Val (BoolValue (b1 == b2)))
          _ -> Just (Val (BoolValue False))
      (Just (Var var1), Just (Val (BoolValue b2))) ->
        case fetchVariableValue env var1 of
          Just (Val (BoolValue b1)) -> Just (Val (BoolValue (b1 == b2)))
          _ -> Just (Val (BoolValue False))

      (Just (Val (DataStructure.StringValue s1)), Just (Var var2)) ->
        case fetchVariableValue env var2 of
          Just (Val (DataStructure.StringValue s2)) -> Just (Val (BoolValue (s1 == s2)))
          _ -> Just (Val (BoolValue False))
      (Just (Var var1), Just (Val (DataStructure.StringValue s2))) ->
        case fetchVariableValue env var1 of
          Just (Val (DataStructure.StringValue s1)) -> Just (Val (BoolValue (s1 == s2)))
          _ -> Just (Val (BoolValue False))

      (Just (Var var1), Just (Var var2)) ->
        case (fetchVariableValue env var1, fetchVariableValue env var2) of
          (Just (Val (IntValue i1)), Just (Val (IntValue i2))) -> Just (Val (BoolValue (i1 == i2)))
          (Just (Val (BoolValue b1)), Just (Val (BoolValue b2))) -> Just (Val (BoolValue (b1 == b2)))
          (Just (Val (DataStructure.StringValue s1)), Just (Val (DataStructure.StringValue s2))) -> Just (Val (BoolValue (s1 == s2)))
          _ -> Just (Val (BoolValue False))
      _ -> Just (Val (BoolValue False))

evaluateArithmeticOperation :: DataEnvironment -> (Int -> Int -> Int) -> LangAstExp -> LangAstExp -> Maybe DataDataPoint
evaluateArithmeticOperation env op e1 e2 = returnValue
  where
    maybeVal1 = evaluateExpression env e1
    maybeVal2 = evaluateExpression env e2
    returnValue = case (maybeVal1, maybeVal2) of
      (Just (Val (IntValue i1)), Just (Val (IntValue i2))) -> Just (Val (IntValue (i1 `op` i2)))
      (Just (Val (IntValue i1)), Just (Var var2)) ->
        case fetchVariableValue env var2 of
          Just (Val (IntValue i2)) -> Just (Val (IntValue (i1 `op` i2)))
          _ -> error ("Mismatching types in expression " ++ show e1 ++ showArithOperator op ++ show e2)
      (Just (Var var1), Just (Val (IntValue i2))) ->
        case fetchVariableValue env var1 of
          Just (Val (IntValue i1)) -> Just (Val (IntValue (i1 `op` i2)))
          _ -> error ("Mismatching types in expression " ++ show e1 ++ showArithOperator op ++ show e2)
      (Just (Var var1), Just (Var var2)) ->
        case (fetchVariableValue env var1, fetchVariableValue env var2) of
          (Just (Val (IntValue i1)), Just (Val (IntValue i2))) -> Just (Val (IntValue (i1 `op` i2)))
          _ -> error ("Mismatching types in expression " ++ show e1 ++ showArithOperator op ++ show e2)
      _ -> error ("Mismatching types in expression " ++ show e1 ++ showArithOperator op ++ show e2)

showArithOperator :: Num a => (a -> a -> a) -> String
showArithOperator op = case op of
    (-) -> "-"
    (+) -> "+"
    (*) -> "*"
    (div) -> "/"
    (mod) -> "%"
    (^) -> "^"

showCompOperator :: Ord a => (a -> a -> Bool) -> String
showCompOperator op = case op of
    (>=) -> ">="
    (<=) -> "<="
    (<) -> "<"
    (>) -> ">"


evaluateArgs :: DataEnvironment -> [LangAstBooleanLogic] -> [Maybe DataDataPoint]
evaluateArgs env (a:args) = (evaluateBooleanCondition env a) : (evaluateArgs env args)
evaluateArgs _ [] = []

---------------------------
-- ALl builtin functions --
---------------------------

builtinNodesetGet :: DataEnvironment -> [Maybe DataDataPoint] -> Int -> Maybe DataDataPoint
builtinNodesetGet (Environment dataComponents _ _) [Just (Val (IntValue i))] nodesetIndex
  | i >= length (nodeData) = error ("Index " ++ show i ++ " out of bounds for nodeset of length " ++ show (length (nodeData)))
  | i < 0 = error ("Index cannot be negative")
  | otherwise = Just (Val (NodeValue (nodesetIndex, i)))
    where
      NodesetConstruct _ nodeData = dataComponents!!nodesetIndex
builtinNodesetGet (Environment dataComponents _ _) [Just (Val (DataStructure.StringValue s))] nodesetIndex = builtinNodesetGet' (nodeData, 0) nodesetIndex s
  where
    NodesetConstruct _ nodeData = dataComponents!!nodesetIndex
    builtinNodesetGet' :: ([N4jAstNodeData], Int) -> Int -> String -> Maybe DataDataPoint
    builtinNodesetGet' ([], _) _ _ = error ("Nodeset is empty")
    builtinNodesetGet' ((NodeDataConstructor id _ _):dataComponents, n) nodeIndex name
      | id == name = Just (Val (NodeValue (nodeIndex, n)))
      | otherwise = builtinNodesetGet' (dataComponents, n + 1) nodeIndex name
    builtinNodesetGet' (_:dataComponents, n) nodeIndex name = builtinNodesetGet' (dataComponents, n+1) nodeIndex name
builtinNodesetGet _ args _ = error ("Get method expected 1 argument, but got " ++ show (length args))