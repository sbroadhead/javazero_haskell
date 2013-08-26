module JavaZero.CodeGen.CIL where

import qualified JavaZero.Types as J
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Numeric (showHex)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-----------
generate assembly = unlines . gen_compilationUnit assembly
-----------

type EnvReader a = Reader Env a
type Gen a = StateT GenState (Reader Env) a
type LabelledCode = IntMap String

data GenState = MkState
    { s_ctr      :: Int
    , s_code     :: LabelledCode
    }
    deriving (Show, Eq, Ord)

reserveLines :: Int -> Gen Int
reserveLines n = do
    ctr <- gets s_ctr
    modify $ \state -> state { s_ctr = (s_ctr state) + n }
    return ctr

label :: Int -> String
label i =
    "L_" ++ (padTo '0' 4 $ Numeric.showHex i "")
    where
        padTo c n xs = (take (n - length xs) (repeat c)) ++ xs

emitAt :: Int -> [String] -> Gen ()
emitAt n lines = do
    let newCode = IntMap.fromList $ zip [n..] lines
    modify $ \state -> state { s_code = (s_code state) `IntMap.union` newCode }

emit :: [String] -> Gen ()
emit lines = do
    ctr <- reserveLines $ length lines
    emitAt ctr lines

renderCode :: LabelledCode -> [String]
renderCode code = map (\(i,l) -> label i ++ ": " ++ l) $ sort $ IntMap.toList code

data Env = MkEnv
    { s_symbols :: IntMap J.Symbol
    }
    deriving (Show, Eq, Ord)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ mapM f xs

retrieveSymbol :: J.SymbolId -> EnvReader J.Symbol
retrieveSymbol id = reader ((IntMap.! id) . s_symbols)

indented :: EnvReader [String] -> EnvReader [String]
indented m = do
    result <- m
    return $ map ("    "++) result

exprType :: J.Expression -> EnvReader J.Type
exprType (J.BinExpr _ _ _) = return J.IntType
exprType (J.UnExpr _ _) = return J.IntType
exprType (J.IntLiteral _) = return J.IntType
exprType (J.BoolLiteral _) = return J.BoolType
exprType (J.SymbolRef id) = do
    symbol <- retrieveSymbol id
    case symbol of
        J.VariableSym _ t _ _ -> return t
        J.MethodSym _ t _ a _ -> return $ J.MethodType t a
        J.TypeSym _ _ _ _ -> return $ J.ClassType id
exprType (J.FieldAccess _ symbolId) = exprType $ J.SymbolRef symbolId
exprType (J.ArrayAccess expr _) = do
    ty <- exprType expr
    case ty of
        J.ArrayType ty -> return ty
        _ -> error $ "exprType: Tried to index into non-array"
exprType (J.CallExpression fun _) = do
    ty <- exprType fun
    case ty of
        J.MethodType returnType _ -> return returnType
        _ -> error $ "exprType: Tried to call a non-method"
exprType (J.New ty) =
    case ty of
        J.ClassType id -> return $ J.RefType id
        _ -> error $ "exprType: Tried to create instance of non class type"
exprType (J.NewArray ty _) =
    case ty of
        J.ClassType id -> return $ J.ArrayType (J.RefType id)
        x -> return $ J.ArrayType x
exprType J.Null = return J.NullType


gen_compilationUnit assembly (J.MkCompilationUnit classes symbols) = flip runReader (MkEnv symbols) $ do
    classCode <- concatMapM gen_class classes
    return $ [".assembly " ++ assembly ++ " {}",""] ++ classCode

gen_class (J.MkClass name body symbolId) = do
    bodyCode <- concatMapM (indented . gen_bodyDeclaration) body
    ctorCode <- indented $ return [".method public hidebysig specialname rtspecialname instance void .ctor() cil managed", "{", "    ret", "}", ""]
    return $ [".class public auto ansi sealed beforefieldinit " ++ name, "{", ""] ++ ctorCode ++ bodyCode ++ ["}", ""]

gen_typeName J.VoidType = return "void"
gen_typeName J.IntType = return "int32"
gen_typeName J.BoolType = return "bool"
gen_typeName (J.RefType symbolId) = do
    symbol <- retrieveSymbol symbolId
    case symbol of
        J.TypeSym "String" _ _ _ -> return $ "class [mscorlib]System.String" -- HACK HACK HACK
        J.TypeSym name _ _ _ -> return $ "class " ++ name
        _ -> error "gen_typeName: RefType does not reference a type symbol"
gen_typeName (J.ArrayType ty) = do
    typeName <- gen_typeName ty
    return $ typeName ++ "[]"
gen_typeName (J.MethodType _ _) = error "gen_typeName: MethodType"
gen_typeName J.NullType = error "gen_typeName: NullType"
gen_typeName (J.ClassType _) = error "gen_typeName: ClassType"

gen_bodyDeclaration (J.FieldDeclaration static ty name symbolId) = do
    typeName <- gen_typeName ty    
    let staticKey = if static then "static " else ""
    return $ [".field public " ++ staticKey ++ typeName ++ " " ++ name, ""]

gen_bodyDeclaration (J.MethodDeclaration static ty name args locals stmts symbolId) = do
    typeName <- gen_typeName ty
    let staticKey = if static then "static " else "instance "
    paramCode <- concat <$> intersperse ", " <$> concatMapM gen_parameter args
    let entryPoint = if static && name == "main" then [".entrypoint"] else []

    innerCode <- indented $ do
        localDeclsCode <- indented $ concatMapM gen_localDeclaration (zip [0..] locals)
        let localDeclsCode' = if null localDeclsCode then [] else [".locals init ("] ++ commafy localDeclsCode ++ [")"]
        stmtsCode <- evalStateT (gen_methodStatements stmts) (MkState 0 IntMap.empty)
        return $ entryPoint ++ localDeclsCode' ++ stmtsCode

    return $ [".method public hidebysig " ++ staticKey ++ typeName ++ " " ++ name ++ "(" ++ paramCode ++ ") cil managed",
        "{"] ++ innerCode ++ ["}", ""]

    where
        commafy [] = []
        commafy [x] = [x]
        commafy (x:xs) = (x++","):commafy xs

gen_localDeclaration (n, (J.MkLocalDecl name ty symbolId)) = do
    typeName <- gen_typeName ty
    return $ ["[" ++ show n ++ "] " ++ typeName ++ " " ++ name]

gen_parameter (J.MkParameter name ty) = do
    typeName <- gen_typeName ty
    return $ [typeName ++ " " ++ name]

gen_methodStatements stmts = do
    mapM_ gen_statement stmts
    emit ["ret"]
    code <- gets s_code
    return $ renderCode code

gen_statement (J.IfStatement e s1 s2) = do
    gen_expression e
    brloc <- reserveLines 1
    gen_statement s1
    case s2 of
        Nothing -> do { line <- gets s_ctr
                      ; emitAt brloc ["brfalse " ++ label line]
                      }
        Just s2' -> do { outloc <- reserveLines 1
                       ; gen_statement s2'
                       ; line <- gets s_ctr
                       ; emitAt brloc ["brfalse " ++ label (outloc + 1)]
                       ; emitAt outloc ["br " ++ label line]
                       }
gen_statement (J.Assignment e1 e2) = do
    gen_lvalueExpression e1
    gen_store e1 e2
gen_statement (J.WhileStatement e s) = do
    brloc <- reserveLines 1 
    gen_statement s
    line <- gets s_ctr
    gen_expression e
    emit ["brtrue " ++ label (brloc + 1)]
    emitAt brloc ["br " ++ label line]
gen_statement (J.ReturnStatement e) = do
    gen_expression e
    emit ["ret"]
gen_statement (J.PrintStatement e) = do
    gen_expression e
    emit ["call void [mscorlib]System.Console::Write(int32)"
         ,"ldstr \" \""
         ,"call void [mscorlib]System.Console::Write(string)"]
gen_statement J.PrintLineStatement = do
    emit ["ldstr \"\""
         ,"call void [mscorlib]System.Console::WriteLine(string)"]
gen_statement (J.ExpressionStatement e) = do
    gen_expression e
    ty <- lift $ exprType e
    when (ty /= J.VoidType) $ emit ["pop"] -- if the expression has a type, we need to ignore the result
gen_statement (J.BlockStatement stmts) = mapM_ gen_statement stmts

gen_qualifiedName symbolId = do
    symbol <- retrieveSymbol symbolId
    case J.s_owner symbol of
        { Nothing -> return $ (concat . intersperse "." $ getNamespace symbol) ++ J.s_name symbol
        ; Just ownerId -> do
              { ownerName <- gen_qualifiedName ownerId
              ; let sep = case symbol of { J.VariableSym _ _ _ _ -> "::"
                                         ; J.MethodSym _ _ _ _ _ -> "::"
                                         ; J.TypeSym _ _ _ _ -> "/"
                                         }
              ; return $ ownerName ++ sep ++ J.s_name symbol
              }
        }
    where
        getNamespace (J.TypeSym _ _ ns _) = ns
        getNamespace _ = []

gen_store (J.SymbolRef symbolId) expr = do
    gen_expression expr
    symbol <- lift $ retrieveSymbol symbolId
    case symbol of
        { J.VariableSym _ ty mode _ -> do
              { typeName <- lift $ gen_typeName ty
              ; qName <- lift $ gen_qualifiedName symbolId
              ; case mode of
                    { J.StaticVar -> emit ["stsfld " ++ typeName ++ " " ++ qName]
                    ; J.InstanceVar -> emit ["stfld " ++ typeName ++ " " ++ qName]
                    ; J.LocalVar idx -> emit ["stloc." ++ show idx]
                    ; J.ArgumentVar idx -> emit ["starg." ++ show idx]
                    }
              }
        ; _ -> error "gen_store: Can't assign to non-variable"
        }
gen_store (J.FieldAccess _ symbolId) expr = gen_store (J.SymbolRef symbolId) expr
gen_store (J.ArrayAccess dest index) expr = do
    gen_expression index
    gen_expression expr
    ty <- lift $ exprType dest
    let suffix = case ty of { J.ArrayType J.IntType -> ".i4"
                            ; J.ArrayType J.BoolType -> ".i4"
                            ; J.ArrayType (J.RefType _) -> ".ref"
                            ; J.ArrayType (J.ArrayType _) -> ".ref"
                            ; _ -> error $ "gen_store: Tried to store into a non-array or an array of invalid type " ++ show ty
                            }
    emit ["stelem" ++ suffix]

gen_loadThisPointer symbolId = do
    symbol <- lift $ retrieveSymbol symbolId
    case symbol of
        {  J.VariableSym _ _ mode _ ->
              case mode of
                  { J.InstanceVar -> emit ["ldarg.0"]  -- load "this" pointer
                  ; _ -> return ()
                  }
        ; J.MethodSym _ _ False _ _ -> emit ["ldarg.0"] -- load "this" pointer
        ; _ -> return ()
        }

gen_lvalueExpression (J.SymbolRef symbolId) = gen_loadThisPointer symbolId
gen_lvalueExpression (J.FieldAccess expr symbolId) = gen_expression expr
gen_lvalueExpression (J.ArrayAccess expr index) = gen_expression expr
gen_lvalueExpression _ = error "gen_lvalueExpression: Not an lvalue expression"

gen_expression (J.BinExpr op e1 e2) = do
    let e12 = gen_expression e1 >> gen_expression e2
    let e21 = gen_expression e2 >> gen_expression e1
    case op of
        J.PlusOp -> e12 >> emit ["add"]
        J.MinusOp -> e12 >> emit ["sub"]
        J.TimesOp -> e12 >> emit ["mul"]
        J.DivideOp -> e12 >> emit ["div"]
        J.LtOp -> e12 >> emit ["clt"]
        J.GtOp -> e12 >> emit ["cgt"]
        J.LeqOp -> e21 >> emit ["cgt"]
        J.GeqOp -> e21 >> emit ["clt"]
        J.EqOp -> e12 >> emit ["ceq"]
        J.NeqOp -> e12 >> emit ["ceq","ldc.i4.0","ceq"]
gen_expression (J.UnExpr op expr) = do
    gen_expression expr
    case op of
        J.NegateOp -> emit ["neg"]
        _ -> return ()
gen_expression (J.SymbolRef symbolId) = do
    gen_loadThisPointer symbolId
    symbol <- lift $ retrieveSymbol symbolId
    case symbol of
        { J.VariableSym _ ty mode _ -> do
              { typeName <- lift $ gen_typeName ty
              ; qName <- lift $ gen_qualifiedName symbolId
              ; case mode of
                    { J.StaticVar -> emit ["ldsfld " ++ typeName ++ " " ++ qName]
                    ; J.InstanceVar -> emit ["ldfld " ++ typeName ++ " " ++ qName]
                    ; J.LocalVar idx -> emit ["ldloc." ++ show idx]
                    ; J.ArgumentVar idx -> emit ["ldarg." ++ show idx]
                    }
              }
        ; _ -> return ()
        }
gen_expression (J.FieldAccess expr symbolId) = do
    gen_expression expr
    symbol <- lift $ retrieveSymbol symbolId
    case symbol of
        { J.VariableSym _ ty mode _ -> do
              { typeName <- lift $ gen_typeName ty
              ; qName <- lift $ gen_qualifiedName symbolId
              ; case mode of
                    { J.StaticVar -> emit ["ldsfld " ++ typeName ++ " " ++ qName]
                    ; J.InstanceVar -> emit ["ldfld " ++ typeName ++ " " ++ qName]
                    ; _ -> error "gen_expression: Can't access non-class variable from instance"
                    }
              }
        ; _ -> return ()
        }
gen_expression (J.ArrayAccess expr index) = do
    gen_expression expr
    gen_expression index
    ty <- lift $ exprType expr
    let suffix = case ty of { J.ArrayType J.IntType -> ".i4"
                            ; J.ArrayType J.BoolType -> ".i4"
                            ; J.ArrayType (J.RefType _) -> ".ref"
                            ; J.ArrayType (J.ArrayType _) -> ".ref"
                            ; _ -> error $ "gen_expression: Tried to load from a non-array or an array of invalid type: " ++ show ty
                            }
    emit ["ldelem" ++ suffix]
gen_expression (J.CallExpression e es) = do
    gen_lvalueExpression e
    mapM_ gen_expression es
    symbolId <- case e of
        J.SymbolRef symbolId -> return symbolId
        J.FieldAccess expr symbolId -> return symbolId
    symbol <- lift $ retrieveSymbol symbolId
    case symbol of
        J.MethodSym _ ty static args _ ->
            do { let instanceKey = if static then "" else "instance "
               ; typeName <- lift $ gen_typeName ty
               ; argTypeNames <- lift $ mapM gen_typeName args
               ; qName <- lift $ gen_qualifiedName symbolId 
               ; emit ["call " ++ instanceKey ++ typeName ++ " " ++ qName ++ "(" ++ (concat . intersperse ", " $ argTypeNames) ++ ")"]
               }
        _ -> error "gen_expression: Tried to call a non-method"
gen_expression (J.IntLiteral val) = emit ["ldc.i4 0x" ++ Numeric.showHex val ""]
gen_expression (J.BoolLiteral True) = emit ["ldc.i4.1"]
gen_expression (J.BoolLiteral False) = emit ["ldc.i4.0"]
gen_expression J.Null = emit ["ldnull"]
gen_expression (J.New ty) = do
    case ty of
        J.RefType symbolId -> do { qName <- lift $ gen_qualifiedName symbolId
                                   ; emit ["newobj instance void " ++ qName ++ "::.ctor()"]
                                   }
        _ -> error $ "gen_expression: Tried to instantiate object of non-class type: " ++ show ty
gen_expression (J.NewArray ty expr) = do
    gen_expression expr
    typeName <- lift $ gen_typeName ty
    emit ["newarr " ++ typeName]
