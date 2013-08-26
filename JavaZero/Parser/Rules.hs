module JavaZero.Parser.Rules where

import qualified Data.Map as Map
import Data.Maybe
import qualified JavaZero.Language as L
import qualified JavaZero.Types as J
import qualified JavaZero.Parser.State as S
import Text.Parsec

-----------
parse src filename = runParser rule_compilationUnit S.emptyState filename src
-----------

rule_compilationUnit = do
    S.newSymbol "String" $ J.TypeSym "String" Map.empty [] Nothing
    classes <- many1 rule_classDeclaration
    state <- getState
    return $ J.MkCompilationUnit classes (S.p_symbols state)

rule_classDeclaration = do
    L.reserved "class"
    name <- L.identifier
    scope <- S.getsState S.p_scope
    let symbol = J.TypeSym name Map.empty [] (S.s_owner scope)
    id <- S.newSymbol name symbol
    S.scopeIn $ Just id
    decls <- L.braces (many $ rule_bodyDeclaration id)
    S.scopeOut
    return $ J.MkClass name decls id

rule_bodyDeclaration owner = do
    optional $ L.reserved "public"
    static <- optionMaybe $ L.reserved "static"
    rule_voidMethodDeclaration owner (isJust static)
        <|> rule_otherBodyDeclaration owner (isJust static)
    where
        rule_voidMethodDeclaration owner static = do
            L.reserved "void"
            name <- L.identifier
            rule_methodDeclaration owner static J.VoidType name
        rule_otherBodyDeclaration owner static = do
            ty <- rule_type
            name <- L.identifier
            choice [rule_methodDeclaration owner static ty name
                   ,rule_fieldDeclaration owner static ty name
                   ]

rule_fieldDeclaration owner static ty name = do
    L.semi
    let mode = if static then J.StaticVar else J.InstanceVar
    let symbol = J.VariableSym name ty mode (Just owner)
    id <- S.newSymbol name symbol
    S.addSymbolToType name owner id
    return $ J.FieldDeclaration static ty name id

rule_methodDeclaration owner static ty name = do
    params <- L.parens $ L.commaSep rule_parameter
    let paramTypes = map J.p_type params
    let symbol = J.MethodSym name ty static paramTypes (Just owner)
    id <- S.newSymbol name symbol
    S.addSymbolToType name owner id
    S.scopeIn Nothing
    let argStart = if static then 0 else 1
    mapM_ (scopeParam id) (zip [argStart..] params)
    (decls, stmts) <- L.braces $
        do { decls <- many $ try $ rule_localDeclaration id
           ; decls' <- mapM createIndexedLocal (zip [0..] decls)
           ; stmts <- many $ rule_statement
           ; return (decls', stmts)
           }
    S.scopeOut
    return $ J.MethodDeclaration static ty name params decls stmts id
    where
        scopeParam id (n, (J.MkParameter name ty)) = S.newSymbol name $
            J.VariableSym name ty (J.ArgumentVar n) Nothing
        createIndexedLocal (n, (name, ty)) = do
            id <- S.newSymbol name $ J.VariableSym name ty (J.LocalVar n) Nothing
            return $ J.MkLocalDecl name ty id

rule_parameter = do
    ty <- rule_type
    name <- L.identifier
    return $ J.MkParameter name ty

rule_localDeclaration owner = do
    ty <- rule_type
    name <- L.identifier
    L.semi
    return $ (name, ty)

rule_statement = choice
    [rule_ifStatement
    ,rule_whileStatement
    ,rule_returnStatement
    ,rule_printStatement
    ,rule_printLineStatement
    ,rule_blockStatement
    ,rule_exprStatement
    ]

rule_ifStatement = do
    L.reserved "if"
    expr <- L.parens rule_expression
    true_stmt <- rule_statement
    false_stmt <- optionMaybe $ L.reserved "else" >> rule_statement 
    return $ J.IfStatement expr true_stmt false_stmt

rule_whileStatement = do
    L.reserved "while"
    expr <- L.parens rule_expression
    stmt <- rule_statement
    return $ J.WhileStatement expr stmt

rule_returnStatement = do
    L.reserved "return"
    expr <- rule_expression
    L.semi
    return $ J.ReturnStatement expr

rule_printStatement = do
    L.reserved "print"
    expr <- L.parens rule_expression
    L.semi
    return $ J.PrintStatement expr

rule_printLineStatement = do
    L.reserved "println"
    L.symbol "("
    L.symbol ")"
    L.semi
    return J.PrintLineStatement

rule_blockStatement = do
    stmts <- L.braces $ do { S.scopeIn Nothing
                           ; stmts <- many rule_statement
                           ; S.scopeOut
                           ; return stmts
                           }
    return $ J.BlockStatement stmts

rule_exprStatement = do
    expr <- rule_expression
    rule_assignRest expr <|> (L.semi >> (return $ J.ExpressionStatement expr))

rule_assignRest expr = do
    L.reservedOp "="
    rhs <- rule_expression
    L.semi
    return $ J.Assignment expr rhs

rule_expression = rule_relationExpression 
rule_relationOp =   do { L.reservedOp "<"; return $ J.BinExpr J.LtOp }
                <|> do { L.reservedOp ">"; return $ J.BinExpr J.GtOp }
                <|> do { L.reservedOp "<="; return $ J.BinExpr J.LeqOp }
                <|> do { L.reservedOp ">="; return $ J.BinExpr J.GeqOp }
                <|> do { L.reservedOp "=="; return $ J.BinExpr J.EqOp }
                <|> do { L.reservedOp "!="; return $ J.BinExpr J.NeqOp }

rule_relationExpression = rule_arithmeticExpression `chainl1` rule_relationOp

rule_arithmeticExpression = rule_term `chainl1` rule_termOp

rule_termOp =   do { L.reservedOp "+"; return $ J.BinExpr J.PlusOp }
            <|> do { L.reservedOp "-"; return $ J.BinExpr J.MinusOp }

rule_term = rule_factor `chainl1` rule_factorOp

rule_factorOp =   do { L.reservedOp "*"; return $ J.BinExpr J.TimesOp }
              <|> do { L.reservedOp "/"; return $ J.BinExpr J.DivideOp }

rule_factor = do
    factor <- rule_factorHead `recurl1` rule_selector
    callArgs <- optionMaybe $ L.parens $ L.commaSep rule_expression
    return $ maybe factor (J.CallExpression factor) callArgs
    where
        rule_factorHead =
            L.parens rule_expression
            <|> rule_unaryExpression
            <|> rule_intLiteral
            <|> rule_boolLiteral
            <|> rule_nullLiteral
            <|> rule_symbolExpression
            <|> rule_newExpression `recurl1` rule_selector
        
rule_unaryOp =   do { L.reservedOp "+"; return J.IdentityOp }
             <|> do { L.reservedOp "-"; return J.NegateOp }

rule_unaryExpression = do
    op <- rule_unaryOp
    expr <- rule_expression
    return $ J.UnExpr op expr

rule_intLiteral = do
    val <- L.integer
    return $ J.IntLiteral val

rule_boolLiteral =   do { L.reserved "true"; return $ J.BoolLiteral True }
                 <|> do { L.reserved "false"; return $ J.BoolLiteral False }

rule_nullLiteral = do
    L.reserved "null"
    return $ J.Null

rule_symbolExpression = do
    name <- L.identifier
    (id, symbol) <- S.getSymbol name
    return $ J.SymbolRef id

rule_selector expr = rule_fieldAccess expr <|> rule_arrayAccess expr

rule_fieldAccess expr = do 
    L.symbol "."
    name <- L.identifier
    ty <- S.exprType expr
    (id, symbol) <- S.getField ty name
    return $ J.FieldAccess expr id

rule_arrayAccess expr = do
    index <- L.brackets rule_expression
    return $ J.ArrayAccess expr index

rule_newExpression = do
    L.reserved "new"
    rule_newValArrayExpression <|> rule_newOtherExpression
    where
        rule_newValArrayExpression = do
            typeName <- L.reserved "int" <|> L.reserved "bool"
            expr <- L.brackets rule_expression
            return $ J.NewArray (if typeName == "int" then J.IntType else J.BoolType) expr
        rule_newOtherExpression = do
            ty <- rule_refType
            expr <- do { L.symbol "("; L.symbol ")"; return Nothing; } <|>
                    do { expr <- L.brackets $ rule_expression; return $ Just expr }
            case expr of
                Nothing -> return $ J.New ty
                Just x -> return $ J.NewArray ty x

rule_baseType = choice
    [ L.reserved "int" >> return J.IntType
    , L.reserved "bool" >> return J.BoolType
    , rule_refType
    ]

rule_refType = do
    name <- L.identifier
    (id, symbol) <- S.getSymbol name
    return $ J.RefType id

rule_type = do
    baseType <- rule_baseType
    rule_arrayPostfix baseType <|> return baseType
    where
        rule_arrayPostfix ty = do
            L.symbol "["
            L.symbol "]"
            return $ J.ArrayType ty

-- Left-associative post-fix chain
recurl1 p c =
    do { x <- p; rest x }
    where rest x = do { y <- c x; rest y } <|> return x
