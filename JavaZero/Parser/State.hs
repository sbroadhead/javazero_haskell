module JavaZero.Parser.State where

import JavaZero.Types as J

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.IntMap as IntMap
import Text.Parsec (getState, modifyState, Parsec)

type Parser a = Parsec String ParserState a

data Scope = MkScope
    { s_outer   :: Maybe Scope
    , s_symbols :: Map J.Identifier J.SymbolId
    , s_owner   :: Maybe J.SymbolId
    }
    deriving (Show, Eq, Ord)

data ParserState = MkState
    { p_ctr     :: Int
    , p_symbols :: J.SymbolTable
    , p_scope   :: Scope
    , p_qname   :: QualifiedName
    }
    deriving (Show, Eq, Ord)

emptyState = MkState 1 IntMap.empty (MkScope Nothing Map.empty Nothing) []

-----------

compileError :: String -> Parser a
compileError = fail 

getsState :: (ParserState -> a) -> Parser a
getsState f = f <$> getState

freshId :: Parser Int
freshId = do
    id <- getsState p_ctr
    modifyState $ \state -> state { p_ctr = (id+1) }
    return id

scopeIn :: Maybe J.SymbolId -> Parser ()
scopeIn owner = modifyState $ \state -> state { p_scope = MkScope (Just $ p_scope state) Map.empty owner }

scopeOut :: Parser ()
scopeOut = modifyState $ \state -> state { p_scope = fromJust $ s_outer $ p_scope state }

pushName :: Identifier -> Parser ()
pushName name = do
    state <- getState
    modifyState $ \state -> state { p_qname = name : (p_qname state) }

popName :: Parser ()
popName = do
    state <- getState
    modifyState $ \state -> state { p_qname = tail $ p_qname state }

scopeTrace :: Parser [J.SymbolId]
scopeTrace = do
    scope <- getsState p_scope
    return $ scopeTrace' scope
    where
        scopeTrace' (MkScope Nothing _ _) = []
        scopeTrace' (MkScope (Just o) _ x) =
            case x of
                Just x -> x : scopeTrace' o
                Nothing -> scopeTrace' o

newSymbol :: J.Identifier -> J.Symbol -> Parser J.SymbolId
newSymbol name symbol = do
    id <- freshId
    state <- getState
    let symbols' = IntMap.insert id symbol (p_symbols state)
    let scope = p_scope state
    let scope' = scope { s_symbols = Map.insert name id (s_symbols scope) }
    modifyState $ \state -> state { p_symbols = symbols', p_scope = scope' }
    return id

findSymbol :: J.Identifier -> Parser (Maybe (J.SymbolId, J.Symbol))
findSymbol name = do
    scope <- getsState p_scope
    findSymbolInScope (Just scope) name
    where
        findSymbolInScope Nothing _ = return Nothing
        findSymbolInScope (Just s) name =
            case Map.lookup name (s_symbols s) of
                Nothing -> findSymbolInScope (s_outer s) name
                Just id -> do { state <- getState
                              ; return $ Just (id, (p_symbols state) IntMap.! id)
                              }

getSymbol :: J.Identifier -> Parser (J.SymbolId, J.Symbol)
getSymbol name = do
    id <- findSymbol name
    case id of
        Just x -> return x 
        Nothing -> compileError $ "The symbol '" ++ name ++ "' was not found in the current scope"

retrieveSymbol :: J.SymbolId -> Parser J.Symbol
retrieveSymbol symbolId = do
    symbols <- getsState p_symbols
    return $ symbols IntMap.! symbolId

addSymbolToType :: J.Identifier -> J.SymbolId -> J.SymbolId -> Parser ()
addSymbolToType name ownerId symbolId = do
    owner <- retrieveSymbol ownerId
    let owner' = owner { s_attr = Map.insert name symbolId (s_attr owner) }
    modifyState $ \state -> state { p_symbols = IntMap.insert ownerId owner' (p_symbols state) }

exprType :: J.Expression -> Parser J.Type
exprType (BinExpr _ _ _) = return IntType
exprType (UnExpr _ _) = return IntType
exprType (IntLiteral _) = return IntType
exprType (BoolLiteral _) = return BoolType
exprType (SymbolRef id) = do
    symbol <- retrieveSymbol id
    case symbol of
        VariableSym _ t _ _ -> return t
        MethodSym _ t _ a _ -> return $ MethodType t a
        TypeSym _ _ _ _ -> return $ ClassType id
exprType (FieldAccess _ symbolId) = exprType $ SymbolRef symbolId
exprType (ArrayAccess expr _) = do
    ty <- exprType expr
    case ty of
        ArrayType ty -> return ty
        _ -> compileError $ "Tried to index into non-array"
exprType (CallExpression fun _) = do
    ty <- exprType fun
    case ty of
        MethodType returnType _ -> return returnType
        _ -> error $ "exprType: Tried to call a non-method"
exprType (New ty) =
    case ty of
        ClassType id -> return $ RefType id
        _ -> compileError $ "Tried to create instance of non class type"
exprType (NewArray ty _) =
    case ty of
        ClassType id -> return $ ArrayType (RefType id)
        x -> return $ ArrayType x
exprType Null = return NullType

getField :: J.Type -> J.Identifier -> Parser (J.SymbolId, J.Symbol)
getField ty name = do
    id <- case ty of { RefType id -> return id
                     ; ClassType id -> return id
                     ; _ -> compileError $ "Tried to access member '" ++ name ++ "' of a non-type"
                     }
    owner <- retrieveSymbol id
    case owner of
        TypeSym n a _ _ -> do { let fieldId = Map.lookup name a
                          ; case fieldId of { Just x -> do { field <- retrieveSymbol x; return (x, field) }
                                            ; Nothing -> compileError $ "Type '" ++ n ++ "' does not contain member '" ++ name ++ "'"
                                            }
                          }
        _ -> error "impossible: getField trying to get field for non-type"
