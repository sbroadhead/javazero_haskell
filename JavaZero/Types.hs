module JavaZero.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Identifier = String
type QualifiedName = [Identifier]
type SymbolTable = IntMap Symbol
type SymbolId = Int
type ScopeId = Int

data Symbol
    = VariableSym
        { s_name   :: Identifier
        , s_type   :: Type
        , s_mode   :: VariableMode
        , s_owner  :: Maybe SymbolId
        }
    | MethodSym
        { s_name   :: Identifier
        , s_type   :: Type
        , s_static :: Bool
        , s_args   :: [Type]
        , s_owner  :: Maybe SymbolId
        }
    | TypeSym
        { s_name      :: Identifier
        , s_attr      :: Map Identifier SymbolId
        , s_namespace :: QualifiedName -- not used
        , s_owner     :: Maybe SymbolId
        }
    deriving (Show, Eq, Ord)

data VariableMode
    = LocalVar Int
    | ArgumentVar Int
    | StaticVar
    | InstanceVar
    deriving (Show, Eq, Ord)

data Type
    = VoidType
    | IntType
    | BoolType
    | RefType SymbolId
    | ArrayType Type
    | MethodType Type [Type]
    | NullType
    | ClassType SymbolId
    deriving (Show, Eq, Ord)

data CompilationUnit = MkCompilationUnit
    { cu_classes :: [Class]
    , cu_symbols :: SymbolTable
    }
    deriving (Show, Eq, Ord)

data Class = MkClass
    { c_name   :: Identifier
    , c_body   :: [BodyDeclaration]
    , c_symbol :: SymbolId
    }
    deriving (Show, Eq, Ord)

data BodyDeclaration
    = FieldDeclaration
        { bd_isStatic :: Bool
        , bd_type     :: Type
        , bd_name     :: Identifier
        , bd_symbol   :: SymbolId
        }
    | MethodDeclaration
        { bd_isStatic :: Bool
        , bd_type     :: Type
        , bd_name     :: Identifier
        , bd_args     :: [Parameter]
        , bd_locals   :: [LocalDeclaration]
        , bd_stmts    :: [Statement]
        , bd_symbol   :: SymbolId
        }
    deriving (Show, Eq, Ord)

data Parameter = MkParameter
    { p_name :: Identifier
    , p_type :: Type
    }
    deriving (Show, Eq, Ord)

data LocalDeclaration = MkLocalDecl
    { ld_name   :: Identifier
    , ld_type   :: Type
    , ld_symbol :: SymbolId
    }
    deriving (Show, Eq, Ord)

data Statement
    = Assignment Expression Expression
    | IfStatement Expression Statement (Maybe Statement)
    | WhileStatement Expression Statement
    | ReturnStatement Expression
    | PrintStatement Expression
    | PrintLineStatement
    | ExpressionStatement Expression
    | BlockStatement [Statement]
    deriving (Show, Eq, Ord)

data BinOp = PlusOp | MinusOp | TimesOp | DivideOp | LtOp | LeqOp | GtOp | GeqOp | EqOp | NeqOp
    deriving (Show, Eq, Ord)

data UnOp = NegateOp | IdentityOp
    deriving (Show, Eq, Ord)

data Expression
    = BinExpr BinOp Expression Expression
    | UnExpr UnOp Expression
    | IntLiteral Integer
    | BoolLiteral Bool
    | SymbolRef SymbolId
    | FieldAccess Expression SymbolId
    | ArrayAccess Expression Expression
    | CallExpression Expression [Expression]
    | New Type
    | NewArray Type Expression
    | Null
    deriving (Show, Eq, Ord)
