import Data.Functor.Identity (Identity, runIdentity)
import Language.Python.Common as Py
import Language.PureScript as Ps
import Control.Monad.Trans.Writer.Lazy

-- Use this to emit statements that need to be defined before an expression
type Prefixed = Writer [Py.Statement ()]

-- TODO: Use Text

main :: IO ()
main = do
    str <- readFile "example.purs"
    case Ps.lex "example.purs" str of
        Left  err    -> print err
        Right tokens -> case runTokenParser "example.purs" parseModule tokens of
            Left err -> print err
            Right m  -> do
                print m
                putStrLn ""
                putStrLn (prettyText (purescriptToPython m))

purescriptToPython :: Ps.Module -> Py.Module ()
purescriptToPython (Ps.Module _ _ decls _) =
    Py.Module (concatMap psDeclarationToPyStatements decls)

psDeclarationToPyStatements :: Ps.Declaration -> [Py.Statement ()]
psDeclarationToPyStatements d = case d of
    PositionedDeclaration _ _ d'          -> psDeclarationToPyStatements d'
    ValueDeclaration i Value [] (Right e) -> case i of
        Ps.Ident name ->
            let (e', statements) = runWriter (psExprToPyExpr e)
            in      statements
                ++  [Assign [Py.Var (Py.Ident name ()) ()] e' ()]
        Op    _    -> error ("Operations unsupported: " ++ show d)
    _ -> error ("Unsupported declaration: " ++ show d)

-- TODO: Go through unimplemented Python expressions and see if there is a
-- PureScript equivalent
psExprToPyExpr :: Ps.Expr -> Prefixed (Py.Expr ())
psExprToPyExpr e = case e of
    Ps.NumericLiteral x                        -> do
        return (case x of
            Left  i -> Py.Int   i (show i) ()
            Right d -> Py.Float d (show d) () )
    Ps.StringLiteral  str                      -> do
        return (Py.Strings [str] ())
    Ps.BooleanLiteral b                        -> do
        return (Py.Bool b ())
    Ps.UnaryMinus e1                           -> do
        e1' <- psExprToPyExpr e1
        return (Py.UnaryOp (Minus ()) e1' ())
--  Ps.BinaryNoParens i e1 e2                  -> do
--      e1' <- psExprToPyExpr e1
--      e2' <- psExprToPyExpr e2
--      return (Py.BinaryOp (psIdentToPyOperation i) e1' e2' ())
    Ps.Parens e'                               -> do
        psExprToPyExpr e'
    Ps.ArrayLiteral es                         -> do
        es' <- mapM psExprToPyExpr es
        return (Py.List es' ())
    Ps.ObjectLiteral xys                       -> do
        xys' <- mapM transform xys
        return (Py.Dictionary xys' ())
      where
        transform (x, y) = do
            y' <- psExprToPyExpr y
            return (DictMappingPair (Py.Var (Py.Ident x ()) ()) y')
    Ps.Accessor str e1                         -> do
        e1' <- psExprToPyExpr e1
        return (Py.Dot e1' (Py.Ident str ()) ())
    Ps.ObjectUpdate e1 xys                     -> do
        xys' <- mapM transform xys
        e1'  <- psExprToPyExpr e1
        let update =
                Py.Fun
                    (Py.Ident "_ps_update" ())
                    [Py.Param (Py.Ident "x" ()) Nothing Nothing ()]
                    Nothing
                    (   [Py.Assign
                            [Py.Var (Py.Ident "y" ()) ()]
                            (Py.Dot
                                (Py.Var (Py.Ident "x" ()) ())
                                (Py.Ident "copy" ())
                                () )
                            () ]
                    ++  xys'
                    ++  [Py.Return (Just (Py.Var (Py.Ident "y" ()) ())) ()]
                    )
                    ()
        tell [update]
        return (Py.Call (Py.Var (Py.Ident "_ps_update" ()) ()) [ArgExpr e1' ()] ())
      where
        transform (x, y) = do
            y' <- psExprToPyExpr y
            return (Py.Assign
                [Py.Subscript
                    (Py.Var (Py.Ident "x" ()) ())
                    (Py.Var (Py.Ident x      ()) ())
                    () ]
                y'
                () )
    Ps.Abs x e1                                -> do
        e1' <- psExprToPyExpr e1
        case x of
            Left (Ps.Ident str) ->
                return (Py.Lambda
                    [Py.Param (Py.Ident str ()) Nothing Nothing ()]
                    e1'
                    () )
    Ps.App e1 e2                               -> do
        e1' <- psExprToPyExpr e1
        e2' <- psExprToPyExpr e2
        return (Py.Call e1' [Py.ArgExpr e2' ()] ())
    Ps.PositionedValue _ _ e1                  -> do
        psExprToPyExpr e1
    Ps.Var (Ps.Qualified x i)                  -> do
        case x of
            Nothing -> case i of
                Ps.Ident str -> return (Py.Var (Py.Ident str ()) ())
                Ps.Op    str -> error ("Unsupported operator name: " ++ show e)
            _       -> error ("Unsupported qualified variable name: " ++ show e)
    Ps.IfThenElse e1 e2 e3                     -> do
        e1' <- psExprToPyExpr e1
        e2' <- psExprToPyExpr e2
        e3' <- psExprToPyExpr e3
        return (CondExpr e1' e2' e3' ())
    Ps.Constructor (Ps.Qualified m x)          -> do
        case m of
            Nothing -> return (Py.Var (Py.Ident (runProperName x) ()) ())
            _       -> error ("Unsupported qualified constructor name: " ++ show e)
--  Ps.Case es alts                            -> do
    _                                          -> do
        error ("Unsupported expression: " ++ show e)

-- TODO: Go through unimplemented Python operators and see if there are official
--     PureScript equivalents.  If not, consider implementing them.
psIdentToPyOperation :: Ps.Ident -> Py.Op ()
psIdentToPyOperation i = case i of
    Ps.Ident str -> case str of
        "not" -> Py.Not ()
        _     -> error ("Unsupported Ident: " ++ show i)
    Ps.Op    str -> case str of
        "&&" -> Py.And ()
        "||" -> Py.Or  ()
        "<"  -> Py.LessThan ()
        ">"  -> Py.GreaterThan ()
        "==" -> Py.Equality ()
        ">=" -> Py.GreaterThanEquals ()
        "<=" -> Py.LessThanEquals ()
        "/=" -> Py.NotEquals ()
        "^"  -> Py.Xor ()
        "&"  -> Py.BinaryAnd ()
        "*"  -> Py.Multiply ()
        "+"  -> Py.Plus ()
        "/"  -> Py.Divide ()
        "%"  -> Py.Modulo ()
        _    -> error ("Unsupported Op: " ++ show i)
