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
    case runIndentParser "example.purs" parseModule str of
        Left err -> print err
        Right m  -> do
            print m
            putStrLn ""
            putStrLn (prettyText (purescriptToPython m))

options :: Options
options = defaultOptions
    { optionsNoPrelude = True
    , optionsBrowserNamespace = Just "PS"
    }

purescriptToPython :: Ps.Module -> Py.Module ()
purescriptToPython (Ps.Module _ decls _) =
    Py.Module (concatMap psDeclarationToPyStatements decls)

psDeclarationToPyStatements :: Ps.Declaration -> [Py.Statement ()]
psDeclarationToPyStatements d = case d of
    PositionedDeclaration _ d'   -> psDeclarationToPyStatements d'
    ValueDeclaration i Value [] Nothing e -> case i of
        Ps.Ident name ->
            let (e', statements) = runWriter (psExprToPyExpr e)
            in      statements
                ++  [Assign [Py.Var (Py.Ident name ()) ()] e' ()]
        Op    _    -> error ("Operations unsupported: " ++ show d)
    _ -> error ("Unsupported declaration: " ++ show d)

psExprToPyExpr :: Ps.Expr -> Prefixed (Py.Expr ())
psExprToPyExpr e = case e of
    Ps.NumericLiteral x                     -> do
        return (case x of
            Left  i -> Py.Int   i (show i) ()
            Right d -> Py.Float d (show d) () )
    Ps.StringLiteral  str                   -> do
        return (Py.Strings [str] ())
    Ps.BooleanLiteral b                     -> do
        return (Py.Bool b ())
    Ps.UnaryMinus e1                        -> do
        e1' <- psExprToPyExpr e1
        return (Py.UnaryOp (Minus ()) e1' ())
    Ps.BinaryNoParens (Qualified m i) e1 e2 -> do
        case m of
            Nothing -> do
                e1' <- psExprToPyExpr e1
                e2' <- psExprToPyExpr e2
                return (Py.BinaryOp (psIdentToPyOperation i) e1' e2' ())
            _       -> do
                error ("Unsupported qualifier: " ++ show e)
    Ps.Parens e'                            -> do
        psExprToPyExpr e'
    Ps.ArrayLiteral es                      -> do
        es' <- mapM psExprToPyExpr es
        return (Py.List es' ())
    Ps.ObjectLiteral xys                    -> do
        xys' <- mapM transform xys
        return (Py.Dictionary xys' ())
      where
        transform (x, y) = do
            y' <- psExprToPyExpr y
            return (Py.Var (Py.Ident x ()) (), y')
    Ps.Accessor str e1                      -> do
        e1' <- psExprToPyExpr e1
        return (Py.BinaryOp (Dot ()) e1' (Py.Var (Py.Ident str ()) ()) ())
    Ps.ObjectUpdate e1 xys                  -> do
        xys' <- mapM transform xys
        e1'  <- psExprToPyExpr e1
        let update =
                Py.Fun
                    (Py.Ident "_ps_update" ())
                    [Py.Param (Py.Ident "x" ()) Nothing Nothing ()]
                    Nothing
                    (   [Py.Assign
                            [Py.Var (Py.Ident "y" ()) ()]
                            (Py.BinaryOp
                                (Py.Dot ())
                                (Py.Var (Py.Ident "x" ()) ())
                                (Py.Var (Py.Ident "copy" ()) ())
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
    Ps.Abs x e1                             -> do
        e1' <- psExprToPyExpr e1
        case x of
            Left (Ps.Ident str) ->
                return (Py.Lambda
                    [Py.Param (Py.Ident str ()) Nothing Nothing ()]
                    e1'
                    () )
    Ps.App e1 e2                            -> do
        e1' <- psExprToPyExpr e1
        e2' <- psExprToPyExpr e2
        return (Py.Call e1' [Py.ArgExpr e2' ()] ())
    Ps.PositionedValue _ e1                 -> do
        psExprToPyExpr e1
    Ps.Var (Qualified x i)                -> do
        case x of
            Nothing -> case i of
                Ps.Ident str -> return (Py.Var (Py.Ident str ()) ())
                Ps.Op    str -> error ("Unsupported operator name: " ++ show e)
            _       -> error ("Unsupported qualified variable name: " ++ show e)
    _                                       -> do
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
