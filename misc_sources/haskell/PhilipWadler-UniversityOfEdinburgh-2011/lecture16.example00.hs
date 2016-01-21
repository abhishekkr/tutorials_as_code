
-- Micro Haskell

import Data.Maybe
-- import Data.Maybe(fromJust)

type Id       = String
type ConId    = String

data Value    = ConVal ConId [Value]
              | FunVal (Value -> Value)

data Expr     = Var Id                                  -- x
              | Lam Id Expr                             -- \x -> e
              | Expr :@: Expr                           -- d e
              | Let Id Expr Expr                        -- let x = d in e
              | Con ConId [Expr]                        -- c x1 .. xn
              | Case Expr [(Pat, Expr)]                 -- case e of p1 -> e1;...

data Pat      = VarPat Id
              | ConPat ConId [Pat]

type Env      = [(Id, Value)]

instance Show Value where
  show (ConVal c vs)  = "ConVal " ++ show c ++ " " ++ show vs
  show (FunVal f)     = error "cannot show function"

apply :: Value -> Value -> Value
apply (ConVal c vs) w   = error "Cannot apply constructor"
apply (FunVal f) w      = f w

eval :: Expr -> Env -> Value
eval (Var x) r        = fromJust (lookup x r)
eval (Lam x e) r      = FunVal (\v -> eval e ((x,v):r))
eval (d :@: e) r      = apply (eval d r) (eval e r)
eval (Let x d e) r    = let v = eval d ((x,v):r) in
                          eval e ((x,v):r)
eval (Con c es) r     = ConVal c [ eval e r | e <- es ]
eval (Case d pes) r   = evalCase (eval d r) pes r

evalCase :: Value -> [(Pat,Expr)] -> Env -> Value
evalCase v [] r             = error "inexhaustive patterns"
evalCase v ((p,e):pes) r    = case (match p v) of
                                Nothing -> evalCase v pes r
                                Just s  -> eval e (s ++ r)

match :: Pat -> Value -> Maybe Env
match (VarPat x) v
  = Just [(x,v)]
match (ConPat c ps) (ConVal d vs)
  | c == d && length ps == length vs
  = combine [ match p v | (p,v) <- zip ps vs ]
match (ConPat c ps) v
  = Nothing

combine :: [ Maybe [a] ] -> Maybe [a]
combine = foldr (lift2 (++)) (lift0 [])
  where
    lift0 :: a -> Maybe a
    lift0 = Just

    lift2 :: (a -> a -> a) -> (Maybe a -> Maybe a -> Maybe a)
    lift2 f Nothing u           = Nothing
    lift2 f (Just x) Nothing    = Nothing
    lift2 f (Just x) (Just y)   = Just (f x y)


expr0 :: Expr
expr0 =
  (Let "add"
    (Lam "x" (Lam "y"
      (Case (Var "x") [
        (ConPat "Zero" [],
          Var "y"),
        (ConPat "Succ" [VarPat "u"],
          (Con "Succ" [
                           ((Var "add" :@: Var "u") :@: Var "y")
                         ]
          )
        ) ]
      )
    ))
    (Let "two"
      (Con "Succ" [Con "Succ" [Con "Zero" []]])
      ((Var "add" :@: Var "two") :@: Var "two")
    )
  )

test = eval expr0 []
