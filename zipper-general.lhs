> {-# LANGUAGE FlexibleInstances #-}

> import Data.Maybe

> data Term n = Add (Term n) (Term n)
>             | Neg (Term n)
>             | Var String
>             | Const n

> instance Functor Term where
>     fmap f (Add t1 t2) = Add (fmap f t1) (fmap f t2)
>     fmap f (Neg t) = Neg (fmap f t)
>     fmap f (Var v) = Var v
>     fmap f (Const x) = Const (f x)

> instance Show n => Show (Term n) where 
>     show (Add t1 t2) = (show' t1) ++ " + " ++ (show' t2)
>     show (Neg t) = "-" ++ (show' t) 
>     show (Var v) = v
>     show (Const n) = show n

<tt>show'</tt> is a helper to minimise brackets e.g. pretty printing "-(v)" as "-v".

> show' :: Show n => Term n -> String
> show' (Var v) = v
> show' (Const n) = show n
> show' t@(Neg (Var v)) = show t
> show' t@(Neg (Const n)) = show t
> show' t = "(" ++ show t ++ ")"

> data Equation t = Eq t (Path t) 

> data Path t = Top                 -- At the top, i.e. ... = 0
>           | A                     -- OR in a binary operation <tt>Op</tt>,
>                 Dir               --    in either left (<tt>L</tt>) or right (<tt>R</tt>) branch
>                 t                 --    with the untaken branch 
>                 (Path t)              --    and the rest of the equation
>           | N (Path t)            -- OR in the unary negation operation

> instance Functor Equation where
>     fmap f (Eq t p) = Eq (f t) (fmap f p)

> instance Functor Path where
>     fmap f Top       = Top
>     fmap f (A d t p) = A d (f t) (fmap f p)
>     fmap f (N p)     = N (fmap f p)
>     

> data Dir = L | R deriving Show

> pathToTerm :: Group n => Path (Term n) -> (Term n)
> pathToTerm Top       = Const (unit ())
> pathToTerm (A L t p) = Add (pathToTerm p) t
> pathToTerm (A R t p) = Add t (pathToTerm p)
> pathToTerm (N p)     = Neg (pathToTerm p)

> left (Eq (Var s) p)    = Eq (Var s) p
> left (Eq (Const n) p)   = Eq (Const n) p
> left (Eq (Add t1 t2) p) = Eq t1 (A L t2 p)
> left (Eq (Neg t) p)     = Eq t (N p)

> right (Eq (Var s) p)     = Eq (Var s) p          
> right (Eq (Const n) p)   = Eq (Const n) p
> right (Eq (Add t1 t2) p) = Eq t2 (A R t1 p)
> right (Eq (Neg t) p)     = Eq t (N p)

> up (Eq t1 Top) = Eq t1 Top
> up (Eq t1 (A L t2 p)) = Eq (Add t1 t2) p
> up (Eq t1 (A R t2 p)) = Eq (Add t2 t1) p
> up (Eq t1 (N p))          = Eq (Neg t1) p


> class Group x where
>     op :: x -> x -> x
>     unit :: () -> x
>     inv :: x -> x

> toFun :: (Show a, Eq a) => [(a, b)] -> (a -> b)
> toFun xs = \x -> fromJust $ lookup x xs

> homom (Eq t p) env = (convT t env, convP p env)

> -- group homomorphism (from free group lifting of a group)
> convT (Add t1 t2) env = op (convT t1 env) (convT t2 env)
> convT (Neg t)     env = inv (convT t env)
> convT (Const n)   env = n
> convT (Var v)     env = env v

> -- some other kind of funny group homomorphism? (Kind of like a group action?)
> -- group action to group homomorphism?
> convP Top       env = unit ()
> convP (A L t p) env = op (convP p env) (inv $ convT t env)
> convP (A R t p) env = op (inv $ convT t env) (convP p env)
> convP (N p)     env = inv (convP p env)

> foo = Eq (Add (Add (Add (Var "x") (Var "y")) (Add (Var "x") (Const 1))) (Const 1)) Top
> foo2 = Eq (Add (Var "x") (Neg (Const 42))) Top 

> instance Group [Char] where
>     op x y  = "(" ++ x ++ " + " ++ y ++ ")"
>     inv x   = "-" ++ x
>     unit () = "0" 

> instance Group Integer where
>     op x y = x + y
>     inv x = - x
>     unit () = 0
>                  
> instance Group (Maybe Term, Integer) where
>     