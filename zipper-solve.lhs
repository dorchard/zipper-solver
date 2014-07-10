Whilst doing some experimenting with some ideas for a project, I
 realised I needed a quick piece of code to rearrange equations in an
 AST form (e.g., given an equation x = y + 3, solve for y to get y = 3
 - x). I needed to do this for arbitrary formulae, and suddenly
 realised that this forms a kind of <i>zipper</i> over the AST. I'll
 show the construction here for a simple arithmetic calculus with the AST:

> data Term = Add Term Term 
>           | Mul Term Term 
>           | Div Term Term
>           | Sub Term Term  
>           | Neg Term
>           | Var String
>           | Const Integer

with some standard pretty printing code:

> instance Show Term where 
>     show (Add t1 t2) = (show' t1) ++ " + " ++ (show' t2)
>     show (Mul t1 t2) = (show' t1) ++ " * " ++ (show' t2)
>     show (Sub t1 t2) = (show' t1) ++ " - " ++ (show' t2)
>     show (Div t1 t2) = (show' t1) ++ " / " ++ (show' t2)
>     show (Neg t) = "-" ++ (show' t) 
>     show (Var v) = v
>     show (Const n) = show n

<tt>show'</tt> is a helper to minimise brackets e.g. pretty printing "-(v)" as "-v".

> show' :: Term -> String
> show' (Var v) = v
> show' (Const n) = show n
> show' t@(Neg (Var v)) = show t
> show' t@(Neg (Const n)) = show t
> show' t = "(" ++ show t ++ ")"

Now, equations on <tt>Term</tt>s are zippers. Zippers represents data (of a recursively-defined data type)
that has part of the data "in focus". This is done by pairing the focal subdata with a "path" to get to the data:

> data Equation = Eq Term Path

For equations, the focus of the zipper is the left-hand side of the equation, and
 the path will be the right-hand side.

Paths give a sequence of direction markers, essentially providing an address to
the subterm in focus, starting from the root, where each marker is accompanied with the label
of the parent node and the subtree of the branch not taken, i.e., a path going left is paired with the 
right subtree, which is not on the path to the focus.

> data Path = Top                   -- At the top, i.e. ... = 0
>           | Bin Op                -- OR in a binary operation <tt>Op</tt>,
>                 Dir               --    in either left (<tt>L</tt>) or right (<tt>R</tt>) branch
>                 Term              --    with the untaken branch 
>                 Path              --    and the rest of the equation
>           | N Path                -- OR in the unary negation operation

> data Dir = L | R 
> data Op = A | M | D | S | So | Do

The <tt>Op</tt> type gives tags for every operation as well as <tt>So</tt> and <tt>Do</tt>
which represent sub and divide but with arguments flipped. This is used to get a bijection between
the operations that zip "up" and "down" the equation zipper, refocussing on subterms. 

A useful helper maps tags to their operations:

> opToTermC :: Op -> (Term -> Term -> Term)
> opToTermC A = Add
> opToTermC M = Mul
> opToTermC D = Div
> opToTermC S = Sub
> opToTermC So = (\x -> \y -> Sub y x)
> opToTermC Do = (\x -> \y -> Div y x)

Equations are pretty printed as follows:

> instance Show Path where
>     show p = show . pathToTerm $ p
>     
> instance Show Equation where
>     show (Eq t p) = (show t) ++ " = " ++ (show p)

where <tt>pathToTerm</tt> converts paths to terms in the obvious way:

> pathToTerm :: Path -> Term
> pathToTerm Top            = Const 0  
> pathToTerm (Bin op L t p) = (opToTermC op) (pathToTerm p) t
> pathToTerm (Bin op R t p) = (opToTermC op) t (pathToTerm p)
> pathToTerm (N p)          = Neg (pathToTerm p)

Now onto the solving zipper. Equations are zipped-down by <tt>left</tt> and <tt>right</tt>
which for a binary operation focus on either the left or right argument respectively, for unary
negation focus on the single argument, and for constants or variables does nothing. 
When going left or right, the equations are rebalanced with inverse arithmetic operations:

> left (Eq (Var s) p)     = Eq (Var s) p
> left (Eq (Const n) p)   = Eq (Const n) p
> left (Eq (Add t1 t2) p) = Eq t1 (Bin S L t2 p)   -- t1 + t2 = p  -> t1 = p - t2
> left (Eq (Mul t1 t2) p) = Eq t1 (Bin D L t2 p)   -- t1 * t2 = p  -> t1 = p / t2
> left (Eq (Div t1 t2) p) = Eq t1 (Bin M L t2 p)   -- t1 / t2 = p  -> t1 = p * t2
> left (Eq (Sub t1 t2) p) = Eq t1 (Bin A L t2 p)   -- t1 - t2 = p  -> t1 = p + t2
> left (Eq (Neg t) p)     = Eq t (N p)             -- -t = p       -> t = -p

> right (Eq (Var s) p)     = Eq (Var s) p          
> right (Eq (Const n) p)   = Eq (Const n) p
> right (Eq (Add t1 t2) p) = Eq t2 (Bin So R t1 p)  -- t1 + t2 = p -> t2 = p - t1 (note: So)
> right (Eq (Mul t1 t2) p) = Eq t2 (Bin Do R t1 p)  -- t1 * t2 = p -> t2 = p / t1 (note: Do)
> right (Eq (Div t1 t2) p) = Eq t2 (Bin D R t1 p)   -- t1 / t2 = p -> t2 = t1 / p
> right (Eq (Sub t1 t2) p) = Eq t2 (Bin S R t1 p)   -- t1 - t2 = p -> t2 = t1 - p
> right (Eq (Neg t) p)     = Eq t (N p)

Notably in <tt>right</tt>, <tt>Add</tt> and <tt>Mul</tt> become
 subtraction and dividing, as do <tt>Div</tt> and <tt>Sub</tt>. In
 order for the the zipping-up operation to be a bijection, the two uses
 are represented separately using <tt>So</tt> and <tt>Do</tt>.

Equations are zipped-up by <tt>up</tt>, which unrolls one step of the path
and reforms the term on the left-hand side from that on the right. This is the 
bijection of <tt>left</tt> and <tt>right</tt>:

> up (Eq t1 Top) = Eq t1 Top
> up (Eq t1 (Bin A L t2 p)) = Eq (Sub t1 t2) p
> up (Eq t1 (Bin M L t2 p)) = Eq (Div t1 t2) p
> up (Eq t1 (Bin D L t2 p)) = Eq (Mul t1 t2) p
> up (Eq t1 (Bin S L t2 p)) = Eq (Add t1 t2) p

> up (Eq t1 (Bin So R t2 p)) = Eq (Add t2 t1) p
> up (Eq t1 (Bin Do R t2 p)) = Eq (Mul t2 t1) p
> up (Eq t1 (Bin D R t2 p))  = Eq (Div t2 t1) p
> up (Eq t1 (Bin S R t2 p))  = Eq (Sub t2 t1) p

> up (Eq t1 (N p))           = Eq (Neg t1) p

And that's it! Here is an example of its use. 

> foo = Eq (Sub (Div (Add (Var "x") (Var "y")) (Add (Var "x") (Const 1))) (Const 1)) Top 

*Main> foo
((x + y) / (x + 1)) - 1 = 0

*Main> left $ foo
(x + y) / (x + 1) = 0 + 1

*Main> right . left $ foo
x + 1 = (x + y) / (0 + 1)

*Main> left . right . left $ foo
x = ((x + y) / (0 + 1)) - 1

*Main> up . left . right . left $ foo
x + 1 = (x + y) / (0 + 1)

*Main> up . up . left . right . left $ foo
(x + y) / (x + 1) = 0 + 1

*Main> up . up . up . left . right . left $ foo
((x + y) / (x + 1)) - 1 = 0

Of course, no normalisation is being done so sometimes terms can look a little ugly, but any evaluator
will deal with this easily. Also, this does not help if you have multiple uses of a variable and you want
to solve the question for a particular variable, e.g. y = x + 1/(3x), or quadratics.

Extension question: can we do this for higher-order abstract syntax?

