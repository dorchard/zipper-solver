Whilst doing some experimenting with some ideas for a project, I
 realised I needed a quick piece of code to rearrange equations
 (defined in terms of +, *, -, and /) in AST form, e.g., given an AST
 for the equation <tt>x = y + 3</tt>, rearrange to get <tt>y = x - 3</tt>.

 I realised that equations can be formulated as <i>zippers</i> over an AST, 
where operations for navigating the zipper essentially rearrange the equation.
I thought this was quite neat, so I thought I would show the technique here. 
The code is in simple Haskell.

I'll show the construction for a simple arithmetic calculus with the
 following AST data type of terms:

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

where <tt>show'</tt> is a helper to minimise brackets e.g. pretty printing "-(v)" as "-v".

> show' :: Term -> String
> show' (Var v) = v
> show' (Const n) = show n
> show' t@(Neg (Var v)) = show t
> show' t@(Neg (Const n)) = show t
> show' t = "(" ++ show t ++ ")"

Equations can be defined as pairs of terms, i.e., 'T1 = T2' where T1 and T2 are both represented
 by values of <tt>Term</tt>. However, instead, I'm going to represent equations via
a zipper. 

Zippers (described beautifully in <a
 href="http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf">the
 paper by Huet</a>) represent values that have some subvalue "in
 focus". The position of the focus can then be shifted through the
 value, refocussing on different parts. This is encoded by pairing a
 focal subvalue with a <i>path</i> to this focus, which records the
 rest of the value that is not in focus.

For equations, the zipper type pairs a focus <tt>Term</tt> (which we'll think
of as the left-hand side of the equation) with a path (which we'll think of
as the right-hand side of the equation).

> data Equation = Eq Term Path

Paths give a sequence of direction markers, essentially providing an
address to the term in focus, starting from the root, where each
marker is accompanied with the label of the parent node and the
subtree of the branch not taken, i.e., a path going left is paired
with the right subtree (which is not on the path to the focus).

> data Path = Top (Either Integer String)  -- At the top, a constant or variable, e.g. ... = 0
>           | Bin Op                -- OR in a binary operation <tt>Op</tt>,
>                 Dir               --    in either left (<tt>L</tt>) or right (<tt>R</tt>) branch
>                 Term              --    with the untaken branch 
>                 Path              --    and the rest of the equation
>           | N Path                -- OR in the unary negation operation

> data Dir = L | R 
> data Op = A | M | D | S | So | Do

The <tt>Op</tt> type gives tags for every operation, as well as
additional tags <tt>So</tt> and <tt>Do</tt> which represent sub and
divide but with arguments flipped. This is used to get an isomorphism
between the operations that zip "up" and "down" the equation zipper,
refocussing on subterms.

A useful helper maps tags to their operations:

> opToTerm :: Op -> (Term -> Term -> Term)
> opToTerm A = Add
> opToTerm M = Mul
> opToTerm D = Div
> opToTerm S = Sub
> opToTerm So = (\x -> \y -> Sub y x)
> opToTerm Do = (\x -> \y -> Div y x)

Equations are pretty printed as follows:

> instance Show Path where
>     show p = show . pathToTerm $ p
>     
> instance Show Equation where
>     show (Eq t p) = (show t) ++ " = " ++ (show p)

where <tt>pathToTerm</tt> converts paths to terms:

> pathToTerm :: Path -> Term
> pathToTerm (Top (Left c)) = Const c
> pathToTerm (Top (Right v))= Var v
> pathToTerm (Bin op L t p) = (opToTerm op) (pathToTerm p) t
> pathToTerm (Bin op R t p) = (opToTerm op) t (pathToTerm p)
> pathToTerm (N p)          = Neg (pathToTerm p)

Now onto the zipper operations which providing rebalancing of the
equation.  Equations are zipped-down by <tt>left</tt> and
<tt>right</tt>, which for a binary operation focus on either the left
or right argument respectively, for unary negation focus on the single
argument, and for constants or variables does nothing.  When going
left or right, the equations are rebalanced with their inverse
arithmetic operations (show in the comments here): 

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

In both <tt>left</tt> and <tt>right</tt>, <tt>Add</tt> and
 <tt>Mul</tt> become subtraction and dividing, but in <tt>right</tt>
 in order for the the zipping-up operation to be the inverse,
 subtraction and division are represented using the flipped
 <tt>So</tt> and <tt>Do</tt> markers.

Equations are zipped-up by <tt>up</tt>, which unrolls one step of the path
and reforms the term on the left-hand side from that on the right. This is the inverse
 of <tt>left</tt> and <tt>right</tt>:

> up (Eq t1 (Top a))        = Eq t1 (Top a)
> up (Eq t1 (Bin A L t2 p)) = Eq (Sub t1 t2) p -- t1 = t2 + p -> t1 - t2 = p
> up (Eq t1 (Bin M L t2 p)) = Eq (Div t1 t2) p -- t1 = t2 * p -> t1 / t2 = p
> up (Eq t1 (Bin D L t2 p)) = Eq (Mul t1 t2) p -- t1 = p / t2 -> t1 * t2 = p
> up (Eq t1 (Bin S L t2 p)) = Eq (Add t1 t2) p -- t1 = p - t2 -> t1 + t2 = p

> up (Eq t1 (Bin So R t2 p)) = Eq (Add t2 t1) p -- t1 = p - t2 -> t2 + t1 = p
> up (Eq t1 (Bin Do R t2 p)) = Eq (Mul t2 t1) p -- t1 = p / t2 -> t2 * t1 = p
> up (Eq t1 (Bin D R t2 p))  = Eq (Div t2 t1) p -- t1 = t2 / p -> t2 / t1 = p
> up (Eq t1 (Bin S R t2 p))  = Eq (Sub t2 t1) p -- t1 = t2 - p -> t2 - t1 = p

> up (Eq t1 (N p))           = Eq (Neg t1) p    -- t1 = -p     -> -t1 = p

And that's it! Here is an example of its use from GHCi.

> foo = Eq (Sub (Mul (Add (Var "x") (Var "y")) (Add (Var "x") (Const 1))) (Const 1)) (Top (Left 0))

*Main> foo
((x + y) * (x + 1)) - 1 = 0

*Main> left $ foo
(x + y) * (x + 1) = 0 + 1

*Main> right . left $ foo
x + 1 = (0 + 1) / (x + y)

*Main> left . right . left $ foo
x = ((0 + 1) / (x + y)) - 1

*Main> up . left . right . left $ foo
x + 1 = (0 + 1) / (x + y)

*Main> up . up . left . right . left $ foo
(x + y) * (x + 1) = 0 + 1

*Main> up . up . up . left . right . left $ foo
((x + y) * (x + 1)) - 1 = 0


Note: no normalisation is being done so sometimes terms can look
a little ugly, but any evaluator will deal with this easily.

It is straightforward to prove that <tt>up . left $ x = x</tt>
(when <tt>left x</tt> is not equal to <tt>x</tt>) and <tt>up . right $ x = x</tt>
(when <tt>right x</tt> is not equal to <tt>x</tt>). 

Note, I am simply rebalancing the syntax of equations: this technique 
does not help if you have multiple uses of a variable and you want to
solve the question for a particular variable, e.g. y = x + 1/(3x), or
quadratics.

Here's a concluding thought. The navigation operations <tt>left</tt>,
<tt>right</tt>, and <tt>up</tt> essentially apply the inverse of the
operation in focus to each side of the equation. We could therefore
reformulate the navigation operations in terms of any <i>group</i>:
given a term <tt>L &#8853 R</tt> under focus where <tt>&#8853</tt> is
the binary operation of a group with inverse operation
<tt><sup>-1</sup></tt>, then navigating left applies <tt>&#8853
R<sup>-1</sup></tt> and navigating right applies <tt>&#8853
L<sup>-1</sup></tt>. However, in the above there is a slight
difference: navigating applies the inverse and then reduces the term
of the left-hand side using the group axiom <tt>X &#8853
X<sup>-1</sup> = I</tt> (where <tt>I</tt> is the identity element of
the group) such that the term does not grow larger and larger with
inverses. 

I wonder if there are other applications, which have a group structure (or number
of interacting groups), for which the above zipper approach would be useful?


Extension question: can we do this for higher-order abstract syntax?


> foo2 = Eq (Mul (Const 0) (Var "x")) (Top (Left 0))
> foo3 = Eq (Mul (Const 0) (Var "x")) (Top (Right "x"))