
module OctopusInterpreter
    where

import OctoParser
import Data.Char
import Data.Maybe
import Test.HUnit
import System.Exit
{- The heart of the interpreter is the eval function, which takes an
OctoValue and evaluates it in the given environment.  As in Racket,
expressions are represented as lists.  The eval function is defined by
cases.

The environment is a list of (name,value) pairs.  The type
'Environment' is a synonym for [(OctoValue,OctoValue)], and is defined in
OctoParser.  To look up a name the interpreter searches the environment
starting from the front, so that one variable can shadow another. -}



-- A read-eval-print loop, using IO monads. If the user types nothing and clicks
-- return, it aborts. Also, it checks whether the expression the user types has
-- "define" or not. If the expression has define, it makes new emvironment and
-- add to the current environment. It tailer-recursively
-- keeps track of new environment defined by "define".
read_eval_print_loop env = do
    putStr "Octopus> "
    n <- getLine
    if n == ""
        then do exitFailure
        else if check_define (parse n) == True
           then read_eval_print_loop (make_env (parse n) env)
           else putStrLn ("result: " ++ (octoshow $ eval (parse n) env))
    read_eval_print_loop env

-- This is main function ran by "runHaskell OctopusInterpreter.hs"
main = do
    read_eval_print_loop global_env



-- This is helper method that makes new environment when the user types "define"
-- and the given "define" is for setting a new variable (i.e. (define x 3)).
-- Then return the new enviroment.
-- It also catches the cases when the "define" defines new function. In that cases,
-- It adds the given function to the current enviroment and return the new environment.
make_env (OctoList[OctoSymbol "define", OctoSymbol x, y]) env =
    [(OctoSymbol x, eval y env)] ++ env

make_env (OctoList(OctoSymbol "define":[OctoList vars, body])) env  =
    [(head vars, eval(OctoList[OctoSymbol "lambda", OctoList $ tail vars, body]) env)] ++ env



-- This function checks an expression the user types has "define" or not.
-- return true if it has. Otherwise, return false.
-- If the user types String word or OctoSymbol, this interpreter should return String word
-- itself or OctoValue corresponding with OctoSymbol, so this function should return false.
check_define (OctoString x) = False
check_define (OctoSymbol x) = False
check_define (OctoList(x:xs)) =
    if x == OctoSymbol "define"
        then  True
        else  False



-- This helper functin takes a condition which the user wants to evaluate
-- and if this given condition is OctoBool False, it returns False. Otherwise,
-- returns true.
transBool cond =
    case cond of
        (OctoBool False) -> False
        _ -> True



-- This helper functin takes "cond" function in Racket. If there is one brnch,
-- it returns a body that will be evaluated. However, if there are many branch,
-- it recursively converts them to a set of nested if expressions, which this
-- program then evaluate.
cond_to_if cond =
    case cond of
        [OctoList[OctoSymbol "else", body]] -> body
        (OctoList[cond, val]: branch) -> OctoList[OctoSymbol "if", cond, val, cond_to_if branch]


-- eval function
eval :: OctoValue -> Environment -> OctoValue

-- integers and booleans evaluate to themselves
eval (OctoInt i) env = OctoInt i
eval (OctoBool b) env = OctoBool b
eval (OctoString s) env = OctoString s

{- To evaluate a symbol, look it up in the current environment and
return the value found; or raise an exception if it's not found. -}
eval s@(OctoSymbol v) env = case lookup s env of
    Nothing -> error ("name not found: " ++ v)
    Just r -> r

-- A quoted expression evaluates to that expression.
eval (OctoList [OctoSymbol "quote", x]) env = x

{- An expression starting with (lambda ...) evaluates to a closure,
where a closure consists of a list of variable names (OctoSymbols),
the environment of definition, and the body. -}
eval (OctoList [OctoSymbol "lambda", OctoList vars, body]) env =
    OctoClosure vars env body

-- eval function for an expression that has "if". If the given expression has
-- OctoSymbol 'if', it searches condition pattern (cond), true pattern (x), false pattern (y)
-- from the expression. Condtion part will be evaluated by transBool function and based on the
-- return boolean of transBool, it chooses whether x or y will evaluates.
eval (OctoList [OctoSymbol "if", cond, x, y]) env =
    eval (if (transBool $ eval cond env) then x else y) env

-- eval function for an expression that has "let". If the given expression has
-- OctoSymbol 'let', it searches variable definition part and body part. Then,
-- add the variable information to the current enviroment by transferring OctoList
-- tuples. Also this Octopus interpreter doesn't have side effect, so it evaluates
-- the body and retuns last part of the body.
eval (OctoList (OctoSymbol "let": OctoList vars: body)) env =
    last $ map (`eval` newenv) body
        where newenv = map (\(OctoList [x, y]) -> (x, eval y env)) vars ++ env

-- eval function for an expression that has "cond". If the given expression has
-- OctoSymbol "cond", it seaches branch part and passes it to cond_to_if function
-- cond_to_if function retuns nested if expression, so it evaluates the given
-- expression.
eval (OctoList (OctoSymbol "cond": branch)) env = eval (cond_to_if branch) env

-- eval function for an expression that has "letrec". If the given expression has
-- OctoSymbol 'letrec', it searches variable definition part and body part. Then,
-- add the variable information to the current enviroment by transferring OctoList
-- tuples. (However, it adds new information to newly created enviroment recursively)
-- Also this Octopus interpreter doesn't have side effect, so it evaluates
-- the body and retuns last part of the body.
eval (OctoList (OctoSymbol "letrec": OctoList vars: body)) env =
    last $ map (`eval` newenv) body
        where newenv = map (\(OctoList [x, y]) -> (x, eval y newenv)) vars ++ env

{- If we don't match any of the special cases, the first thing in the
list should evaluate to a function.  Apply it to its arguments.  There
are two cases: either the function is a user-defined function, or a
primitive.  These are handled separately.  In either case, the
arguments are found by evaluating each of the expressions after the
function name in the current environment. -}
eval (OctoList (f:xs)) env = case eval f env of
    c@(OctoClosure _ _ _) -> apply c args
    (OctoPrimitive p) -> fromJust (lookup p primitives) $ args
    where  args = map (\x -> eval x env) xs

{- Apply a user-defined function to the given arguments.  The user-defined
function has already been evaluated to get an OctoClosure, and the arguments
have already been evaluated as well in the calling environment.
Then make a new environment by extending the environment of definition
of the lambda (which is part of the closure).  In the extended
environment, the actual args are bound to the respective formal names,
evaluate the body of the function in this new environment, and return
the result. -}
apply (OctoClosure vars f_env body) args =
  if not (length vars == length args)
    then error ("arity mismatch") -- If number of args doesn't match number of vars, throw error (Question 15)
    else eval body (zip vars args ++ f_env)

-- list of primitive functions and their definitions in Haskell
-- for the starter, we only have +
-- you need to add various other functions
primitives = [ ("+",octoplus), ("-", octominus), ("*", octotimes), ("cons", octocons),
               ("car", octocar), ("cdr", octocdr), ("equal?", octoequal), ("eval", octoeval),
               ("string-append", stringappend)]

-- helper function for arithmetic functions (if we defined OctoInt using
-- record syntax we wouldn't need this, but I didn't feel like cluttering
-- everything else up)
getint (OctoInt i) = i

-- helper function for the stringappend function. It takes OctoString and transrates it to String.
-- (Question 13)
getString (OctoString i) = i

-- The octoplus function takes a list of OctoInts and adds them.
octoplus = OctoInt . sum . map getint

-- The octominus function takes a list of OctoInts and subtract lest values of the list
-- from the top value of the list.
-- If there is one value on the list, it returns its negative number. (i.e. OctoInt (-3))
octominus ls =
  if null (tail ls)
    then OctoInt(-(getint $ head ls))
    else OctoInt(foldl (-) (getint $ head ls) (map getint $ tail ls))

-- The octotimes function takes a list of OctoInts and multiply them.
octotimes = OctoInt . product . map getint

-- The octocons function takes 2 argument of OctoValue and combines them.
-- It can catch two cases that 1st argument is a list and 2nd argument is a list.
octocons [x, OctoList xs] = OctoList(x:xs)
octocons [OctoList xs, x] = OctoList(xs ++ [x])

-- The octocar function takes a list of OctoValue and returns the top value of it.
octocar [OctoList ls] = head ls

-- The octocdr function takes a list of OctoValue and returns the lest values of it.
-- If an empty list is passed, it throws an error.
octocdr [OctoList ls] =
  if ls == []
    then error ("cdr: contract violation; expected: pair?;  given: '()")
    else OctoList(tail ls)

-- The octoequal function takes a condition and return whether it is
-- OctoBool True or OctoBool False
octoequal [x,y] = OctoBool(x == y)

-- The octoeval function takes a list of OctoValue and evaluate it.
octoeval [x] = eval x global_env

-- The stringappend function takes a list of OctoString and combines them.
-- Then returns it as one OctoString.
stringappend ls = OctoString(foldl (++) "" (map getString ls))

-- The octoshow function takes OctoValue and turns any OctoValue into a String.
-- If val is OctoClosure it returns "<closure>". If val is OctoPrimitive, it
-- returns "<primitive>" with val. Also, if it passed an empty list, it returns
-- "()". OctoBool turns to be "#t" or "#f". OctoSymbol "quoto" also turn to be "'".
-- Also it can manage when the list has "quote". In other cases such as OctoString,
-- OctoSymbol, and OctoInt, return String value and Integer value.
octoshow val=
  case val of
    (OctoClosure _ _ _) -> "<closure>"
    (OctoPrimitive val) -> "<primitive " ++ val ++ ">"
    (OctoInt val) -> show val
    (OctoSymbol val) ->
      if val == "quote"
        then "'"
        else val
    (OctoBool val) ->
      if val
        then "#t"
        else "#f"
    (OctoList []) -> "()"
    (OctoList val) ->
      if head val == OctoSymbol "quote"
        then "'" ++ unwords(map octoshow $ tail val)
        else "(" ++ unwords(map octoshow val) ++ ")"
    (OctoString val) -> val


-- the global enviroment has null?, not, and the primitives
-- (and 'not' after you add it)
global_env = [
  (OctoSymbol "null?", eval (parse "(lambda (x) (equal? x '()))") global_env),
  (OctoSymbol "not", eval (parse "(lambda (x) (equal? x #f))") global_env)]
   ++ map (\(name,fn) -> (OctoSymbol name, OctoPrimitive name)) primitives
{- null? is defined by evaluating the result of parsing the lambda.
Notice that the environment in which it is evaluated is the global
environment, which we are defining.  This may look circular, but it
works fine in Haskell due to lazy evaluation.  You won't be able to
actually use null? until you have lambda working, but again because
Haskell is lazy it's fine to put the definition in from the start. -}

-- for the unit tests, make a test environment by extending the global env
testenv = [(OctoSymbol "k", OctoInt 5) , (OctoSymbol "s" , OctoSymbol "x")]
   ++ global_env ++ [(OctoSymbol "f",OctoList [OctoSymbol "lambda",OctoList [OctoSymbol "x"],OctoSymbol "x"])]

evparse s = eval (parse s) testenv

{- unit tests for the interpreter -}
{- the tests that don't work yet with the starter program are commented out -}

-- Define a helper function to make test cases so that they aren't so
-- ridiculously verbose
testeval expr val = TestLabel
  ("evaluating " ++ expr ++ " should give " ++ show val)
  (TestCase (assertEqual "an OctoTest" val (evparse expr)))

-- note that we need to get the white space right for the input strings
-- for this, since we are testing for string equality on the output
show_test_cases = ["7", "#f", "(1 2 3)", "(+ 1 (* 2 3))", "'squid", "'(1 2 3)"]

-- some expressions to test let
shadowing_let1 = " \
\  (let ((k (+ 3 k))  \
\        (j k))       \
\    (+ j k))"

nested_let1 = " \
\  (let ((x 3)       \
\        (y 4))      \
\    (let ((x 100))  \
\      (+ x y)))"

nested_let2 = " \
\  (let ((x 3)       \
\        (y 4))      \
\    (let ((x (+ x y)))  \
\      (+ x y)))"

nested_let3 = " \
\  (let ((n 10))                     \
\    (let ((f (lambda (x) (+ x n)))  \
\          (n 3))                    \
\      (+ (f 100) n)))"

{- Expr to test that lambda is closing over its environment of
definition.  Here, n is defined in the let but not where f is used --
so we would get an error if the body of the lambda were evaluated in
the wrong environment.-}
let_test_closure = "                         \
\ (let ((y 10)                               \
\       (f (let ((n 50))                     \
\            (lambda (x) (+ x (* 2 n))))))   \
\   (f y))"

-- the factorial function
letrec_fact =  " \
\ (letrec       \
\    ((fact (lambda (n) (if (equal? 0 n) 1 (* n (fact (- n 1)))))))   \
\  (fact 4))"

-- the range function returns a list of integers from n down to 0
letrec_range =  " \
\ (letrec       \
\    ((range (lambda (n) (if (equal? 0 n) '() (cons n (range (- n 1)))))))   \
\  (range 4))"

letrec_map =  " \
\ (letrec \
\    ((map (lambda (f s) \
\        (if (null? s) '() (cons (f (car s)) (map f (cdr s)))))))  \
\  (map (lambda (n) (* n 2)) '(10 20 30)))"

-- Curried version of map. Here map itself takes one argument (a function),
-- and returns a new function that maps over a list.
letrec_curried_map =  "                                                     \
\ (letrec                                                                   \
\    ((map (lambda (f)                                                      \
\            (lambda (s)                                                    \
\              (if (null? s) '() (cons (f (car s)) ((map f) (cdr s))))))))  \
\  ((map (lambda (n) (* n 2))) '(10 20 30)))"

-- test mutual recursion in letrec with a ridiculously slow version of
-- the mod2 function
letrec_mod2 = "                                                 \
\ (letrec                                                       \
\     ((zero? (lambda (x) (equal? x 0)))                        \
\      (even? (lambda (x) (if (zero? x) #t (odd? (- x 1)))))    \
\      (odd?  (lambda (x) (if (zero? x) #f (even? (- x 1)))))   \
\      (mod2 (lambda (x) (if (even? x) 0 1))))                   \
\   (cons (mod2 5) (cons (mod2 6) '())))"

-- overcommented expression to test handling Racket comments
complicated_comment_test = "  ; this is a comment on its own line \n \
\  (+ 3  ; another comment \n  \
\  4  )  \n      \
\ ; and a final comment \n"

-- For Question 16
-- This function is for test "define" and REPL. This function takes a list that contains
-- the expression the user supporse to type in a console and the current environment.
-- If it hits the expression that contains "define", it makes the new environment and
-- go into the next REPL roop. Otherwise, return the value based on the evaluation.
test_define (x:xs) env =
    if check_define (parse x) == True
        then test_define xs (make_env (parse x) env)
        else eval (parse x) env


tests = TestList [
  testeval "#t" (OctoBool True),
  testeval "#f" (OctoBool False),
  testeval "3" (OctoInt 3),
  testeval "'x" (OctoSymbol "x"),
  testeval "'(x 5)" (OctoList [OctoSymbol "x", OctoInt 5]),
  testeval "k" (OctoInt 5),
  testeval "s" (OctoSymbol "x"),
  -- test for the case when the user types String word.
  testeval "\"test\"" (OctoString "test"),
  -- test + for 0, 1, 2, and 4 args
  testeval "(+)" (OctoInt 0),
  testeval "(+ 3)" (OctoInt 3),
  testeval "(+ 3 4)" (OctoInt 7),
  testeval "(+ 3 4 10 20)" (OctoInt 37),
  testeval "(+ 3 (+ 3 3))" (OctoInt 9),
  -- test - for 1, 2, and 4 args (can't test (-) because it returns error)
  testeval "(- 3)" (OctoInt (-3)),
  testeval "(- 3 4)" (OctoInt (-1)),
  testeval "(- 4 3)" (OctoInt 1),
  testeval "(- 3 4 10 20)" (OctoInt (-31)),
  testeval "(- 0 (- 1 3))" (OctoInt 2),
  -- test * for 0, 1, 2, and 4 args
  testeval "(*)" (OctoInt 1),
  testeval "(* 3)" (OctoInt 3),
  testeval "(* 3 4)" (OctoInt 12),
  testeval "(* 3 -4)" (OctoInt (-12)),
  testeval "(* 3 4 10 20)" (OctoInt 2400),
  testeval "(* 3 4 (* 3 4))" (OctoInt 144),
  -- test cons (the case 1sr argument is a list, 2nd argument is a list and both are lists)
  testeval "(cons '(1 2) 3)" (OctoList [OctoInt 1, OctoInt 2, OctoInt 3]),
  testeval "(cons '() 3)" (OctoList [OctoInt 3]),
  testeval "(cons 3 '())" (OctoList [OctoInt 3]),
  testeval "(cons 3 '(x 5))" (OctoList [OctoInt 3, OctoSymbol "x", OctoInt 5]),
  testeval "(cons '(x 7) '(x 5))" (OctoList [OctoList[OctoSymbol "x", OctoInt 7], OctoSymbol "x", OctoInt 5]),
  testeval "(cons \"hello\" '(1 2))" (OctoList [OctoString "hello", OctoInt 1, OctoInt 2]),
  -- test car
  testeval "(car '(x (car (1 3))))" (OctoSymbol "x"),
  testeval "(car '(x 5 7))" (OctoSymbol "x"),
  testeval "(car '(x 5))" (OctoSymbol "x"),
  testeval "(car '(x))" (OctoSymbol "x"),
  -- test cdr
  testeval "(cdr '(x))" (OctoList []),
  testeval "(cdr '(x 5 3))" (OctoList [OctoInt 5, OctoInt 3]),
  testeval "(cdr '(x 5 3 1))" (OctoList [OctoInt 5, OctoInt 3, OctoInt 1]),
  testeval "(cdr '(x 5 3 (1 2)))" (OctoList [OctoInt 5, OctoInt 3, OctoList[OctoInt 1, OctoInt 2]]),
  -- test equal?
  testeval "(equal? 1 1)" (OctoBool True),
  testeval "(equal? '() '())" (OctoBool True),
  testeval "(equal? '(x 5 3) '(x 5 3))" (OctoBool True),
  testeval "(equal? '(x 5 3) '(x 5 2))" (OctoBool False),
  testeval "(equal? (+ 5 3) (+ 4 4))" (OctoBool True),
  testeval "(equal? (car '(5 3)) (car '(5 2)))" (OctoBool True),
  testeval "(equal? (cdr '(5 3 4)) (cdr '(4 3 4)))" (OctoBool True),
  testeval "(equal? (cdr '(5 3 2)) (car '(5 2 3)))" (OctoBool False),
  -- can't use the shortcut for these -- testing octoshow
  TestLabel "octoshow" (TestCase (assertEqual "test octoshow"
    show_test_cases (map (octoshow . parse) show_test_cases))),
  TestLabel "octoshow primitive" (TestCase (assertEqual "test octoshow"
    "<primitive *>" (octoshow $ evparse "*"))),
  testeval "( (lambda (x) x) 7)" (OctoInt 7),
  testeval "((lambda (x y) (+ x (+ y 10))) 3 4)" (OctoInt 17),
  -- the inner lambda's y should shadow the outer one, so we get 11
  -- rather than 3
  testeval "( (lambda (x y) ((lambda (y) (+ x y)) 10)) 1 2)" (OctoInt 11),
  testeval "(let ((x 3)) (+ x 4))" (OctoInt 7),
  testeval "(let ((x 3) (y 4)) (+ x y))" (OctoInt 7),
  testeval shadowing_let1 (OctoInt 13),
  testeval nested_let1 (OctoInt 104),
  testeval nested_let2 (OctoInt 11),
  testeval nested_let3 (OctoInt 113),
  testeval let_test_closure (OctoInt 110),
  -- The first two if cases have a nonexistant variable on the branch not
  -- taken. If we evaluated it we would get an error, so if this works it
  -- means 'if' is not evaluating the branch not taken.
  -- The third checks that anything other than #f counts as true.
  -- The fourth makes sure the test is evaluated (it evaluates to #f).
  testeval "(if #t 3 bad)" (OctoInt 3),
  testeval "(if #f bad (+ 2 3))" (OctoInt 5),
  testeval "(if 2 3 5)" (OctoInt 3),
  testeval "(if (equal? k 10) (+ 2 3) (+ 10 20))" (OctoInt 30),
  testeval "(if (equal? k 15) \"true\" \"false\")" (OctoString "false"),
  -- test cases for not
  testeval "(not #t)" (OctoBool False),
  testeval "(not #f)" (OctoBool True),
  testeval "(not 1)" (OctoBool False),
  testeval "(not '())" (OctoBool False),
  testeval "(not 'we-have-no-bananas)" (OctoBool False),
  testeval "(not (car '(#f #t)))" (OctoBool True),
  -- test cases for cond
  testeval "(cond (else (+ 2 3)))" (OctoInt 5),
  testeval "(cond (#t (+ 10 10)) (else (+ 2 3)))" (OctoInt 20),
  testeval "(cond (#f bad) (else (+ 2 3)))" (OctoInt 5),
  testeval "(cond ((equal? 1 2) bad) (#f bad) (else (+ 2 3)))" (OctoInt 5),
  testeval "(cond (#f bad) (#t 88) (else (+ 2 3)))" (OctoInt 88),
  -- bind a new name to a primitive and try it
  testeval "(let ((f +)) (f 3 4))" (OctoInt 7),
  -- rebind * (!!!).  This is a very weird thing to do, but it should work
  testeval "(let ((* +)) (* 3 4))" (OctoInt 7),
  -- tests for eval
  testeval "(eval '(+ 2 3))" (OctoInt 5),
  testeval "(eval (car '(2 3)))" (OctoInt 2),
  testeval "(eval (let((x 10)(y 20)) (+ x y)))" (OctoInt 30),
  testeval "(eval ((lambda(x)(+ x 10))10))" (OctoInt 20),
  testeval "(eval (if (equal? (+ 3 5)(+ 4 4)) #t #f))" (OctoBool True),
  testeval "(eval (cond ((equal? (+ 3 5)(+ 4 4)) #t) (else #f)))" (OctoBool True),
  testeval "(eval \"test\")" (OctoString "test"),
  -- more complex eval example -- make sure the argument to eval is evaluated
  -- in the current environment (here with x bound to 10)
  testeval "(let ((x 10)) (eval (cons '+ (cons x (cons 5 '())))))"
    (OctoInt 15),
  -- another complex eval example -- make sure eval evaluates its expression
  -- in the global environment.  To do this, (yuck) rebind * and make sure the
  -- expression still uses the global *
  -- (If you don't believe this is legal, try pasting the part between
  -- the " marks into racket and evaluating it.)
  testeval "(let ((* null?)) (eval (cons '* (cons 3 (cons 5 '())))))"
    (OctoInt 15),
  -- Recursive function tests
  testeval letrec_fact (OctoInt 24),
  testeval letrec_range (evparse "'(4 3 2 1)"),
  testeval letrec_map (evparse "'(20 40 60)"),
  testeval letrec_curried_map (evparse "'(20 40 60)"),
  testeval letrec_mod2 (evparse "'(1 0)"),
  -- tests handling Racket comments
  testeval "(+ 3 4)  ; a comment at the end" (OctoInt 7),
  testeval complicated_comment_test (OctoInt 7),
  -- tests for the string extra credit question
  testeval "(string-append)" (OctoString ""),
  testeval "(string-append \"fish \" \"clam \" \"squid\" )"
    (OctoString "fish clam squid"),
  testeval "3" (OctoInt 3),
  -- Test for define (Question 16)
  TestLabel "define" (TestCase (assertEqual "test define"
    (test_define ["(define x 3)", "(+ x 10)"] global_env) (OctoInt 13))),
  TestLabel "define with let" (TestCase (assertEqual "test define"
    (test_define ["(define (flip x y) (let ((x (+ y 1))(y (- x 10)))(+ x y)))", "(flip 2 3)"] global_env) (OctoInt (-4)))),
  TestLabel "define with lambda" (TestCase (assertEqual "test define"
    (test_define ["(define (f x) (+ x 10))", "(f 2)"] global_env) (OctoInt 12))),
  TestLabel "define with an empty list" (TestCase (assertEqual "test define"
    (test_define ["(define a '())", "a"] global_env) (OctoList []))),
  TestLabel "define with String" (TestCase (assertEqual "test define"
    (test_define ["(define a \"test\")", "a"] global_env) (OctoString "test"))),
  TestLabel "define with complex stuff" (TestCase (assertEqual "test define"
    (test_define ["(define (way-too-many-lets x y z)(let ((x (+ x y))(y (- x y)))(let ((x (* x 2))(y (* x y 10)))(+ x y z))))"
                  , "(define p 10)", "(+ p (way-too-many-lets 1 2 3))"] global_env) (OctoInt (-11))))
  ]

run = runTestTT tests
