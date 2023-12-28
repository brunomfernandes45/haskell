import Data.List
import Data.Ord (comparing)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

data StackValue =
  IntValue Integer | BoolValue Bool
  deriving Show

type Stack = [StackValue]
type State = [(String, StackValue)]

-- Pushes an integer onto the stack
push :: Integer -> Stack -> Stack
push n stack = IntValue n:stack

-- Pops two integers from the stack, adds them, and pushes the result
add :: Stack -> Stack
add (IntValue n1:IntValue n2:stack) = IntValue (n1 + n2):stack
add _ = error "Run-time error"

-- Pops two integers from the stack, multiplies them, and pushes the result
mult :: Stack -> Stack
mult (IntValue n1:IntValue n2:stack) = IntValue (n1 * n2):stack
mult _ = error "Run-time error"

-- Pops two integers from the stack, subtracts them, and pushes the result
sub :: Stack -> Stack
sub (IntValue n1:IntValue n2:stack) = IntValue (n1 - n2):stack
sub _ = error "Run-time error"

-- Pushes the boolean value True onto the stack
tru :: Stack -> Stack
tru stack = BoolValue True:stack

-- Pushes the boolean value False onto the stack
fals :: Stack -> Stack
fals stack = BoolValue False:stack

-- Pops two values from the stack and pushes True if they are equal, False otherwise (works for both integers and booleans)
equ :: Stack -> Stack
equ (IntValue n1:IntValue n2:stack) = BoolValue (n1 == n2):stack
equ (BoolValue b1:BoolValue b2:stack) = BoolValue (b1 == b2):stack
equ (IntValue n1:BoolValue b2:stack) = BoolValue False:stack
equ (BoolValue b1:IntValue n2:stack) = BoolValue False:stack
equ _ = error "Run-time error"

-- Pops two integers from the stack and pushes True if the first is less than or equal to the second, False otherwise (only works for integers)
le :: Stack -> Stack
le (IntValue n1:IntValue n2:stack) = BoolValue (n1 <= n2):stack
le _ = error "Run-time error"

-- Pops two booleans from the stack and pushes True if both are True, False otherwise
and' :: Stack -> Stack
and' (BoolValue b1:BoolValue b2:stack) = BoolValue (b1 && b2):stack
and' _ = error "Run-time error"

-- Pops a boolean from the stack and pushes its negation
neg :: Stack -> Stack
neg (BoolValue b:stack) = BoolValue (not b):stack
neg _ = error "Run-time error"

-- Pushes the value of the variable with the given name onto the stack
fetch :: String -> Stack -> State -> (Stack, State)
fetch var stack store = (value:stack, store)
  where value = case lookup var store of
                  Just value -> value
                  Nothing -> error "Run-time error"

-- Pops a value from the stack and stores it in the variable with the given name
-- If the variable already exists, it will be overwritten
store :: String -> Stack -> State -> (Stack, State)
store var (value:stack) state = (stack, updateState var value state)

-- Updates the value of the variable if it exists, if not adds it to the state
updateState :: String -> StackValue -> State -> State
updateState var value [] = [(var, value)]
updateState var value ((existingVar, existingValue):rest)
  | (var == existingVar) = (var, value) : rest
  | otherwise = (existingVar, existingValue) : updateState var value rest

-- Will change the flow of control:  if the top of the stack is the
--value tt (that is, some boolean expression has been evaluated to true), then the
--stack is popped and c1 is to be executed next. Otherwise, if the top element of the
--stack is ff, then it will be popped and c2 will be executed next.
branch :: Code -> Code -> Stack -> State -> (Code, Stack, State)
branch c1 c2 (BoolValue True:stack) store = (c1, stack, store)
branch c1 c2 (BoolValue False:stack) store = (c2, stack, store)
branch _ _ _ _ = error "Run-time error"

-- Dummy instruction that returns the input stack and store
noop :: Stack -> State -> (Stack, State)
noop stack store = (stack, store)

-- Creates an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Converts a stack to a string representation
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToStr stack)

-- Converts a stack value to a string representation
stackValueToStr :: StackValue -> String
stackValueToStr (IntValue n) = show n
stackValueToStr (BoolValue b) = show b

-- Creates an empty state
createEmptyState :: State
createEmptyState = []

-- Converts a state to a string representation and sorts it
state2Str :: State -> String
state2Str = intercalate "," . map (\(var, value) -> var ++ "=" ++ stackValueToStr value) . sortByName
  where
    sortByName = sortBy (comparing fst)

exec ::Inst -> Stack -> State -> (Stack, State)
exec (Push n) stack state = (push n stack, state)
exec Add stack state = (add stack, state)
exec Mult stack state = (mult stack, state)
exec Sub stack state = (sub stack, state)
exec Tru stack state = (tru stack, state)
exec Fals stack state = (fals stack, state)
exec Equ stack state = (equ stack, state)
exec Le stack state = (le stack, state)
exec And stack state = (and' stack, state)
exec Neg stack state = (neg stack, state)
exec (Fetch var) stack state = fetch var stack state
exec (Store var) stack state = store var stack state
exec Noop stack state = noop stack state
-- exec (Branch c1 c2) stack state = (c, stack, state)
--  where (c, _, _) = branch c1 c2
--exec (Loop c1 c2) stack state = (c, stack, state)
--  where (c, _, _) = branch (c1 ++ [Loop c1 c2]) [Noop]

-- Runs the given code with the given stack and state and returns the resulting stack and state
-- given a list of instructions (type defined as Code, i.e. type Code = [Inst]), a stack (type defined as
-- Stack) and that is initially empty, and a storage (type defined as State), runs the
-- list of instructions returning as ouput an empty code list, a stack and the output
-- values in the storage.
run :: (Code, Stack, State) -> (Code, Stack, State)
run ( [], stack , store ) = ( [] , stack , store )
run (code , stack , state) = run ( tail code , stack' , state' )
  where (stack', state') = exec ( head code ) stack state

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- DONE -- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- DONE -- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- DONE -- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- DONE -- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- DONE -- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- DONE -- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- DONE -- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- DONE -- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- DONE -- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- DONE -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- type Aexp = undefined
-- type Bexp = undefined
-- type Stm = undefined
-- type Program = undefined

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
  -- where (_,stack,store) = run (compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")