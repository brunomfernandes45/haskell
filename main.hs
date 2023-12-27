import Data.List
import Distribution.Simple.Test (test)

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
and :: Stack -> Stack
and (BoolValue b1:BoolValue b2:stack) = BoolValue (b1 && b2):stack
and _ = error "Run-time error"

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

-- Creates an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Converts a stack to a string representation
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToStr (reverse stack))

-- Converts a stack value to a string representation
stackValueToStr :: StackValue -> String
stackValueToStr (IntValue n) = show n
stackValueToStr (BoolValue b) = show b

-- Creates an empty state
createEmptyState :: State
createEmptyState = []

-- Converts a state to a string representation
state2Str :: State -> String
state2Str = intercalate "," . map (\(var, value) -> var ++ "=" ++ stackValueToStr value)

-- Runs the given code with the given stack and state and returns the resulting stack and state
run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

-- To help you test your assembler
-- testAssembler :: Code -> (String, String)
-- testAssembler code = (stack2Str stack, state2Str state)
  -- where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

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