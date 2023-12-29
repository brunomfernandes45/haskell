import Data.List
import Data.Ord (comparing)
import Data.Char 

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

data StackValue =
  IntValue Integer | BoolValue String
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
tru stack = BoolValue "tt":stack

-- Pushes the boolean value False onto the stack
fals :: Stack -> Stack
fals stack = BoolValue "ff":stack

-- Pops two values from the stack and pushes True if they are equal, False otherwise (works for both integers and booleans)
equ :: Stack -> Stack
equ (IntValue n1:IntValue n2:stack) = BoolValue (n1 == n2):stack
equ (BoolValue b1:BoolValue b2:stack) = BoolValue (b1 == b2):stack
equ _ = error "Run-time error"

-- Pops two integers from the stack and pushes True if the first is less than or equal to the second, False otherwise (only works for integers)
le :: Stack -> Stack
le (IntValue n1:IntValue n2:stack) = BoolValue (n1 <= n2):stack
le _ = error "Run-time error"

-- Pops two booleans from the stack and pushes True if both are True, False otherwise
and' :: Stack -> Stack
and' (BoolValue "tt":BoolValue "tt":stack) = BoolValue "tt":stack
and' (BoolValue "tt":BoolValue "ff":stack) = BoolValue "ff":stack
and' (BoolValue "ff":BoolValue "tt":stack) = BoolValue "ff":stack
and' (BoolValue "ff":BoolValue "ff":stack) = BoolValue "ff":stack
and' _ = error "Run-time error"

-- Pops a boolean from the stack and pushes its negation
neg :: Stack -> Stack
neg (BoolValue "tt":stack) = BoolValue "ff":stack
neg (BoolValue "ff":stack) = BoolValue "tt":stack
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
-- value tt (that is, some boolean expression has been evaluated to true), then the
-- stack is popped and c1 is to be executed next. Otherwise, if the top element of the
-- stack is ff, then it will be popped and c2 will be executed next.
branch :: Code -> Code -> Stack -> Code
branch c1 c2 (BoolValue "tt":stack) = c1
branch c1 c2 (BoolValue "ff":stack) = c2
branch c1 c2 stack = error $ "Run-time error with c1: " ++ show c1 ++ " and c2: " ++ show c2 ++ " and stack: " ++ stack2Str stack

-- Dummy instruction that returns the input stack and store
noop :: Stack -> State -> (Stack, State)
noop stack state = (stack, state)

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

exec :: Code -> Stack -> State -> (Code, Stack, State)
exec ((Push n):code) stack state = (code, stack', state)
  where stack' = push n stack
exec ((Add):code) stack state = (code, stack', state)
  where stack' = add stack
exec ((Mult):code) stack state = (code, stack', state)
  where stack' = mult stack
exec ((Sub):code) stack state = (code, stack', state)
  where stack' = sub stack
exec ((Tru):code) stack state = (code, stack', state)
  where stack' = tru stack
exec ((Fals):code) stack state = (code, stack', state)
  where stack' = fals stack
exec ((Equ):code) stack state = (code, stack', state)
  where stack' = equ stack
exec ((Le):code) stack state = (code, stack', state)
  where stack' = le stack
exec ((And):code) stack state = (code, stack', state)
  where stack' = and' stack
exec ((Neg):code) stack state = (code, stack', state)
  where stack' = neg stack
exec ((Fetch var):code) stack state = (code, stack', state')
  where (stack', state') = fetch var stack state
exec ((Store var):code) stack state = (code, stack', state')
  where (stack', state') = store var stack state
exec ((Noop):code) stack state = (code, stack', state')
  where (stack', state') = noop stack state
exec ((Branch c1 c2):code) stack state = (c, stack', state')
  where (c, stack', state') = ((branch c1 c2 stack), (tail stack), state)
exec ((Loop c1 c2):code) stack state = (c, stack', state')
  where (c, stack', state') = (c1 ++ [(Branch (c2 ++ [Loop c1 c2]) [Noop])], stack, state)

-- Runs the given code with the given stack and state and returns the resulting stack and state
-- given a list of instructions (type defined as Code, i.e. type Code = [Inst]), a stack (type defined as
-- Stack) and that is initially empty, and a storage (type defined as State), runs the
-- list of instructions returning as ouput an empty code list, a stack and the output
-- values in the storage.
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (code, stack, state) = run (code', stack', state')
  where (code', stack', state') = exec code stack state

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- DONE -- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- DONE -- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- DONE -- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- DONE -- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- DONE -- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- DONE -- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- DONE -- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- DONE -- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- DONE -- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- DONE -- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- DONE -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program
data Aexp = Num Integer | Var String | Add' Aexp Aexp | Sub' Aexp Aexp | Mult' Aexp Aexp deriving Show

data Bexp = 
  Tru' | Fals' | Neg' Bexp | And' Bexp Bexp | Le' Aexp Aexp | Equi Aexp Aexp | Equb Bexp Bexp
  deriving Show

data Stm =
  Assign String Aexp | If Bexp Program Program | While Bexp Program 
  deriving Show
  
type Program = [Stm]

compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var var) = [Fetch var]
compA (Add' a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (Sub' a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (Mult' a1 a2) = compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB Tru' = [Tru]
compB Fals' = [Fals]
compB (Neg' b) = compB b ++ [Neg]
compB (And' b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (Le' a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (Equi a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (Equb b1 b2) = compB b2 ++ compB b1 ++ [Equ]

compile :: Program -> Code
compile [] = []
compile (Assign var a:rest) = compA a ++ [Store var] ++ compile rest
compile (If b p1 p2:rest) = compB b ++ [Branch (compile p1) (compile p2)] ++ compile rest
compile (While b p:rest) = [Loop (compB b) (compile p)] ++ compile rest

lexer :: String -> [String]
lexer [] = []
lexer input@(c:cs)
  | "<=" `isPrefixOf` input = "<=" : lexer (drop 2 input)
  | ":=" `isPrefixOf` input = ":=" : lexer (drop 2 input)
  | "==" `isPrefixOf` input = "==" : lexer (drop 2 input)
  | "while" `isPrefixOf` input = "while" : lexer (drop 5 input)
  | "do" `isPrefixOf` input = "do" : lexer (drop 2 input)
  | "==" `isPrefixOf` input = "==" : lexer (drop 2 input)
  | "True" `isPrefixOf` input = "True" : lexer (drop 4 input)
  | "False" `isPrefixOf` input = "False" : lexer (drop 5 input)
  | "not" `isPrefixOf` input = "not" : lexer (drop 3 input)
  | "and" `isPrefixOf` input = "and" : lexer (drop 3 input)
  | "if" `isPrefixOf` input = "if" : lexer (drop 2 input)
  | "then" `isPrefixOf` input = "then" : lexer (drop 4 input)
  | "else" `isPrefixOf` input = "else" : lexer (drop 4 input)
  | isDigit c = (takeWhile isDigit input) : lexer (dropWhile isDigit input)
  | isLower c = (takeWhile isAlpha input) : lexer (dropWhile isAlpha input)
  | " " `isPrefixOf` input = lexer (drop 1 input)
  | "\n" `isPrefixOf` input = lexer (drop 1 input)
  | "=" `isPrefixOf` input = "=" : lexer (drop 1 input)
  | "+" `isPrefixOf` input = "+" : lexer (drop 1 input)
  | "-" `isPrefixOf` input = "-" : lexer (drop 1 input)
  | "*" `isPrefixOf` input = "*" : lexer (drop 1 input)
  | "(" `isPrefixOf` input = "(" : lexer (drop 1 input)
  | ")" `isPrefixOf` input = ")" : lexer (drop 1 input)
  | ";" `isPrefixOf` input = ";" : lexer (drop 1 input)
  | otherwise = error $ "Cannot parse: " ++ input


--parse :: String -> Program
parse =  undefined-- TODO

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
  -- where (_, stack, store) = run (compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")