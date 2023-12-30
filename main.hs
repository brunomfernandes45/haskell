import Data.List
import Data.Ord (comparing)
import Data.Char 

-- Data type for instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Type synonym for a list of instructions
type Code = [Inst]

-- Data type for stack values
data StackValue =
  IntValue Integer | BoolValue String
  deriving Show

-- Type synonym for a list of stack values
type Stack = [StackValue]

-- Type synonym for a list of tuples of strings and stack values
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

-- Converts a boolean to a string representation
boolToStr :: Bool -> String
boolToStr True = "tt"
boolToStr False = "ff"

-- Pops two values from the stack and pushes True if they are equal, False otherwise (works for both integers and booleans)
equ :: Stack -> Stack
equ (IntValue n1:IntValue n2:stack) = BoolValue (boolToStr (n1 == n2)):stack
equ (BoolValue b1:BoolValue b2:stack) = BoolValue (boolToStr (b1 == b2)):stack
equ _ = error "Run-time error"

-- Pops two integers from the stack and pushes True if the first is less than or equal to the second, False otherwise (only works for integers)
le :: Stack -> Stack
le (IntValue n1:IntValue n2:stack) = BoolValue (boolToStr (n1 <= n2)):stack
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
fetch var stack state = (value:stack, state)
  where value = case lookup var state of
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

-- Dummy instruction that returns the input stack and state
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
stackValueToStr (BoolValue "tt") = show True
stackValueToStr (BoolValue "ff") = show False

-- Creates an empty state
createEmptyState :: State
createEmptyState = []

-- Converts a state to a string representation and sorts it
state2Str :: State -> String
state2Str = intercalate "," . map (\(var, value) -> var ++ "=" ++ stackValueToStr value) . sortByName
  where
    sortByName = sortBy (comparing fst)

-- Executes the first instruction of the code list with the given stack and state and returns the resulting code, stack and state
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
  where (c, stack', state') = ((branch c1 c2 stack) ++ code, (tail stack), state)
exec ((Loop c1 c2):code) stack state = (c, stack', state')
  where (c, stack', state') = ((c1 ++ [(Branch (c2 ++ [Loop c1 c2]) [Noop])]) ++ code, stack, state)

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

-- Data type for arithmetic expressions
data Aexp = 
  Num Integer | Var String | Add' Aexp Aexp | Sub' Aexp Aexp | Mult' Aexp Aexp 
  deriving Show

-- Data type for boolean expressions
data Bexp = 
  Tru' | Fals' | Neg' Bexp | And' Bexp Bexp | Le' Aexp Aexp | Equi Aexp Aexp | Equb Bexp Bexp
  deriving Show

-- Data type for statements
data Stm =
  Assign String Aexp | If Bexp Program Program | While Bexp Program 
  deriving Show

-- Type synonym for a list of statements
type Program = [Stm]

-- Data type for tokens
data Token =
  NumToken Integer | VarToken String | AddToken | SubToken | MultToken | TruToken | FalsToken | NegToken | 
  AndToken | LeToken | EquiToken | EqubToken | AssignToken | IfToken | ThenToken | ElseToken | WhileToken | 
  DoToken | SemicolonToken | OpenToken | CloseToken | NoopToken
  deriving Show

-- Compiles an arithmetic expression into a list of instructions
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var var) = [Fetch var]
compA (Add' a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (Sub' a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (Mult' a1 a2) = compA a2 ++ compA a1 ++ [Mult]

-- Compiles a boolean expression into a list of instructions
compB :: Bexp -> Code
compB Tru' = [Tru]
compB Fals' = [Fals]
compB (Neg' b) = compB b ++ [Neg]
compB (And' b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (Le' a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (Equi a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (Equb b1 b2) = compB b2 ++ compB b1 ++ [Equ]

-- Compiles a program into a list of instructions
compile :: Program -> Code
compile [] = []
compile (Assign var a:rest) = compA a ++ [Store var] ++ compile rest
compile (If b p1 p2:rest) = compB b ++ [Branch (compile p1) (compile p2)] ++ compile rest
compile (While b p:rest) = [Loop (compB b) (compile p)] ++ compile rest

-- Lexes the given string and returns the corresponding list of tokens
lexer :: String -> [Token]
lexer [] = []
lexer input@(c:cs)
  | "<=" `isPrefixOf` input = LeToken : lexer (drop 2 input)
  | ":=" `isPrefixOf` input = AssignToken : lexer (drop 2 input)
  | "==" `isPrefixOf` input = EquiToken : lexer (drop 2 input)
  | "while" `isPrefixOf` input = WhileToken : lexer (drop 5 input)
  | "do" `isPrefixOf` input = DoToken : lexer (drop 2 input)
  | "True" `isPrefixOf` input = TruToken : lexer (drop 4 input)
  | "False" `isPrefixOf` input = FalsToken : lexer (drop 5 input)
  | "not" `isPrefixOf` input = NegToken : lexer (drop 3 input)
  | "and" `isPrefixOf` input = AndToken : lexer (drop 3 input)
  | "if" `isPrefixOf` input = IfToken : lexer (drop 2 input)
  | "then" `isPrefixOf` input = ThenToken : lexer (drop 4 input)
  | "else" `isPrefixOf` input = ElseToken : lexer (drop 4 input)
  | isDigit c = (NumToken (read (takeWhile isDigit input))) : lexer (dropWhile isDigit input)
  | isLower c = VarToken (takeWhile isAlpha input) : lexer (dropWhile isAlpha input)
  | isSpace c = lexer cs
  | "\n" `isPrefixOf` input = lexer (drop 1 input)
  | "=" `isPrefixOf` input = EqubToken : lexer (drop 1 input)
  | "+" `isPrefixOf` input = AddToken : lexer (drop 1 input)
  | "-" `isPrefixOf` input = SubToken : lexer (drop 1 input)
  | "*" `isPrefixOf` input = MultToken : lexer (drop 1 input)
  | "(" `isPrefixOf` input = OpenToken : lexer (drop 1 input)
  | ")" `isPrefixOf` input = CloseToken : lexer (drop 1 input)
  | ";" `isPrefixOf` input = SemicolonToken : lexer (drop 1 input)
  | otherwise = error ("unexpected character: '" ++ show c ++ "'")

-- Parses an integer
parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (NumToken n : restTokens) = Just (Num n, restTokens)
parseInt tokens = Nothing


-- Parses a variable
parseVar :: [Token] -> Maybe (Aexp, [Token])
parseVar (VarToken var : restTokens) = Just (Var var, restTokens)
parseVar tokens = Nothing


-- Parses an Assign statement
parseAssign :: [Token] -> Maybe (Stm, [Token])
parseAssign (VarToken var : AssignToken : TruToken : restTokens) 
  = error "Run-time error"
parseAssign (VarToken var : AssignToken : FalsToken : restTokens)
  = error "Run-time error"
parseAssign (VarToken var : AssignToken : restTokens) 
  = case parseAexp restTokens of
    Just (aexp, SemicolonToken : restTokens') -> Just (Assign var aexp, restTokens')
    _ -> Nothing
parseAssign tokens = Nothing


-- Parses an integer or variable
parseAtom :: [Token] -> Maybe (Aexp, [Token])
parseAtom tokens = case parseInt tokens of
  Just (aexp, restTokens) -> Just (aexp, restTokens)
  _ -> case parseVar tokens of
    Just (aexp, restTokens) -> Just (aexp, restTokens)
    _ -> Nothing


-- Parses an integer, variable or Multiplication arithmetic expressions
parseMult :: [Token] -> Maybe (Aexp, [Token])
parseMult tokens 
  = case parseAtom tokens of
    Just (aexp1, MultToken : restTokens) -> 
      case parseMult restTokens of
        Just (aexp2, restTokens') -> Just (Mult' aexp1 aexp2, restTokens')
        Nothing -> Nothing
    result -> result


-- Parses all types of arithmetic expressions except for parenthesized ones
parseAddOrSub :: [Token] -> Maybe (Aexp, [Token])
parseAddOrSub tokens 
  = case parseMult tokens of
    Just (aexp1, AddToken : restTokens) -> 
      case parseMult restTokens of
        Just (aexp2, restTokens') -> Just (Add' aexp1 aexp2, restTokens')
        Nothing -> Nothing
    Just (aexp1, SubToken : restTokens) -> 
      case parseMult restTokens of
        Just (aexp2, restTokens') -> Just (Sub' aexp1 aexp2, restTokens')
        Nothing -> Nothing
    result -> result

-- Parses an integer, variable or Parenthesized arithmetic expressions
parseAtomOrPar :: [Token] -> Maybe (Aexp, [Token])
parseAtomOrPar (NumToken n : restTokens) = Just (Num n, restTokens)
parseAtomOrPar (VarToken var : restTokens) = Just (Var var, restTokens)
parseAtomOrPar (OpenToken : restTokens) 
  = case parseAexp restTokens of
      Just (aexp, CloseToken : restTokens') -> Just (aexp, restTokens')
      Just _ -> Nothing
      Nothing -> Nothing
parseAtomOrPar _ = Nothing

-- Parses an integer, variable, Multiplication or Parenthesized arithmetic expressions
parseMultOrAtomOrPar :: [Token] -> Maybe (Aexp, [Token])
parseMultOrAtomOrPar tokens 
  = case parseAtomOrPar tokens of
      Just (aexp1, MultToken : restTokens) -> 
        case parseMultOrAtomOrPar restTokens of
          Just (aexp2, restTokens') -> Just (Mult' aexp1 aexp2, restTokens')
          Nothing -> Nothing
      result -> result


-- Parses all types of arithmetic expressions
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens 
  = case parseMultOrAtomOrPar tokens of
      Just (aexp1, AddToken : restTokens) -> 
        case parseMultOrAtomOrPar restTokens of
          Just (aexp2, restTokens') -> Just (Add' aexp1 aexp2, restTokens')
          Nothing -> Nothing
      Just (aexp1, SubToken : restTokens) -> 
        case parseMultOrAtomOrPar restTokens of
          Just (aexp2, restTokens') -> Just (Sub' aexp1 aexp2, restTokens')
          Nothing -> Nothing
      result -> result

-- Parses boolean values
parseBool :: [Token] -> Maybe (Bexp, [Token])
parseBool (TruToken : restTokens) = Just (Tru', restTokens)
parseBool (FalsToken : restTokens) = Just (Fals', restTokens)
parseBool _ = Nothing

-- Parses boolean values and Le or Equi boolean expressions
parseLeOrEqui :: [Token] -> Maybe (Bexp, [Token])
parseLeOrEqui tokens 
  = case parseAexp tokens of
    Just (aexp1, LeToken : restTokens) -> 
      case parseAexp restTokens of
        Just (aexp2, restTokens') -> Just (Le' aexp1 aexp2, restTokens')
        Nothing -> Nothing
    Just (aexp1, EquiToken : restTokens) -> 
      case parseAexp restTokens of
        Just (aexp2, restTokens') -> Just (Equi aexp1 aexp2, restTokens')
        Nothing -> Nothing
    _ -> Nothing


-- Parses boolean values, Neg, Le or Equi boolean expressions
parseNeg :: [Token] -> Maybe (Bexp, [Token])
parseNeg (TruToken : restTokens) = Just (Tru', restTokens)
parseNeg (FalsToken : restTokens) = Just (Fals', restTokens)
parseNeg (NegToken : restTokens) 
  = case parseLeOrEqui restTokens of
      Just (bexp, restTokens') -> Just (Neg' bexp, restTokens')
      Nothing -> case parseBool restTokens of
        Just (bexp, restTokens') -> Just (Neg' bexp, restTokens')
        Nothing -> Nothing
parseNeg tokens 
  = case parseLeOrEqui tokens of
    Just (bexp1, restTokens) -> Just (bexp1, restTokens)
    Nothing -> Nothing


-- Parses all types of boolean expressions except for And and Parenthesized ones
parseEqub :: [Token] -> Maybe (Bexp, [Token])
parseEqub tokens 
  = case parseNeg tokens of
    Just (bexp1, EqubToken : restTokens) -> 
      case parseNeg restTokens of
        Just (bexp2, restTokens') -> Just (Equb bexp1 bexp2, restTokens')
        Nothing -> Nothing
    result -> result


-- Parses all types of boolean expressions except for Parenthesized ones
parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens 
  = case parseEqub tokens of
    Just (bexp1, AndToken : restTokens) -> 
      case parseEqub restTokens of
        Just (bexp2, restTokens') -> Just (And' bexp1 bexp2, restTokens')
        Nothing -> Nothing
    result -> result


-- Parses boolean values and Parenthesized boolean expressions
parseBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrPar (TruToken : restTokens) = Just (Tru', restTokens)
parseBoolOrPar (FalsToken : restTokens) = Just (Fals', restTokens)
parseBoolOrPar (OpenToken : restTokens) 
  = case parseBexp restTokens of
      Just (bexp, CloseToken : restTokens') -> Just (bexp, restTokens')
      Just _ -> Nothing
      Nothing -> Nothing
parseBoolOrPar _ = Nothing


-- Parses boolean values and Le, Equi or Parenthesized boolean expressions
parseBoolOrLeOrEquiOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrLeOrEquiOrPar tokens 
  = case parseLeOrEqui tokens of
    Just (bexp1, restTokens) -> Just (bexp1, restTokens)
    Nothing -> case parseBoolOrPar tokens of
      Just (bexp1, restTokens) -> Just (bexp1, restTokens)
      Nothing -> Nothing

-- Parses all types of boolean expressions except for And and Neg
parseNegOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNegOrPar (NegToken : restTokens) 
  = case parseBoolOrPar restTokens of
      Just (bexp, restTokens') -> Just (Neg' bexp, restTokens')
      Nothing -> Nothing
parseNegOrPar tokens
  = case parseBoolOrLeOrEquiOrPar tokens of
    Just (bexp1, restTokens) -> Just (bexp1, restTokens)
    Nothing -> Nothing

-- Parses all types of boolean expressions except for And
parseEqubOrPar :: [Token] -> Maybe (Bexp, [Token])
parseEqubOrPar tokens 
  = case parseNegOrPar tokens of
    Just (bexp1, EqubToken : restTokens) -> 
      case parseNegOrPar restTokens of
        Just (bexp2, restTokens') -> Just (Equb bexp1 bexp2, restTokens')
        Nothing -> Nothing
    result -> result

-- Parses all types of boolean expressions
parseAndOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrPar tokens 
  = case parseEqubOrPar tokens of
    Just (bexp1, AndToken : restTokens) -> 
      case parseEqubOrPar restTokens of
        Just (bexp2, restTokens') -> Just (And' bexp1 bexp2, restTokens')
        Nothing -> Nothing
    result -> result

-- Parses a boolean expression
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens 
  = case parseAndOrPar tokens of
    Just (bexp1, restTokens) -> Just (bexp1, restTokens)
    Nothing -> Nothing


-- Parses an If statement
parseIf :: [Token] -> Maybe (Stm, [Token])
parseIf (IfToken : restTokens) 
  = case parseBexp restTokens of
    Just (bexp, ThenToken : restTokens') -> 
      case parseSubProgram restTokens' [] of
        Just (p1, ElseToken : restTokens'') -> 
          case parseSubProgram restTokens'' [] of
            Just (p2, restTokens''') -> Just (If bexp p1 p2, restTokens''')
            Nothing -> Nothing
        Just (p1, restTokens'') -> Just (If bexp p1 [], restTokens'')
        Nothing -> Nothing
    _ -> Nothing
parseIf tokens = Nothing

-- Parses a While statement
parseWhile :: [Token] -> Maybe (Stm, [Token])
parseWhile (WhileToken : restTokens) 
  = case parseBexp restTokens of
    Just (bexp, DoToken : restTokens') -> 
      case parseSubProgram restTokens' [] of
        Just (p, restTokens'') -> Just (While bexp p, restTokens'')
        Nothing -> Nothing
    _ -> Nothing
parseWhile tokens = Nothing


-- Parses the given list of tokens and returns the corresponding program
parseSubProgram :: [Token] -> Program -> Maybe (Program, [Token])
parseSubProgram [] _ = Nothing
parseSubProgram (OpenToken : restTokens) program
  = case parseSubProgram restTokens program of
    Just (p, CloseToken : SemicolonToken : restTokens') -> Just (p, restTokens')
    Just (p, CloseToken : restTokens') -> Just (p, restTokens')
    Just (p, restTokens') -> parseSubProgramToClose restTokens' p
parseSubProgram tokens program = case parseAssign tokens of
  Just (stm, restTokens) -> Just (program ++ [stm], restTokens)
  Nothing -> case parseIf tokens of
    Just (stm, restTokens) -> Just (program ++ [stm], restTokens)
    Nothing -> case parseWhile tokens of
      Just (stm, restTokens) -> Just (program ++ [stm], restTokens)
      Nothing -> Nothing


-- Parses the given list of tokens until it finds a CloseToken and returns the corresponding program
parseSubProgramToClose :: [Token] -> Program -> Maybe (Program, [Token])
parseSubProgramToClose [] _ = Nothing
parseSubProgramToClose (CloseToken : SemicolonToken : restTokens) program = Just (program, restTokens)
parseSubProgramToClose (CloseToken : restTokens) program = Just (program, restTokens)
parseSubProgramToClose tokens program = case parseAssign tokens of
  Just (stm, restTokens) -> parseSubProgramToClose restTokens (program ++ [stm])
  Nothing -> case parseIf tokens of
    Just (stm, restTokens) -> parseSubProgramToClose restTokens (program ++ [stm])
    Nothing -> case parseWhile tokens of
      Just (stm, restTokens) -> parseSubProgramToClose restTokens (program ++ [stm])
      Nothing -> Nothing

-- Parses the given string and returns the corresponding program
parse :: String -> Program
parse = parseProgram . lexer

-- Parses the given list of tokens and returns the corresponding program
parseProgram :: [Token] -> Program
parseProgram [] = []
parseProgram tokens = case parseAssign tokens of
  Just (stm, restTokens) -> stm : parseProgram restTokens
  Nothing -> case parseIf tokens of
    Just (stm, restTokens) -> stm : parseProgram restTokens
    Nothing -> case parseWhile tokens of
      Just (stm, restTokens) -> stm : parseProgram restTokens
      Nothing -> error ("unexpected error while parsing" ++ show tokens)

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- To help you test your compiler
testCompiler :: Program -> (String, String)
testCompiler programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (compile (programCode), createEmptyStack, createEmptyState)


-- Examples:

-- DONE -- testCompiler [Assign "x" (Num 5), Assign "x" (Sub' (Var "x") (Num 1))]
-- DONE -- testCompiler [If (And' (Neg' (Tru')) (Equb (Le' (Num 2) (Num 5)) (Equi (Num 3) (Num 4)))) [Assign "x" (Num 1)] [Assign "y" (Num 2)]]
-- DONE -- testCompiler [Assign "i" (Num 10), Assign "fact" (Num 1), While (Neg' (Equi (Var "i") (Num 1))) [Assign "fact" (Mult' (Var "fact") (Var "i")), Assign "i" (Sub' (Var "i") (Num 1))]]

-- DONE -- testParser "x := 5; x := x - 1;" == ("","x=4")
-- DONE -- testParser "x := 0 - 2;" == ("","x=-2")
-- DONE -- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- DONE -- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- DONE -- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- DONE -- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- DONE -- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- DONE -- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- DONE -- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- DONE -- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- DONE -- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- DONE -- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
