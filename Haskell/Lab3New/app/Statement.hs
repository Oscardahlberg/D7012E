module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Read String |
    Write Expr.T |
    While Expr.T Statement |
    Skip |
    Begin [Statement] |
    Repeat Statement Expr.T
    deriving Show

statements = iter statement
statement = assignment ! ifCon ! Statement.read ! Statement.write ! Statement.while ! skip ! begin ! Statement.repeat

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifCon = (accept "if" -# Expr.parse) # (require "then" -# statement) # (require "else" -# statement) >-> buildIfCon
buildIfCon ((e, s1), s2) = If e s1 s2

read = (accept "read" -# word) #- require ";" >-> buildRead
buildRead = Read

write = (accept "write" -# Expr.parse) #- require ";" >-> buildWrite
buildWrite = Write

while = (accept "while" -# Expr.parse) # (require "do" -# statement) >-> buildWhile
buildWhile (e, s1) = While e s1

skip = accept "skip;" >-> buildSkip
buildSkip _ = Skip

begin = (accept "begin" -# statements) #- require "end" >-> buildBegin
buildBegin = Begin

repeat = accept "repeat" -# statement # require "until" -# Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s1, e) = Repeat s1 e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment assignee toAssign : stmts) dict input =
    exec stmts (Dictionary.insert (assignee, Expr.value toAssign dict) dict) input

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
        then exec (thenStmts: stmts) dict input
        else exec (elseStmts: stmts) dict input

exec (Read toRead: stmts) dict (inputF:inputR) =
    exec stmts (Dictionary.insert (toRead, inputF) dict) inputR

exec (Write toWrite : stmts) dict input =
    Expr.value toWrite dict: exec stmts dict input

exec (While cond doStmts: stmts) dict input =
    if (Expr.value cond dict)>0
        then exec (doStmts: (While cond doStmts: stmts)) dict input
        else exec stmts dict input

exec (Skip:stmts) dict input =
    exec stmts dict input

exec (Begin stmt : stmts) dict input =
    exec (stmt ++ stmts) dict input

exec (Repeat doStmts cond: stmts) dict input = 
    exec (doStmts: after) dict input
      where
	after
	 | Expr.value cond dict <= 0 = Repeat doStmts cond : stmts
	 | otherwise = stmts

exec _ _ _ = []

fromParsed :: T -> String
fromParsed (If e s1 s2) = "if " ++ toString e ++ " then\n" ++ toString s1 ++ " else\n" ++ toString s2 ++ "\n"
fromParsed (Assignment v e) = v ++ " := " ++ toString e ++ ";\n"
fromParsed (Read v) = "read " ++ v ++ ";\n"
fromParsed (Write e) = "write " ++ toString e ++ ";\n"
fromParsed (While e s1) = "while " ++ toString e ++  "do\n" ++ toString s1 ++ "\n"
fromParsed Skip = "skip;\n"
fromParsed (Begin (s:s1)) = "begin\n" ++ toString s ++ stmts s1 ++ "end" ++ "\n"
    where
        stmts :: [Statement] -> String
        stmts [] = ""
        stmts [sF] = toString sF
        stmts (sF:sR) = toString sF ++ stmts sR
fromParsed (Repeat s1 e) = "repeat\n" ++ toString s1 ++ "until " ++ toString e ++ "\n"

instance Parse Statement where
  parse = statement
  toString = fromParsed
