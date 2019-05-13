module ReplaceVerilog where

import Language.Verilog.Parser
import Language.Verilog.Parser.Lex
import Language.Verilog.Parser.Tokens
import Language.Verilog.AST
import Data.BitVec

replace_vars :: [ModuleItem] -> Identifier -> Identifier -> [ModuleItem]
replace_vars [] a b = []
replace_vars ((Input v xs) : r) a b =
  Input v (map (\x -> if x == a then b else x) xs) : replace_vars r a b
replace_vars ((Output v xs) : r) a b =
  Output v (map (\x -> if x == a then b else x) xs) : replace_vars r a b
replace_vars ((Reg v xs) : r) a b =
  Reg v (map (\x -> if (fst x) == a then (b,snd x) else x) xs) : replace_vars r a b
replace_vars ((Assign e1 e2) : r) a b =
  Assign (replace_var_lhs e1 a b) e2 : replace_vars r a b
replace_vars ((Always sense s) : r) a b =
  Always (sense >>= \s -> Just (replace_vars_sense s a b)) (replace_vars_stmt s a b) : replace_vars r a b
replace_vars (x : r) a b = x : replace_vars r a b

replace_vars_sense (Sense lhs) a b = Sense (replace_var_lhs lhs a b)
replace_vars_sense (SenseOr s1 s2) a b = SenseOr (replace_vars_sense s1 a b) (replace_vars_sense s2 a b)

replace_vars_stmt :: Stmt -> Identifier -> Identifier -> Stmt
replace_vars_stmt (If e s1 s2) a b =
  If (replace_var_expr e a b) (replace_vars_stmt s1 a b) (replace_vars_stmt s2 a b)
replace_vars_stmt (StmtInteger k) a b = StmtInteger k
replace_vars_stmt (StmtReg e f) a b =
  StmtReg e (map (\x -> if fst x == a then (b,snd x) else x) f)
replace_vars_stmt (NonBlockingAssignment lhs e) a b =
  (NonBlockingAssignment (replace_var_lhs lhs a b) (replace_var_expr e a b))
replace_vars_stmt (BlockingAssignment lhs e) a b =
  (NonBlockingAssignment (replace_var_lhs lhs a b) (replace_var_expr e a b))
  
replace_var_lhs (LHS i) a b = LHS (if i == a then b else i)
replace_var_lhs (LHSBit i e) a b = LHSBit (if i == a then b else i) e
replace_var_lhs (LHSRange i r) a b = LHSRange (if i == a then b else i) r

replace_var_expr :: Expr -> Identifier -> Identifier -> Expr
replace_var_expr (Ident i) a b = Ident (if i == a then b else i)
replace_var_expr (BinOp v x y) a b = BinOp v (replace_var_expr x a b) (replace_var_expr y a b)
replace_var_expr (UniOp v x) a b = UniOp v (replace_var_expr x a b)
replace_var_expr x a b = x
