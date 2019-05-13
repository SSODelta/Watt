import Language.Verilog.Parser
import Language.Verilog.Parser.Lex
import Language.Verilog.Parser.Tokens
import Language.Verilog.AST
import Data.BitVec
import ReplaceVerilog
import System.Environment

print_tokens = putStrLn . show . alexScanTokens
print_tokenStrings = putStrLn . (foldr (\a b -> a ++ " " ++ b) "") . (map tokenString) . alexScanTokens

fileParse in_path out_path = readFile in_path >>= writeFile out_path . foldr (\a b -> show a ++ b) "" . map expand . (parseFile [] in_path)
fileParsePrint in_path out_path = readFile in_path >>= print_tokenStrings . foldr (\a b -> show a ++ b) "" . map expand . (parseFile [] in_path)

-- expand :: Module -> Module
-- expand (Module moduleName arguments body) =
--   let new_body = (expand_body body) in
--     (Module moduleName (get_args new_body []) new_body)

expand :: Module -> Module
expand (Module moduleName arguments body) =
  let new_body = replace_xor body in
    (Module moduleName arguments new_body)

replace_xor :: [ModuleItem] -> [ModuleItem]
replace_xor [] = []
replace_xor (Assign lhs (BinOp BWXor e1 e2) : r) = (Assign lhs (BinOp BWOr (BinOp BWAnd (UniOp BWNot e1) e2) (BinOp BWAnd e1 (UniOp BWNot e2)))) : replace_xor r
replace_xor (Assign lhs (Mux e1 e2 e3) : r) = (Assign lhs (BinOp BWOr (BinOp BWAnd e1 e2) (BinOp BWAnd (UniOp BWNot e1) e3))) : replace_xor r
replace_xor (a : r) = a : replace_xor r

-- get_assign :: [ModuleItem] -> [(LHS , Expr)]
-- get_assign [] = []
-- get_assign (Assign lhs expr : r) =
--   (lhs, expr) ++ expand_body r
-- get_assign (k : r) =
--   expand_body r

-- get_ids_in_expr :: Expr -> [LHS]
-- get_ids_in_expr (Ident lhs) = [lhs]
-- get_ids_in_expr (UniOp _ e) = get_ids_in_expr e
-- get_ids_in_expr (BinOp _ e1 e2) = get_ids_in_expr e1 ++ get_ids_in_expr e2
-- get_ids_in_expr _ = []

-- get_identiers :: [(LHS , Expr)] -> [(LHS , [LHS])]
-- get_identiers l = map (\(a,b) -> (a,get_ids_in_expr b)) l

-- get_matrix :: [ModuleItem] -> [ModuleItem]
-- get_matrix s = get_identifiers $ get_assign s

main = getArgs >>= \args -> fileParse (args!!0) (args!!1)
  -- fileParse hd args[0] args[1]
