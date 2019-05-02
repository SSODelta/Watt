import Language.Verilog.Parser
import Language.Verilog.Parser.Lex
import Language.Verilog.Parser.Tokens
import Language.Verilog.AST

print_tokens = putStrLn . show . alexScanTokens
print_tokenStrings = putStrLn . (foldr (\a b -> a ++ " " ++ b) "") . (map tokenString) . alexScanTokens

fileParse path = readFile path >>= putStrLn . show . map expand . (parseFile [] path)

expand :: Module -> Module
expand (Module moduleName arguments body) = (Module moduleName arguments (expand_body body))

expand_body :: [ModuleItem] -> [ModuleItem]
expand_body [] = []
expand_body (a : r) = (Input Nothing ["23"]) : a : r

main = fileParse "testfile"
