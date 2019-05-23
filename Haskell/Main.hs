import System.Environment
import Flatten
import VerilogParse
import CNF

process_input "0" = flatten
process_input "1" = optimize

main = getArgs >>= \args -> fileParse (args!!1) (args!!2) (process_input (args!!0))
