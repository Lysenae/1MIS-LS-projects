import System.Environment
import System.Exit

main = getArgs >>= parseCmdArg

parseCmdArg ["-h"] = usage >> Main.exitSuccess
parseCmdArg a      = badArg a >> Main.exitFailure

exitSuccess = exitWith ExitSuccess
exitFailure = exitWith (ExitFailure 1)

usage = putStrLn "Usage:\n\tplg-2-nka [options] [input]"
badArg [a] = putStrLn ("Wrong argument found: '" ++ a ++ "'")
