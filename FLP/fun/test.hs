-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

import Lib.Misc.Misc

import Lib.RLG.RLG
import Lib.RLG.Handler
import Lib.RLG.Transform.SimpleRules

main :: IO ()
main = print (trSimpleRule rl trules)

--print (findDerivations "A" ["C", "B", "A"] trules)
--(findXRules "A" "B" trules)
--(getSetNX "B" (trules))

rl :: Rule
rl = Rule "A" ["A"]

trules :: [Rule]
trules = [Rule "A" ["A"], Rule "B" ["C"], Rule "C" ["#"], Rule "B" ["b","C"]]
