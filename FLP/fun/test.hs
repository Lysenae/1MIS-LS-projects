-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

import Lib.Misc.Misc

import Lib.RLG.RLG
import Lib.RLG.Handler
import Lib.RLG.Transform.SimpleRules

main :: IO ()
main = print (findXRules "A" "A" trules)

--print (findDerivations "A" ["C", "B", "A"] trules)
--(findXRules "A" "B" trules)
--(getSetNX "B" (trules))

trules :: [Rule]
trules = [Rule "A" ["B"], Rule "B" ["C"], Rule "C" ["#"], Rule "B" ["b","C"]]
