module Lib.CmdArgs.Functions where

import Lib.CmdArgs.Data

unwrap :: Either String CmdArgs -> CmdArgs
unwrap (Right (CmdArgs pi p1 p2 inf)) = CmdArgs pi p1 p2 inf
