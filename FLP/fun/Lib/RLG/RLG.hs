-- Project: FLP #1 - plg-2-nka
-- Author:  Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.RLG.RLG where

import Lib.Type.Symbol

data Rule = Rule
    { left  :: Symbol
    , right :: [Symbol]
    }
  deriving (Show, Eq, Ord)

data RLG = RLG
    { nonterminals :: [Symbol]
    , terminals    :: [Symbol]
    , rules        :: [Rule]
    , start        :: Symbol
    }
  deriving (Show)
