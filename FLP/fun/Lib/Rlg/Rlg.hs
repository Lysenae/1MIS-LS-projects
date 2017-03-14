-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Rlg where

type Symbol = String

data Rule = Rule
    { left  :: Symbol
    , right :: [Symbol]
    }
  deriving (Show)

data Rlg = Rlg
    { nonterminals :: [Symbol]
    , terminals    :: [Symbol]
    , rules        :: [Rule]
    , start        :: Symbol
    }
  deriving (Show)
