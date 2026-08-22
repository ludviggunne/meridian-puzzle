{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Bifunctor (first)
import Game
import Data.Function
import Debug.Trace

main :: IO()
main = gameMain @Graph @Position

data Node = Node { getState    :: State
                 , getPosition :: Position }
                 deriving (Eq,Show,Ord)

newtype Graph = Graph { getGraph :: [Node] } deriving (Eq,Ord)

data State = Off | On deriving (Eq,Ord)

type Position = (Int,Int)

instance Read Graph where
    readsPrec :: Int -> String -> [(Graph,String)]
    readsPrec _ = pure . first Graph . go 0 0
        where go :: Int -> Int -> String -> ([Node],String)
              go c l (' ':tl)  = go (c+1) l tl
              go _ l ('\n':tl) = go 0 (l+1) tl
              go c l ('.':tl)  = first (Node Off (c,l):) $ go (c+1) l tl
              go c l ('o':tl)  = first (Node On (c,l):) $ go (c+1) l tl
              go _ _ s         = ([],s)


instance Show Graph where
    show :: Graph -> String
    show = go 0 0 . getGraph
        where go :: Int -> Int -> [Node] -> String
              go c l nodes@(Node s (c',l'):tl)
                | l < l'             = '\n':go 0 (l+1) nodes
                | l == l' && c < c'  = ' ':go (c+1) l nodes
                | l == l' && c == c' = show s <> go (c+1) l tl
                | otherwise          = error "?"
              go _ _ _ = ""

instance Show State where
    show :: State -> String
    show On  = "●"
    show Off = "○"

instance Game Graph Position where
    transition :: Position -> Graph -> Graph
    transition pos (Graph g) = Graph $ map mapFn g
        where mapFn :: Node -> Node
              mapFn node@(Node s pos')
                | pos' `elem` neighbourhood pos = toggle node
                | otherwise      = node

              toggle :: Node -> Node
              toggle (Node Off pos) = Node On pos
              toggle (Node On pos) = Node Off pos

              neighbourhood :: Position -> [Position]
              neighbourhood (x,y) = [(x+dx,y+dy) | (dx,dy)<-offsets]

              offsets :: [Position]
              offsets = [ (0,0),  (-2,0), (2,0)
                        , (0,1),  (0,-1), (1,1)
                        , (1,-1), (-1,1), (-1,-1) ]

    final :: Graph -> Bool
    final = all ((== On) . getState) . getGraph

    actions :: Graph -> [Position]
    actions = map getPosition . getGraph
