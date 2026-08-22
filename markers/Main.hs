{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List
import Game

main :: IO ()
main = gameMain @Board @Action

data Cell = Empty|Block|Mark|Goal|MG deriving (Eq,Ord)

data Action = L|R|U|D deriving (Enum,Bounded)

type Board = [[Cell]]

instance Show Cell where
    show :: Cell -> String
    show Empty = " "
    show Block = "\x1b[33m█\x1b[0m"
    show Mark  = "\x1b[34m∙\x1b[0m"
    show Goal  = "\x1b[34m○\x1b[0m"
    show MG    = "\x1b[34m●\x1b[0m"

instance {-# OVERLAPPING #-} Show Board where
    show :: Board -> String
    show = unlines . map (concatMap show)

instance {-# OVERLAPPING #-} Read Board where
    readsPrec :: Int -> String -> [(Board,String)]
    readsPrec _ str = case impl str of
                        Nothing    -> []
                        Just board -> [(board,"")]
        where impl :: String -> Maybe Board
              impl = traverse (traverse cell) . lines
              cell :: Char -> Maybe Cell
              cell '.' = Just Empty
              cell 'X' = Just Block
              cell 'o' = Just Mark
              cell 'O' = Just Goal
              cell _   = Nothing

instance Show Action where
    show :: Action -> String
    show L   = "◄"
    show R   = "►"
    show U   = "▲"
    show D   = "▼"

instance Read Action where
    readsPrec :: Int -> String -> [(Action,String)]
    readsPrec _ s
        | Just tl <- stripPrefix "\x1b[A" s = [(U,tl)]
        | Just tl <- stripPrefix "\x1b[B" s = [(D,tl)]
        | Just tl <- stripPrefix "\x1b[C" s = [(R,tl)]
        | Just tl <- stripPrefix "\x1b[D" s = [(L,tl)]
        | ('k':tl) <- s = [(U,tl)]
        | ('j':tl) <- s = [(D,tl)]
        | ('l':tl) <- s = [(R,tl)]
        | ('h':tl) <- s = [(L,tl)]
        | otherwise  = []

instance Game Board Action where
    transition :: Action -> Board -> Board
    transition a = itr a . map shift . tr a
        where shift :: [Cell] -> [Cell]
              shift (Empty:Mark:tl) = Mark:shift (Empty:tl)
              shift (Goal:Mark:tl)  = MG:shift (Empty:tl)
              shift (Empty:MG:tl)   = Mark:shift (Goal:tl)
              shift (Goal:MG:tl)    = MG:shift (Goal:tl)
              shift (a:b:tl)        = a:shift (b:tl)
              shift [a]             = [a]

              tr, itr :: Action -> Board -> Board
              tr L  = id
              tr R  = map reverse
              tr U  = transpose
              tr D  = tr R . tr U
              itr L = tr L
              itr R = tr R
              itr U = tr U
              itr D = tr U . tr R

    final :: Board -> Bool
    final = notElem Mark . concat

    actions :: Board -> [Action]
    actions = const [L,R,U,D]
