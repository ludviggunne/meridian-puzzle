{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game (Game,transition,final,actions,gameMain) where

import System.IO
import System.Exit
import Text.Printf
import Control.Monad
import GHC.Environment
import Control.Concurrent
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Sequence (Seq(..))

class (Eq s, Ord s, Read s, Show s, Read a, Show a) => Game s a | s -> a where
    transition :: a -> s -> s
    final      :: s -> Bool
    actions    :: s -> [a]

solve :: forall s a. Game s a => s -> Maybe [a]
solve s = reverse <$> go mempty (Seq.singleton (s,[]))
    where go :: Set s -> Seq (s,[a]) -> Maybe [a]
          go _ Seq.Empty = Nothing
          go v ((s,p):<|t)
            | final s    = Just p
            | s `Set.member` v = go v t
            | otherwise  = go v' t'
                where v'  = Set.singleton s <> v
                      t'  = t <> Seq.fromList (map f (actions s))
                      f a = (transition a s,a:p)

solveMain :: forall s a. Game s a => s -> IO ()
solveMain state = case solve state of
    Nothing -> do hPutStrLn stderr "Impossible"
                  exitFailure
    Just actions -> mapM_ print actions

playMain :: forall s a. Game s a => s -> IO ()
playMain state = do hSetBuffering stdout NoBuffering
                    hSetBuffering stdin NoBuffering
                    hSetEcho stdin False
                    input <- getContents
                    draw state
                    play state input
    where play :: s -> String -> IO ()
          play _ ('q':_) = exitSuccess
          play _ []      = exitSuccess
          play state input =
              do let (state',input') = case reads @a input of
                                [] -> (state,tail input)
                                [(action,rest)] -> (transition @s @a action state,rest)
                 draw state'
                 when (final state') $
                     do putStrLn "You won!"
                        exitSuccess
                 play state' input'

animMain :: forall s a. Game s a => s -> IO ()
animMain state = case solve state of
        Nothing -> do hPutStrLn stderr "Impossible"
                      exitFailure
        Just actions -> anim state actions
    where anim :: s -> [a] -> IO ()
          anim s [] = draw s >> exitSuccess
          anim s (a:tl) = do draw s
                             threadDelay 1000000
                             anim (transition a s) tl

draw :: Game s a => s -> IO ()
draw s = do putStr "\x1b[2J\x1b[H"
            print s

usage :: String -> IO ()
usage = hPrintf stderr "Usage: %s play|solve|anim FILE\n"

data MainAction = Solve|Play|Anim

gameMain :: forall s a. Game s a => IO ()
gameMain = do
    args <- getFullArgs

    (action,path) <- case args of
        (_:"play":path:_) -> pure (Play,path)
        (_:"solve":path:_) -> pure (Solve,path)
        (_:"anim":path:_) -> pure (Anim,path)
        (name:_)  -> usage name >> exitFailure

    handle <- openFile path ReadMode
    content <- hGetContents handle
    let state = read @s $ strip content
        strip = unlines . filter keep . lines
        keep ('#':_)   = False
        keep (' ':tl)  = keep tl
        keep ('\t':tl) = keep tl
        keep ""        = False
        keep _         = True

    case action of
        Play  -> playMain state
        Solve -> solveMain state
        Anim -> animMain state
