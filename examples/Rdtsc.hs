{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}
{-# LANGUAGE LambdaCase, BlockArguments, NumericUnderscores #-}

module Main where

import Control.Monad
import Data.List
import Data.String.Interpolate
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GHC.Word
import System.Environment

import Language.Asm.Inline
import Language.Asm.Inline.QQ

defineAsmFunM "rdtsc"
  [asmTy| | (out : Word64) |]
  [asm|
  rdtsc
  mov %rdx, {out}
  shl $32, {out}
  add %rax, {out}
  |]

defineAsmFunM "rdtscP"
  [asmTy| | (hi : Word32) (lo : Word32) |]
  [asm|
  rdtsc
  mov %rdx, {hi}
  mov %rax, {lo}
  |]

defineAsmFunM "rdtsc2"
  [asmTy| | (out1 : Word64) (out2 : Word64) |]
  [asm|
  rdtsc
  mov %rdx, {out1}
  shl $32, {out1}
  add %rax, {out1}

  rdtsc
  mov %rdx, {out2}
  shl $32, {out2}
  add %rax, {out2}
  |]

example :: IO ()
example = do
  v1 <- rdtsc
  v2 <- rdtsc
  print v1
  print v2
  print $ v2 - v1

  p1 <- rdtscP
  p2 <- rdtscP
  print p1
  print p2

  (o1, o2) <- rdtsc2
  print $ o2 - o1

main :: IO ()
main = getArgs >>= \case [] -> example
                         ["bench"] -> bench
                         _ -> putStrLn "Unknown args"

---- Benchmarking stuff
bench :: IO ()
bench = do
  baselineMeas <- removeOutliers <$> replicateM count do
    (v1, v2) <- rdtsc2
    pure $ v2 - v1
  asmMeas <- removeOutliers <$> replicateM count do
    v1 <- rdtsc
    v2 <- rdtsc
    pure $ v2 - v1
  cMeas <- removeOutliers <$> replicateM count do
    v1 <- rdtscC
    v2 <- rdtscC
    pure $ v2 - v1

  printPlot [("only asm", baselineMeas), ("inline-asm", asmMeas), ("c", cMeas)]
  where
    count = 1_000_000 :: Int

foreign import ccall unsafe "rdtscC"
  rdtscC :: IO Word64

printPlot :: [(String, [Word64])] -> IO ()
printPlot allStats = do
  forM_ allStats $ uncurry printStats
  toFile def "out.svg" $ do
    layout_title .= "rdtsc diff time"
    mapM_ (plot . uncurry histPlot) allStats
  where
    minVal = fromIntegral $ minimum $ concat $ snd <$> allStats
    maxVal = fromIntegral $ maximum $ concat $ snd <$> allStats
    histPlot :: String -> [Word64] -> EC l (Plot Double Int)
    histPlot name vals = do
      color <- takeColor
      pure $ histToPlot $ plot_hist_title .~ name
                        $ plot_hist_values .~ (fromIntegral <$> vals)
                        $ plot_hist_fill_style.fill_color .~ dissolve 0.2 color
                        $ plot_hist_line_style.line_color .~ color
                        $ plot_hist_range ?~ (minVal, maxVal)
                        $ defaultPlotHist

printStats :: String -> [Word64] -> IO ()
printStats name allRuns = putStrLn [i|#{name}:\n    min: #{minimum runs}; max: #{maximum runs}; avg: #{avg}|]
  where
    runs = removeOutliers allRuns
    avg = sum runs `div` genericLength runs

removeOutliers :: [Word64] -> [Word64]
removeOutliers allRuns = drop cutoff $ take (runsLen - cutoff) $ sort allRuns
  where
    runsLen = genericLength allRuns
    cutoff = runsLen `div` 10000
