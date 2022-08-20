module Lib.Matrix.Main where

import Prelude
import Control.MonadPlus as Control.MonadPlus
import Math as Math
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Ord as Ord
import Data.Number as Number
import Data.Foldable as Foldable
import Data.Tuple as Tuple
import Data.Tuple.Nested as Tuple.Nested
import Lib.Array.Main as Lib.Array

type Matrix = Array (Array Number)

from :: (Int -> Int -> Number) -> Int -> Matrix
from fn n = map (\x -> map (fn x) range) range
  where range = Array.range 0 (n - 1)

fromN :: Int -> Matrix
fromN n = from step n
  where step x y | x == y    = 1.0
                 | otherwise = 0.0

multiply :: Matrix -> Matrix -> Matrix
multiply xss yss =
  ( Lib.Array.transpose yss #
    \yss ->
      ( xss #
        map \xs ->
          ( yss #
            map \ys ->
              ( Array.zipWith (*) xs ys #
                Foldable.sum
              )
          )
      )
  )

invert :: Matrix -> Matrix
invert xss = fromTriangle $ toTriangle xss (fromN $ Array.length xss)

fromTriangle :: Tuple.Nested.Tuple2 Matrix Matrix -> Matrix
fromTriangle txs =
  let ass  = Tuple.Nested.get1 txs
      bss  = Tuple.Nested.get2 txs
      as   = Maybe.maybe [] identity (Array.head ass)
      at   = Maybe.maybe [] identity (Array.tail ass)
      a    = Maybe.maybe 0.0 identity (Array.head as)
      bs   = Maybe.maybe [] identity (Array.head bss)
      bt   = Maybe.maybe [] identity (Array.tail bss)
      iter xss yss zss = Maybe.maybe zss identity $ do
        let xs = Maybe.maybe [] identity (Array.head xss)
            xt = Maybe.maybe [] identity (Array.tail xss)
            x  = Maybe.maybe 0.0 identity (Array.head xs)
            xu = Maybe.maybe [] identity (Array.tail xs)
            ys = Maybe.maybe [] identity (Array.head yss)
            yt = Maybe.maybe [] identity (Array.tail yss)
            zs = Maybe.maybe [] identity (Array.head $ multiply [xu] zss)
        Control.MonadPlus.guard (not $ Foldable.null xss)
        Control.MonadPlus.guard (not $ Foldable.null yss)
        pure $ iter xt yt $ Array.cons (map (\z -> z / x) $ Array.zipWith (-) ys zs) zss
        in iter at bt [(map (\b -> b / a) bs)]

toTriangle :: Matrix -> Matrix -> Tuple.Nested.Tuple2 Matrix Matrix
toTriangle xss yss =
  let iter tss zss = Maybe.maybe tss identity $ do
        Control.MonadPlus.guard (not $ Foldable.null $ Tuple.Nested.get1 zss)
        Control.MonadPlus.guard (not $ Foldable.null $ Tuple.Nested.get2 zss)
        let txs = toBubble (Tuple.Nested.get1 zss) (Tuple.Nested.get2 zss)
            uss = Tuple.Nested.get1 txs
            css = Tuple.Nested.get2 txs
            oss = Tuple.Nested.get1 tss
            pss = Tuple.Nested.get2 tss
            us  = Maybe.maybe [] identity (Array.head uss)
            u   = Maybe.maybe 0.0 identity (Array.head us)
            ut  = Maybe.maybe [] identity (Array.tail uss)
            tus = Maybe.maybe [] identity (Array.tail us)
            cs  = Maybe.maybe [] identity (Array.head css)
            ct = Maybe.maybe [] identity (Array.tail css)
        pure $ iter
          (Tuple.Nested.tuple2 (Array.cons us oss) (Array.cons cs pss))
          ( Array.zip ut ct #
            map
              (\uxs ->
                  let vs = Tuple.fst uxs
                      es = Tuple.snd uxs
                      v  = Maybe.maybe 0.0 identity (Array.head vs)
                      vt = Maybe.maybe [] identity (Array.tail vs)
                      fn = Array.zipWith (\x y -> v * x - u * y)
                  in Tuple.Tuple (fn tus vt) (fn cs es)
              ) #
            Array.unzip #
            \tx -> Tuple.Nested.tuple2 (Tuple.fst tx) (Tuple.snd tx)
          )
  in iter (Tuple.Nested.tuple2 [] []) (Tuple.Nested.tuple2 xss yss)

toBubble :: Matrix -> Matrix -> Tuple.Nested.Tuple2 Matrix Matrix
toBubble xss yss =
  let idmax =
        ( xss #
          map (Maybe.maybe 0.0 identity <<< map Math.abs <<< Array.head) #
          Array.zip (Array.range 0 $ Array.length xss) #
          Foldable.maximumBy (Ord.comparing Tuple.snd) #
          Maybe.maybe 0 Tuple.fst
        )
      go ys =
        let tx = Array.splitAt idmax ys
        in tx.after <> tx.before
  in Tuple.Nested.tuple2 (go xss) (go yss)
