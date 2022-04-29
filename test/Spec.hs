{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Metai.Token
import Metai.Syllable
import Metai.Hexameter
import Metai.Parse

tokenization :: TestTree
tokenization =
  let
    line = normalize "Iau Sauléle wėl atkópdămă buddı̆nŏ Swieta"
  in
  testGroup "Tokenization"
  [ testCase "foo" $ 1 @=?  2 - 1
  ]

syllabification :: TestTree
syllabification =
  let
    syllabify' = map (renderTokens . segments) . syllabify . tokenize . normalize
  in
  testGroup "Syllabification"
  [ testCase "extrasyllabics" $ ["žvaigž", "dems"] @=? syllabify' "żwaigżdėms"
  , testCase "simple" $ ["sau", "le", "le"] @=? syllabify' "Sauléle"
  , testCase "geminate" $ ["bud", "di", "no"] @=? syllabify' "buddı̆nŏ"
  , testCase "diphthong" $ ["džau", "ges"] @=? syllabify' "dʒ̇augės"
  , testCase "ignore morphology" $ ["i", "šim", "ti"] @=? syllabify' "iſʒimtı̆"
  ]

scansion :: TestTree
scansion = testGroup "Scansion"
  [ testCase "bar" $ 1 @=? 2 / 2
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests" [tokenization, syllabification, scansion]
