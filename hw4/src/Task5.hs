{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Task5
  ( ToS (..)
  ) where

import Task4              ( HScript (..)
                          , Convertable (..)
                          )
import Data.Functor.Const ( Const
                          )
import Data.List          ( intercalate
                          )
import Data.List.Split    ( splitOn
                          )

newtype ToS a
  = ToS 
  { toString :: String
  }
  deriving (Show, Semigroup)

castTS :: ToS a -> ToS b
castTS (ToS ts) = ToS ts 

tab :: String
tab = "    "

addTabs :: ToS a -> ToS b
addTabs (ToS tos) = ToS 
  $ intercalate "\n"
  $ map 
      ( \l -> 
        if l == "" 
          then l 
          else tab ++ l
      )
  $ splitOn "\n" tos

instance HScript ToS where
  
  (%+%) a b = a <> ToS " + " <> b
  (%-%) a b = a <> ToS " - " <> b
  (%*%) a b = a <> ToS " * " <> b

  (===) a b = castTS a <> ToS " = "  <> castTS b <> ToS ";\n"
  (+=)  a b = castTS a <> ToS " += " <> castTS b <> ToS ";\n"
  (-=)  a b = castTS a <> ToS " -= " <> castTS b <> ToS ";\n"
  (*=)  a b = castTS a <> ToS " *= " <> castTS b <> ToS ";\n"

  (~=~) a b  = castTS a <> ToS " == "  <> castTS b
  (~/~) a b  = castTS a <> ToS " != "  <> castTS b
  (~<~) a b  = castTS a <> ToS " < "   <> castTS b
  (~>~) a b  = castTS a <> ToS " > "   <> castTS b
  (~<=~) a b = castTS a <> ToS " <= "  <> castTS b
  (~>=~) a b = castTS a <> ToS " >= "  <> castTS b
  (~==~) a b = castTS a <> ToS " === " <> castTS b
  (~/=~) a b = castTS a <> ToS " !== " <> castTS b
  (~&&~) a b = castTS a <> ToS " && "  <> castTS b
  (~||~) a b = castTS a <> ToS " || "  <> castTS b

  while condition toEval = ToS "while ("
    <> castTS condition 
    <> ToS ") {\n"
    <> addTabs toEval 
    <> ToS "}\n"

  iff condition trueEval falseEval = ToS "if ("
    <> castTS condition 
    <> ToS ") {\n"
    <> addTabs trueEval 
    <> ToS "} else {\n"
    <> addTabs falseEval 
    <> ToS "}\n"

  oneArgFunc toEval _ = ToS "function (arg) {\n"
    <> addTabs (ToS "var returnValue = 0;\n")
    <> (castTS $ addTabs $ toEval (ToS "arg") $ ToS "returnValue")
    <> ToS "}\n"

  twoArgFunc toEval _ _ = ToS "function (arg1, arg2) {\n"
    <> addTabs (ToS "var returnValue = 0;\n")
    <> (castTS $ addTabs $ toEval (ToS "arg1") (ToS "arg2") $ ToS "returnValue")
    <> ToS "}\n"

  (\\) previous next = castTS previous <> next

  (##) newVar next = 
    let varName = "v" ++ show newVar
    in (ToS $ "var " ++ varName ++ " = ")
    <> (ToS $ show $ convert newVar)
    <> ToS ";\n"
    <> (castTS $ next $ ToS varName)

  ($$) inputVar = castTS inputVar

  (!!) v = ToS $ show $ convert v

  type Saver ToS t = Const String t

