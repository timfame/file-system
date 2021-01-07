{-# LANGUAGE RankNTypes #-}

module Task7
  ( cd
  , ls
  , file
  ) where

import Task6      ( FS (..)
                  , _File
                  , _Dir
                  , name
                  , contents
                  )
import Lens.Micro ( Traversal'
                  , traversed
                  , filtered
                  , (^.)
                  )

cdFilter :: FilePath -> Traversal' FS FS
cdFilter n = _Dir.filtered (\d -> d^.name == n)

cd :: FilePath -> Traversal' FS FS
cd n = contents.traversed.(cdFilter n)

ls :: Traversal' FS FilePath
ls = contents.traversed.name

fileFilter :: FilePath -> Traversal' FS FS
fileFilter n = _File.filtered (\f -> f^.name == n)

file :: FilePath -> Traversal' FS FilePath
file n = contents.traversed.(fileFilter n).name