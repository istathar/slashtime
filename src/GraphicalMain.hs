{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Core.Program
import GraphicalWindow (program)

#ifdef __GHCIDE__
version :: Version
version = "0"
#else
version :: Version
version = $(fromPackage)
#endif

main :: IO ()
main = do
  context <-
    configure
      version
      None
      ( simple
          [ 
          ]
      )

  executeWith context program
