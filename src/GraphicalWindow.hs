{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_ghc -fno-warn-unused-do-bind #-}
{-# OPTIONS_ghc -fno-warn-unused-imports #-}

module GraphicalWindow where

import Core.Program
import Core.Text
import Data.GI.Base
import qualified GI.Gtk as Gtk

program :: Program None ()
program = do
  Gtk.init Nothing

  window <- new Gtk.Window [#title := "Hi there"]

  on window #destroy Gtk.mainQuit

  button <- new Gtk.Button [#label := "Click me"]

  on
    button
    #clicked
    ( set
        button
        [ #sensitive := False,
          #label := "Thanks for clicking me"
        ]
    )

  #add window button

  #showAll window

  Gtk.main
