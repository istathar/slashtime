{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module GraphicalWindow where

import Core.Program
import Core.Text
import Data.GI.Base
import qualified GI.Gtk as Gtk

data ZonesWindow = ZonesWindow
  { topLevelWindow :: Gtk.Window
  }

program :: Program None ()
program = do
  Gtk.init Nothing

  window <- setupWindow
  view <- setupTreeView
  -- setupContextMenu

  -- hookupWindowManagement
  selection <- hookupSelectionSignals view
  -- hookupReactingToWindowVisibilityChanges

  -- populateZonesIntoModel

  -- createClockThread
  initialPresentation window selection

  Gtk.main

setupWindow :: Program None Gtk.Window
setupWindow = do
  window <-
    new
      Gtk.Window
      [ #title := "slashtime",
        #decorated := False,
        #borderWidth := 1,
        #hasResizeGrip := False
      ]

  return window

{-
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
-}

hookupSelectionSignals :: Gtk.TreeView -> Program None Gtk.TreeSelection
hookupSelectionSignals view = do
  selection <- #getSelection view
  return selection

initialPresentation :: Gtk.Window -> Gtk.TreeSelection -> Program None ()
initialPresentation window selection = do
  #showAll window
  #unselectAll selection
