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

  (window,top) <- setupWindow
  view <- setupTreeView top
  -- setupContextMenu

  -- hookupWindowManagement
  selection <- hookupSelectionSignals view
  -- hookupReactingToWindowVisibilityChanges

  -- populateZonesIntoModel

  -- createClockThread
  initialPresentation window selection

  Gtk.main

-- Initialize and pack outer Widgets; prepare Window properties.
setupWindow :: Program None (Gtk.Window,Gtk.Box)
setupWindow = do
  window <-
    new
      Gtk.Window
      [ #title := "slashtime",
        #decorated := False,
        #borderWidth := 1,
        #hasResizeGrip := False
      ]

  top <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical,
        #homogeneous := False,
        #spacing := 0
      ]

  #add window top
  return (window,top)

setupTreeView :: Gtk.Box -> Program None Gtk.TreeView
setupTreeView top = do
  return undefined

{-

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

hookupWindowManagement :: Gtk.Window -> Program None ()
hookupWindowManagement window = do
  on window #destroy Gtk.mainQuit
  return ()

hookupSelectionSignals :: Gtk.TreeView -> Program None Gtk.TreeSelection
hookupSelectionSignals view = do
  selection <- #getSelection view
  return selection

initialPresentation :: Gtk.Window -> Gtk.TreeSelection -> Program None ()
initialPresentation window selection = do
  #showAll window
  #unselectAll selection
