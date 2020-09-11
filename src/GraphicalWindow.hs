{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module GraphicalWindow where

import Core.Program (None, Program)
import Data.GI.Base
import Data.GI.Base.GType
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

data ZonesWindow = ZonesWindow
  { topLevelWindow :: Gtk.Window
  }

program :: Program None ()
program = do
  Gtk.init Nothing

  (window, top) <- setupWindow
  view <- setupTreeView top
  -- setupContextMenu

  selection <- hookupSelectionSignals view
  hookupWindowManagement window selection

  -- hookupReactingToWindowVisibilityChanges

  -- populateZonesIntoModel

  -- createClockThread
  initialPresentation window selection

  Gtk.main

-- Initialize and pack outer Widgets; prepare Window properties.
setupWindow :: Program None (Gtk.Window, Gtk.Box)
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
  return (window, top)

setupTreeView :: Gtk.Box -> Program None Gtk.TreeView
setupTreeView top = do
  model <-
    Gtk.listStoreNew
      [ -- 0 icon image
        gtypeObject,
        -- 1 place markup
        gtypeString,
        -- 2 time markup
        gtypeString,
        -- 3 time sort
        gtypeInt,
        -- 4 offset markup
        gtypeString,
        -- 5 row colour
        gtypeString,
        -- 6 row background
        gtypeString
      ]

  -- sorted <- Gtk.treeModelSortNewWithModel model
  sorted <- new Gtk.TreeModelSort [#model := model]
  #setSortColumnId sorted 3 Gtk.SortTypeAscending

  view <- new Gtk.TreeView [#model := sorted]
  #setRulesHint view False
  #setHeadersVisible view False
  #setEnableSearch view False

  -- Unusually, we can pack all the CellRenderers into one TreeViewColumn as
  -- they reserve a constant width per actual row. This has the nice side
  -- effect of eliminating the one pixel boundary between the former
  -- TreeViewColumns whose headings we weren't looking at anyway.

  vertical <- new Gtk.TreeViewColumn []
  #appendColumn view vertical

  image <-
    new
      Gtk.CellRendererPixbuf
      []

  #addAttribute vertical image "pixbuf" 0
  #addAttribute vertical image "cell-background" 6

  place <-
    new
      Gtk.CellRendererText
      [ #xalign := 0.0,
        #yalign := 0.0
      ]

  #addAttribute vertical place "markup" 1
  #addAttribute vertical place "foreground" 5
  #addAttribute vertical place "cell-background" 6

  datetime <-
    new
      Gtk.CellRendererText
      [ #xalign := 0.5,
        #yalign := 0.0
      ]
  #addAttribute vertical datetime "markup" 2
  #addAttribute vertical datetime "foreground" 5
  #addAttribute vertical datetime "cell-background" 6

  offset <-
    new
      Gtk.CellRendererText
      [ #xalign := 0.5,
        #yalign := 0.0
      ]
  #addAttribute vertical offset "markup" 4
  #addAttribute vertical offset "foreground" 5
  #addAttribute vertical offset "cell-background" 6

  #packStart top view True True 0

  return view

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

--
-- When focus leaves the ZonesWindow, deselect so that it's not left
-- with a blue selected row for no terribly useful reason.
--
hookupWindowManagement :: Gtk.Window -> Gtk.TreeSelection -> Program None ()
hookupWindowManagement window selection = do
  on
    window
    #destroy
    Gtk.mainQuit

  on
    window
    #leaveNotifyEvent
    ( \eventCrossing -> do
        mode <- get eventCrossing #mode
        case mode of
          Gdk.CrossingModeGrab -> do
            #unselectAll selection
            return False
          _ -> do
            return False
    )
  return ()

hookupSelectionSignals :: Gtk.TreeView -> Program None Gtk.TreeSelection
hookupSelectionSignals view = do
  selection <- #getSelection view
  return selection

initialPresentation :: Gtk.Window -> Gtk.TreeSelection -> Program None ()
initialPresentation window selection = do
  -- It's necessary to update the time fields here as a preload to ensure the
  -- TreeView is properly sized and that all columns are showing.

  updateNow window
  indicateCorrectTime window

  -- unselect has to be after map, hence the showAll() first.
  #showAll window
  #unselectAll selection

updateNow :: Gtk.Window -> Program None ()
updateNow window = undefined

indicateCorrectTime :: Gtk.Window -> Program None ()
indicateCorrectTime window = do
  #overrideBackgroundColor window [Gtk.StateFlagsNormal] Nothing

indicateWrongTime :: Gtk.Window -> Program None ()
indicateWrongTime window = do
  color <- new Gdk.RGBA [#red := 1.0, #green := 0.0, #blue := 0.0]
  #overrideBackgroundColor window [Gtk.StateFlagsNormal] (Just color)
