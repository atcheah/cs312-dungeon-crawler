import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Welcome!", containerBorderWidth := 10]
  label <- labelNew (Just "Press Enter to continue")
  containerAdd window label
  widgetShowAll window
  label `on` keyPressEvent $ tryEvent $ do
    keyval <- eventKeyName
    case keyval of
      "Return" -> labelSetText label "Level 1"
      _ -> return ()
  mainGUI