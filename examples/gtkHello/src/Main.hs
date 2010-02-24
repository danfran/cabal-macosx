import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = do set b [buttonLabel := "Hello World" ]
             putStrLn "Hello World"

main :: IO ()
main = do initGUI
          window <- windowNew
          button <- buttonNew
          set window [windowDefaultWidth := 200
                     ,windowDefaultHeight := 200
                     ,containerBorderWidth := 10
                     ,containerChild := button]
          onClicked button (hello button)
          onDestroy window mainQuit
          widgetShowAll window
          mainGUI
