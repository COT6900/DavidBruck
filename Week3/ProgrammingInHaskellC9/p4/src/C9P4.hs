{-|
Module      : C9P4
Description : Programming in Haskell, chapter 9, problem 4
Maintainer  : David Bruck
-}
{-# LANGUAGE LambdaCase #-}
module C9P4
    ( lifeEditor
    ) where

import C9P3BookFunctions
    ( cls
    , writeat
    , height
    , wrap
    )
import C9P3
    ( flickerFreeLife
    )
import System.IO
    ( hFlush
    , stdout
    )
import System.IO.HiddenChar
    ( getHiddenChar
    )
import Control.Monad.State
    ( StateT
    , runStateT
    , gets
    , modify
    , liftIO
    )

cursorOverEmpty     = "°"
cursorOverFilled    = "Θ"
clearChar           = " "
filledChar          = "O"

data EditorData = EditorData {
    cursor          :: (Integer, Integer),
    alreadyFilled   :: Bool,
    board           :: [(Integer, Integer)]
}

startingState = EditorData (1, 1) False []

{-|
    Commandline interface editor for the "Game of Life".
    Allows starting the game with the custom board once done editing.
-}
lifeEditor = instructions >> do runStateT editorImpl startingState
                                return () where
    instructions = do
        cls
        writeat (1, height + 1) "\"Game of life\" editor by David Bruck"
        writeat (1, height + 2) $ "Arrows keys to move, space to toggle, " ++
                                  "enter to run"
    
    editorImpl :: StateT EditorData IO ()
    editorImpl = do
        alreadyFilled   <- gets alreadyFilled
        cursor          <- gets cursor

        liftIO $ writeat
                     cursor
                     (if alreadyFilled
                      then cursorOverFilled
                      else cursorOverEmpty)
        
        -- draw immediately instead of only after the console
        -- output buffer fills each time
        liftIO $ hFlush stdout

        {-
            Cross-platform wait for character via Richard Cook's
            'System.IO.HiddenChar.getHiddenChar'. Supports control keys.
            Unfortunately, arrow keys are 2-3 of these "hidden" characters.
        -}
        liftIO getHiddenChar >>= processKey

    processKey :: Char -> StateT EditorData IO ()
    processKey key
        {-
            Windows arrow keys are like "\0H"
            except Windows MSYS is "\224H";
            In 'nix it's "\ESC[A"
        -}
        | key
              `elem`
              ['\0', '\224', '\ESC']
                                = do -- ctrl character has 2-3 parts
                                     key2 <- liftIO getHiddenChar
                                     processArrow key2

        -- "Enter key" starts the game
        | key
              `elem`
              ['\r', '\n'  ]    = do board <- gets board
                                     liftIO $ flickerFreeLife board

        -- Ctrl+C to quit
        | key == '\ETX'         = liftIO $ return ()

        -- Space key changes whether position starts alive or dead
        | key == ' '            = do alreadyFilled  <- gets alreadyFilled
                                     board          <- gets board
                                     cursor         <- gets cursor

                                     modify $ \vs ->
                                         vs {alreadyFilled = not alreadyFilled,
                                             board =
                                             if alreadyFilled
                                             then (/= cursor) `filter` board
                                             else cursor : board}
                                     editorImpl -- loop

        -- Unknown key
        | otherwise             = editorImpl -- loop

    -- Arrow keys move the cursor position
    processArrow :: Char -> StateT EditorData IO ()
    processArrow key2 = do alreadyFilled                <- gets alreadyFilled
                           cursor@(cursorX, cursorY)    <- gets cursor

                           liftIO $ if not alreadyFilled
                                    then writeat cursor clearChar
                                    else pure ()
                           arrow <- liftIO $ getArrowKey key2
                           let newCursor = case arrow of
                                           '↑' -> wrap (cursorX, cursorY - 1)
                                           '↓' -> wrap (cursorX, cursorY + 1)
                                           '→' -> wrap (cursorX + 1, cursorY)
                                           '←' -> wrap (cursorX - 1, cursorY)
                                           _   -> cursor

                           if cursor /= newCursor
                           then do board <- gets board
                                   modify $ \vs ->
                                       vs {cursor = newCursor,
                                           alreadyFilled =
                                               newCursor `elem` board}
                           else liftIO $ writeat (1, height + 1) (show key2)
                                     
                           editorImpl -- loop

    getArrowKey :: Char -> IO Char
    getArrowKey = \case
        -- First are the Windows / MSYS keys:
        'H' -> return '↑'
        'P' -> return '↓'
        'M' -> return '→'
        'K' -> return '←'

        {-
            'Nix has a common 2nd character for control characters,
            so we keep reading for the last character...
        -}
        '[' -> getArrowKeyLinux <$> getHiddenChar
        _   -> return '\0'

    getArrowKeyLinux = \case
        'A' -> '↑'
        'B' -> '↓'
        'C' -> '→'
        'D' -> '←'
        _   -> '\0'
