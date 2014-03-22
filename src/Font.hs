module Font (fontData, padr) where

import Data.Word
import Data.Sized.Matrix
import Data.Char (isSpace)

-- From http://laurencescotford.co.uk/?p=440
fontData :: Matrix X16 (Matrix X5 Word8)
fontData = matrix . map (matrix . map lineToByte) $
           [ -- 0
             [ "****"
             , "*  *"
             , "*  *"
             , "*  *"
             , "****"
             ]
             -- 1
           , [ "  * "
             , " ** "
             , "  * "
             , "  * "
             , " ***"
             ]
             -- 2
           , [ "****"
             , "   *"
             , "****"
             , "*   "
             , "****"
             ]
             -- 3
           , [ "****"
             , "   *"
             , "****"
             , "   *"
             , "****"
             ]
             -- 4
           , [ "*  *"
             , "*  *"
             , "****"
             , "   *"
             , "   *"
             ]
             -- 5
           , [ "****"
             , "*   "
             , "****"
             , "   *"
             , "****"
             ]
             -- 6
           , [ "****"
             , "*   "
             , "****"
             , "*  *"
             , "****"
             ]
             -- 7
           , [ "****"
             , "   *"
             , "  * "
             , " *  "
             , " *  "
             ]
             -- 8
           , [ "****"
             , "*  *"
             , "****"
             , "*  *"
             , "****"
             ]
             -- 9
           , [ "****"
             , "*  *"
             , "****"
             , "   *"
             , "****"
             ]
             -- a
           , [ "****"
             , "*  *"
             , "****"
             , "*  *"
             , "*  *"
             ]
             -- b
           , [ "*** "
             , "*  *"
             , "*** "
             , "*  *"
             , "*** "
             ]
             -- c
           , [ "****"
             , "*   "
             , "*   "
             , "*   "
             , "****"
             ]
             -- d
           , [ "*** "
             , "*  *"
             , "*  *"
             , "*  *"
             , "*** "
             ]
             -- e
           , [ "****"
             , "*   "
             , "****"
             , "*   "
             , "****"
             ]
             -- f
           , [ "****"
             , "*   "
             , "****"
             , "*   "
             , "*   "
             ]
           ]

lineToByte :: String -> Word8
lineToByte = foldl push 0 . padr 8 ' '
  where
    push :: Word8 -> Char -> Word8
    push x c = if isSpace c then x' else x' + 1
      where
        x' = x * 2

padr :: Int -> a -> [a] -> [a]
padr 0 _ xs     = xs
padr k y []     = replicate k y
padr k y (x:xs) = x:padr (k-1) y xs
