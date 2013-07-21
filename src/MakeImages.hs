
module Main where

import Codec.Picture
import Codec.Picture.Types

import DataAccess
import Mosaic

main = loop 0 0 0

loop :: Int -> Int -> Int -> IO ()
loop r g b
   | r > 255   = return ()
   | g > 255   = do
      loop (r+20) 0 0
   | b > 255   = do
      loop r (g+20) 0
   | otherwise = do
      savePng r g b
      loop r g (b+20)

savePng r g b = mkPngTile 100
   (PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b))
   (fname r g b)


mkPngTile :: Int -> PixelRGB8 -> String -> IO ()
mkPngTile sz col name = do
   let img = generateImage (\_ _ -> col) sz sz
   let path = "tiles/" ++ name
   savePngImage path (ImageRGB8 img)
   id <- insert (ImageRow
                  path
                  path
                  (PhotoVal col)
                  "")
   return ()



