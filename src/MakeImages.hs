
module Main where

import Codec.Picture
import Codec.Picture.Types
import System.Directory
import System.FilePath
import qualified Control.Monad.Parallel as Par

import DataAccess
import Mosaic

main = addAllMosaics

commaSeparated :: [String] -> String
commaSeparated []       = []
commaSeparated [x]      = x
commaSeparated (x:xs)   = x ++ "," ++ (commaSeparated xs)

-- creates solod color pngs
{-
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

-}

importHandler = do
   fileNames <- getDirectoryContents "conv/"

   mapM_ insertFileWOutMosaic fileNames

   putStrLn ("Imported " ++ ((show . length) fileNames) ++ "images")

insertFileWOutMosaic :: FilePath -> IO ()
insertFileWOutMosaic string = do
   eDyn <- readImage ("conv/" ++ string)

   case eDyn of
      Left _      -> return ()
      Right dyn   -> do
         let rgb8 = rgb8Image dyn
             fullP = ("images/" ++ takeFileName string)
             tileP = ("tiles/" ++ takeFileName string)
             (sqr, pv) = prepareForDB (ImageRGB8 rgb8)

         writePng fullP rgb8
         writePng tileP sqr

         id <- insert (ImageRow fullP tileP pv "")
         return ()


addAllMosaics = do
   a <- allRows "Images"

   --new <- Par.mapM addMosaic >>= a

   mapM_ (\r -> (addMosaic r) >>= insertReplace) a

   --mapM_ insertReplace new

   return ()


addMosaic :: (Int, ImageRow) -> IO (Int, ImageRow)
addMosaic (i, imgRow) = do

   eDyn <- readImage (fullImage imgRow)

   putStrLn $ show i

   case eDyn of
      Left _  -> return (i, imgRow)
      Right d -> do
         let !rgb8 = rgb8Image d
             !pvs = concat $ scaleAvgPhotoVal rgb8 50

         -- rewrite image to drive
         dbEntries <- mapM getClosestColor pvs
         let !fpaths  = commaSeparated $ map tileImage dbEntries

         return (i, (ImageRow (fullImage imgRow) (tileImage imgRow)
            (photoVal imgRow) fpaths))



