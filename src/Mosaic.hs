
module Mosaic where

import           System.Directory
import           System.FilePath
import           Control.Monad

import           Data.Word
import           Codec.Picture
import           Codec.Picture.Bitmap
import           Codec.Picture.Gif
import           Codec.Picture.HDR
import           Codec.Picture.Jpg
import           Codec.Picture.Png
import           Codec.Picture.Saving
import           Codec.Picture.Tiff
import           Codec.Picture.Types
import qualified Data.Vector.Storable as V

type RCord = (Rational, Rational)

data PhotoVal = PhotoVal PixelRGB8 deriving (Show)

-- Remove later
right (Left _) = error " not right"
right (Right a) = a

dynMap :: (forall pixel . (Pixel pixel) => Image pixel -> Image pixel)
           -> DynamicImage -> DynamicImage
dynMap f (ImageY8    i)  = ImageY8 (f i)
dynMap f (ImageY16   i)  = ImageY16 (f i)
dynMap f (ImageYF    i)  = ImageYF (f i)
dynMap f (ImageYA8   i)  = ImageYA8 (f i)
dynMap f (ImageYA16  i)  = ImageYA16 (f i)
dynMap f (ImageRGB8  i)  = ImageRGB8 (f i)
dynMap f (ImageRGB16 i)  = ImageRGB16 (f i)
dynMap f (ImageRGBF  i)  = ImageRGBF (f i)
dynMap f (ImageRGBA8 i)  = ImageRGBA8 (f i)
dynMap f (ImageRGBA16 i) = ImageRGBA16 (f i)
dynMap f (ImageYCbCr8 i) = ImageYCbCr8 (f i)
dynMap f (ImageCMYK8 i)  = ImageCMYK8 (f i)
dynMap f (ImageCMYK16 i) = ImageCMYK16 (f i)

dynWidth :: DynamicImage -> Int
dynWidth = dynamicMap imageWidth

dynHeight :: DynamicImage -> Int
dynHeight = dynamicMap imageHeight

dynSquare :: DynamicImage -> DynamicImage
dynSquare = dynMap squareImage

squareImage :: Pixel a => Image a -> Image a
squareImage img = generateImage (\x y -> pixelAt img x y) edge edge
   where edge = min (imageWidth img) (imageHeight img)

thumbNailWidth = 400

prepareForDB :: DynamicImage -> (Image PixelRGB8, PhotoVal)
prepareForDB dyn = (imageRGB8, PhotoVal (meanInRectRGB imageRGB8 (0,0) (sz,sz)))
   where imageRGB8 = squareImage (rgb8Image dyn)
         sz = fromIntegral (imageWidth imageRGB8)

calQual :: Int -> Int
calQual imgWidth
   | imgWidth <= thumbNailWidth  = 100
   | otherwise                   = round $ (fromIntegral thumbNailWidth)/
         (fromIntegral imgWidth)

eitherSaveImage (Left _) _       = return ()
eitherSaveImage (Right img) name = do
   putStrLn $ "Saving " ++ name
   savePngImage name img

squareFor :: FilePath -> IO ()
squareFor dir = do
   fileNames <- getDirectoryContents dir
   createDirectoryIfMissing True "squares"

   mapM_ putStrLn fileNames

   photos <- mapM (\f -> readImage (dir++f)) fileNames
   mapM_ (\(img, name) -> eitherSaveImage (liftM dynSquare img) name)
      (zipWith (\a b -> (a,"squares/" ++ b)) photos fileNames)

charDims img charsWide = (xdim, xdim)
   where w = (fromIntegral . imageWidth) img :: Rational
         h = (fromIntegral . imageHeight) img :: Rational
         xdim = w / (fromIntegral charsWide)

bound n = (max 0 n) `min` 240

nearest20 :: Word8 -> Word8
nearest20 n
   | from20 > 10     = (fromIntegral . bound) ((20 - from20) + n')
   | otherwise       = (fromIntegral . bound) (n' - from20)
      where from20 = n' `rem` 20
            n' = fromIntegral n :: Int

nearestTile :: PixelRGB8 -> FilePath
nearestTile (PixelRGB8 r g b) = "tiles/" ++
   (fname (nearest20 r) (nearest20 g) (nearest20 b))

fname r g b = (show3Dig r) ++ (show3Dig g) ++ (show3Dig b) ++ ".png"

show3Dig i
   | length sh >  3  = (reverse . (take 3) . reverse) sh
   | length sh == 3  = sh
   | length sh < 3   = (reverse . (take 3)) $ (reverse sh) ++ (repeat '0')
      where sh = show i

scaleAvgPixs :: Image PixelRGB8 -> Int -> [[PixelRGB8]]
scaleAvgPixs img width = avgPixs
   where avgPixs = map row [0..(chsHigh)]
         dim = charDims img width
         chsHigh = floor ((fromIntegral (imageHeight img))/(snd dim))
         row c = map (\i -> meanInRectRGB img
               ((fromIntegral i)*(fst dim),(fromIntegral c)*(snd dim)) dim )
               [0..(width-1)]

scaleMosic :: Image PixelRGB8 -> Int -> [[FilePath]]
scaleMosic img width = (map . map) nearestTile avgPixs
   where avgPixs = map row [0..(chsHigh)]
         dim = charDims img width
         chsHigh = floor ((fromIntegral (imageHeight img))/(snd dim))
         row c = map (\i -> meanInRectRGB img
               ((fromIntegral i)*(fst dim),(fromIntegral c)*(snd dim)) dim )
               [0..(width-1)]

imageType :: DynamicImage -> String
imageType dyImage = case dyImage of
   ImageY8     img      -> "Y8"
   ImageY16    img      -> "Y16"
   ImageYF     img      -> "F" 
   ImageYA8    img      -> "not"
   ImageYA16   img      -> "not"
   ImageRGB8   img      -> "yes"
   ImageRGB16  img      -> "not"
   ImageRGBF   img      -> "not"
   ImageRGBA8  img      -> "not"
   ImageRGBA16 img      -> "not"
   ImageYCbCr8 img      -> "not"
   ImageCMYK8  img      -> "not"
   ImageCMYK16 img      -> "not"

-- Convert a dynamic image to the rgb8 version of it
rgb8Image :: DynamicImage -> Image PixelRGB8
rgb8Image dyImage = case dyImage of
   ImageY8     img      -> promoteImage img
   ImageY16    img      -> pixelMap (\p -> promotePixel (fromIntegral (p`div`2) :: Pixel8)) img
   ImageYF     img      -> pixelMap (\p -> promotePixel (round (p*255) :: Pixel8)) img
   ImageYA8    img      -> pixelMap (promotePixel . dropTransparency) img
   ImageYA16   img      -> pixelMap ((\p -> promotePixel (fromIntegral (p`div`2) :: Pixel8))
                              . dropTransparency) img
   ImageRGB8   img      -> img
   ImageRGB16  img      -> pixelMap (\(PixelRGB16 r g b) -> PixelRGB8
                                       (fromIntegral (r`div`2))
                                       (fromIntegral (r`div`2))
                                       (fromIntegral (r`div`2))) img
   ImageRGBF   img      -> pixelMap (\(PixelRGBF r g b) -> PixelRGB8
                                       (round (r*255))
                                       (round (g*255))
                                       (round (b*255))) img 

   ImageRGBA8  img      -> pixelMap dropTransparency img
   ImageRGBA16 img      -> rgb8Image (ImageRGB16 (pixelMap dropTransparency img))
   ImageYCbCr8 img      -> convertImage img 
   ImageCMYK8  img      -> convertImage img
   ImageCMYK16 img      -> rgb8Image (ImageRGB16 (convertImage img))

weightedMean :: [(PixelRGB8, Rational)] -> PixelRGB8
weightedMean xs = if den /= 0 then toPix (multAll num (1/den)) else PixelRGB8 0 0 0 
   where num = foldl (\acc (pix,a) -> (multAll (toTulpe pix) a) `addAll` acc) (0,0,0) xs
         den = foldl (\acc (_,b) -> acc + b) 0 xs

toTulpe :: PixelRGB8 -> (Rational,Rational,Rational)
toTulpe (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

toPix :: (Rational, Rational, Rational) -> PixelRGB8
toPix (r,g,b) = PixelRGB8 (fromIntegral r') (fromIntegral g') (fromIntegral b')
   where r' = (max 0 (round r)) `min` 255 :: Int
         g' = (max 0 (round g)) `min` 255 :: Int
         b' = (max 0 (round b)) `min` 255 :: Int

multAll :: Num a => (a,a,a) -> a -> (a,a,a)
multAll (a,b,c) p = (a*p,b*p,c*p)

addAll :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
addAll (a,b,c) (d,e,f) = (a+d,b+e,c+f)

meanInRectRGB :: (Image PixelRGB8) -> RCord -> RCord -> PixelRGB8
meanInRectRGB img (x,y) (xd,yd) = weightedMean valAndWgt
   where maxX = (imageWidth img) - 1
         maxY = (imageHeight img) - 1
         inds = [ (i,j) | i <- xinds, j <- yinds]
         xinds = [(floor x)..(min (floor (x+xd)) maxX) ]
         yinds = [(floor y)..(min (floor (y+yd)) maxY) ]
         area xi yi = ((x2 xi)-(x1 xi))*((y2 xi)-(y1 xi))
         x1 xi = max (fromIntegral xi) x
         x2 xi = min (fromIntegral (xi+1)) (x+xd)
         y1 yi = max (fromIntegral yi) y
         y2 yi = min (fromIntegral (yi+1)) (y+yd)
         valAndWgt = map (\(a,b) -> (pixelAt img a b,area a b)) inds

