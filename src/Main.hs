{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding (head, div)
import           Data.String
import           Data.Char (chr,ord)
import           System.Directory
import           System.FilePath
import           Codec.Picture
import           Network.HTTP hiding (GET,POST)
import           Network.URI (parseURI)

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Blaze
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes hiding (dir, method, form)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as B

import           Mosaic
import           DataAccess

toBS str = B.pack $ map (fromIntegral . ord) str

emptyHtml :: Html
emptyHtml = htmlStr ""

htmlStr :: String -> Html
htmlStr = toHtml

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (homePageHandler)
    <|>
    route [ ("addphoto", addPhotoHandler)
          , ("added", addedHandler)
          , ("about", writeBS "about")
          , ("echo/:echoparam", echoHandler)
          , ("", serveDirectory "")
          ]
    <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

homePageHandler :: Snap ()
homePageHandler = method GET getter
   where getter = do
            dyn <- liftIO $ readImage "images/Sir Ian.png"
            case dyn of
               Left  _     -> writeBS "fuck"
               Right image -> do
                  let arr = scaleMosic ((\(ImageRGB8 img) -> img) image) 50
                  blaze $ homePage (concat arr) 50

addPhotoHandler :: Snap ()
addPhotoHandler = method GET getter
   where getter = do
            blaze addPhotoPage

addedHandler :: Snap ()
addedHandler = method POST setter
   where setter = do
            url <- getParam "photoUrl"

            case url of
               Nothing  -> writeBS "No Url"
               Just u   -> do
                  res <- liftIO $ downloadImage (map (chr . fromIntegral) (B.unpack u))
                  case res of
                     Left err -> writeBS err
                     Right r  -> do
                        dyn <- liftIO $ readImage r
                        case dyn of
                           Left er  -> writeBS $ toBS er
                           Right dy -> do
                              let rgb8 = rgb8Image dy
                                  pixs = concat $ scaleAvgPixs rgb8 50
                              paths <- liftIO $ mapM getClosestColor pixs

                              blaze (homePage (map tileImage paths) 50)

getFile :: HStream a => String -> IO (Maybe a)
getFile url = do
   case (parseURI url) of
      Nothing  -> return Nothing
      Just u   -> do
         resp <- ( (simpleHTTP . defaultGETRequest_) u ) >>= getResponseBody
         return (Just resp)

isImageExt :: String -> Bool
isImageExt ".jpg"    = True
isImageExt ".jpeg"   = True
isImageExt ".png"    = True
isImageExt _         = False

-- tries to download the image given by the url if fails returns the error
-- on the left, success, filepath on the right
downloadImage :: String -> IO (Either B.ByteString FilePath)
downloadImage url = do
   let ext = takeExtension url

   case isImageExt ext  of
      False -> return (Left "Not supported image type")
      True  -> do

         file  <- getFile url

         case file of
            Nothing -> return
               (Left "Could not download the file from specified url")
            Just f  -> do
               let fname = "temp" ++ (takeExtension url)
               B.writeFile fname f
               return (Right fname)

addPhotoPage :: Html
addPhotoPage = do

   wrapper $ do

      form ! name "addphoto" ! action "added" ! A.method "post" $ do
         htmlStr "Image Url"
         input ! type_ "url" ! name "photoUrl"
         input ! type_ "submit" ! value "Add"


navBar :: Html
navBar = do
   div ! class_ "navBar" $ do

      div ! class_ "innerNav" $ do

         a ! href "/addphoto" $ htmlStr "Add Your Own Photo"

         a ! href "/about" $ htmlStr "About"

wrapper :: Html -> Html
wrapper inner = do
   docType
   head $ do
      link ! rel "stylesheet" ! type_ "text/css" ! href "Site.css"

   body $ do

      navBar

      inner

homePage :: [FilePath] -> Int -> Html
homePage photoNames cols = do

   wrapper $ do

      div ! class_ "gridContainer" $ do
         pictureTable photoNames cols


pictureTable :: [FilePath] -> Int -> Html
pictureTable paths col = do
   table $ do
      picTable paths col
         where picTable _     0   = emptyHtml
               picTable [] _      = emptyHtml
               picTable paths col = do
                  let (row,remains) = splitAt col paths
                  pictureRow row col
                  picTable remains col

pictureRow :: [FilePath] -> Int -> Html
pictureRow paths cols = do
   tr $ do
      picRow paths cols
         where picRow _          0   = emptyHtml
               picRow [] _           = emptyHtml
               picRow (p:paths) cols = do
                  td $ do
                     div ! class_ "tile" $ do
                        img ! src (fromString p)

                  picRow paths (cols-1)




