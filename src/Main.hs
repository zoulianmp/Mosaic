
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
import           Data.List (intersperse)

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Blaze
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as BS

import           Mosaic
import           DataAccess

commaSeparated :: [String] -> String
commaSeparated []       = []
commaSeparated [x]      = x
commaSeparated (x:xs)   = x ++ "," ++ (commaSeparated xs)

isImageExt :: String -> Bool
isImageExt ".jpg"    = True
isImageExt ".jpeg"   = True
isImageExt ".png"    = True
isImageExt _         = False

fromCS :: String -> [String]
fromCS str
   | null l       = [f]
   | otherwise    = f : fromCS l
      where (f,l) = splACom str []

splACom :: String -> String -> (String, String)
splACom [] acc    = (reverse acc,[])
splACom (s:ss) acc
            | s == ','     = (reverse acc,ss)
            | otherwise    = splACom ss  (s : acc)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                  [(x,"")] -> Just x
                  _        -> Nothing

toBS :: String -> BS.ByteString
toBS str = BS.pack $ map (fromIntegral . ord) str

fromBS :: BS.ByteString -> String
fromBS bs = map (chr . fromIntegral) (BS.unpack bs)

emptyHtml :: Html
emptyHtml = htmlStr ""

htmlStr :: String -> Html
htmlStr = B.toHtml

-- ^^
-- Functions that should probably be elsewhere

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (homePageHandler)
    <|>
    route [
            ("addphoto", addPhotoHandler)
          , ("random", randomHandler)
          , ("added", addedHandler)
          , ("photo/:photoind", photoHandler)
          , ("about", writeBS "about")
          , ("", serveDirectory "")
          ]
    <|>
    dir "static" (serveDirectory ".")


randomHandler :: Snap ()
randomHandler = do
   redirect "/"

echoHandler :: Snap ()
echoHandler = do
    par <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS par

photoHandler :: Snap ()
photoHandler = do
   par <- getParam "photoind"
   case par of
      Nothing -> writeBS "Must specific photo index"
      Just id -> do
         case readMaybe (fromBS id) of
            Nothing -> writeBS "Could not parse id"
            Just i  -> do
               fPath <- liftIO $ getById i
               case fPath of
                  Nothing -> writeBS "Not in Database"
                  Just a  -> do
                     if ((not . null) (mosaicIds a))
                     then do
                        let fpaths = fromCS (mosaicIds a)
                        blaze $ homePage ('/' : fullImage a) (map ('/':) fpaths) 50
                     else do
                        (blaze .  wrapper . singleImage . ('/' :) . fullImage) a

singleImage :: FilePath -> Html
singleImage path = do
   B.img B.! A.src (fromString path)

homePageHandler :: Snap ()
homePageHandler = method GET getter
   where getter = do
            imgRow <- liftIO getRandom
            let fpaths = fromCS (mosaicIds imgRow)
            blaze $ homePage (fullImage imgRow) fpaths 50

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
                  -- download image off web
                  res <- liftIO $ downloadImage (fromBS u)
                  case res of
                     Left err -> writeBS err
                     Right r  -> do
                        -- readimage from drive
                        dyn <- liftIO $ readImage r
                        case dyn of
                           Left er  -> writeBS $ toBS er
                           Right dy -> do
                              -- get the mosaic
                              let !rgb8 = rgb8Image dy
                                  !pvs = concat $ scaleAvgPhotoVal rgb8 50
                                  !fullP = "images/" ++ (takeFileName (fromBS u))
                                  !tileP = "tiles/" ++ (takeFileName (fromBS u))
                                  (!sqrImage, !pVal) = prepareForDB (ImageRGB8 rgb8)


                              -- rewrite image to drive
                              dbEntries <- liftIO $ mapM getClosestColor pvs
                              let !fpaths  = map tileImage dbEntries
                                  !newEntry = ImageRow fullP tileP pVal
                                                 (commaSeparated fpaths)

                              liftIO $ savePngImage fullP (ImageRGB8 rgb8)
                              liftIO $ savePngImage tileP (ImageRGB8 sqrImage)
                              newId <- liftIO $ insert newEntry

                              redirect (toBS ("photo/" ++ show newId))
                              -- blaze (homePage (map tileImage dbEntries) 50)


getFile :: HStream a => String -> IO (Maybe a)
getFile url = do
   case (parseURI url) of
      Nothing  -> return Nothing
      Just u   -> do
         resp <- ( (simpleHTTP . defaultGETRequest_) u ) >>= getResponseBody
         return (Just resp)

-- tries to download the image given by the url if fails returns the error
-- on the left, success, filepath on the right
downloadImage :: String -> IO (Either BS.ByteString FilePath)
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
               let f_name = "temp" ++ (takeExtension url)
               BS.writeFile f_name f
               return (Right f_name)

addPhotoPage :: Html
addPhotoPage = do

   wrapper $ do

      B.form B.! A.name "addphoto" B.! A.action "added" B.! A.method "post" $ do
         htmlStr "Image Url"
         B.input B.! A.type_ "url" B.! A.name "photoUrl"
         B.input B.! A.type_ "submit" B.! A.value "Add"


navBar :: Html
navBar = do
   B.div B.! A.class_ "navBar" $ do

      B.div B.! A.class_ "innerNav" $ do

         B.table $ do
            B.tr $ do
               B.td $ do
                  navButton "Random" "/random"

               B.td $ do
                  navButton "Add Your Own Photo" "/addphoto"

               B.td $ do
                  navButton "About" "/about"

               {-
               B.td $ do
                  B.div B.! A.id "actualPhoto" B.! A.class_ "navButton" $ do 
                     htmlStr "Show Photo"
               -}

--navButton :: Html -> B.AttributeValue -> Html
navButton name location = do
   B.a B.! A.href location $ do
      B.div B.! A.class_ "navButton" $ do
         htmlStr name

wrapper :: Html -> Html
wrapper inner = do
   B.docType
   B.head $ do
      B.link B.! A.rel "stylesheet" B.! A.type_ "text/css" B.! A.href "/Site.css"

   B.body $ do

      navBar

      inner

homePage :: FilePath -> [FilePath] -> Int -> Html
homePage photo photoNames columns = do

   wrapper $ do

      B.div B.! A.class_ "gridContainer" $ do
         {-
         B.div B.! A.id "displayPhoto" $ do
            B.span $ htmlStr "Display Photo"
         -}
         B.div B.! A.id "actualContainer" $ do
            B.img B.! A.src (fromString photo) B.! A.id "actualImage"

         pictureTable photoNames columns

pictureTable :: [FilePath] -> Int -> Html
pictureTable paths column = do
   B.table $ do
      picTable paths column
         where picTable _     0   = emptyHtml
               picTable [] _      = emptyHtml
               picTable paths column = do
                  let (row,remains) = splitAt column paths
                  pictureRow row column
                  picTable remains column

pictureRow :: [FilePath] -> Int -> Html
pictureRow paths cols = do
   B.tr $ do
      picRow paths cols
         where picRow _          0   = emptyHtml
               picRow [] _           = emptyHtml
               picRow (p:paths) cols = do
                  B.td $ do
                     B.div B.! A.class_ "tile" $ do
                        B.a B.! A.href (fromString ("/photos/" ++ p)) $ do 
                           B.img B.! A.src (fromString p)

                  picRow paths (cols-1)



