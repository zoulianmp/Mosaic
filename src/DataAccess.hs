
module DataAccess where

import           System.FilePath
import           Codec.Picture.Types

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Control.Applicative


import           Mosaic

data ImageRow = ImageRow
   {
     fullImage :: !FilePath
   , tileImage :: !FilePath
   , photoVal  :: !PhotoVal
   , mosaicIds :: !String
   } deriving (Show)

testRow = ImageRow "test.png" "square.png" (PhotoVal (PixelRGB8 100 170 20))
   "1,2"

testRow1 = ImageRow "test.png" "square.png" (PhotoVal (PixelRGB8 50 30 80))
   "1,2"

testRow2 = ImageRow "test.png" "square.png" (PhotoVal (PixelRGB8 200 100 10))
   "1,2"

toSqlValues (ImageRow f t (PhotoVal (PixelRGB8 r g b)) s) =
   [toSql f, toSql t,
   toSql ((fromIntegral r) :: Int),
   toSql ((fromIntegral g) :: Int),
   toSql ((fromIntegral b) :: Int),
   toSql s]

fromSqlValues entry = (ImageRow  (fromSql $ entry !! 1)
                           (fromSql $ entry !! 2)
                           (PhotoVal (PixelRGB8
                              (fromIntegral (fromSql (entry !! 3) :: Int))
                              (fromIntegral (fromSql (entry !! 4) :: Int))
                              (fromIntegral (fromSql (entry !! 5) :: Int))))
                           (fromSql $ entry !! 6))


connection = connectSqlite3 "Images.db"

tables = do
   conn <- connection
   ts <- getTables conn
   disconnect conn
   return ts

createImagesTable = do
   conn <- connection

   res <- run conn ("CREATE TABLE IF NOT EXISTS Images" ++
            "(Id INTEGER PRIMARY KEY AUTOINCREMENT," ++
            "FullImage TEXT NOT NULL, " ++
            "TileImage TEXT NOT NULL, " ++
            "Red INTEGER NOT NULL, " ++
            "Green INTEGER NOT NULL, " ++
            "Blue INTEGER NOT NULL, " ++
            "MosaicIds)") []
   commit conn
   disconnect conn


insert :: ImageRow -> IO Int
insert row = do
   conn <- connection

   createImagesTable

   res <- run conn ("INSERT INTO Images " ++
            "(FullImage, TileImage, Red, Green, Blue, MosaicIds) VALUES " ++
            "(    ?    ,     ?    ,  ? ,   ?  ,  ?  ,     ? )")
            (toSqlValues row)

   id <- quickQuery' conn "SELECT last_insert_rowid()" []

   commit conn

   disconnect conn

   return ((fromSql . head . head) id)

getClosestId :: Int -> IO [[SqlValue]]
getClosestId i = do
   conn <- connection

   res <- quickQuery' conn ("SELECT *, (MIN (ABS(rowid - (?)))) AS Dist FROM " ++
               "Images ORDER BY Dist DESC") [toSql i]

   disconnect conn

   return res

getById :: Int -> IO (Maybe ImageRow)
getById i = do
   conn <- connection

   res <- quickQuery' conn "SELECT * FROM Images WHERE rowid = (?)" [toSql i]

   disconnect conn

   case res of
      [entry] -> return (Just (fromSqlValues entry))
      _       -> return Nothing


getClosestColor :: PixelRGB8 -> IO ImageRow
getClosestColor (PixelRGB8 r g b) = do
   conn <- connection

   res <- quickQuery' conn ("SELECT *, " ++
               "MIN( " ++
               "(Red - " ++ (show r) ++ ")*(Red - " ++ (show r) ++ ") + " ++
               "(Green - " ++ (show g) ++ ")*(Green - " ++ (show g) ++ ") + " ++
               "(Blue - " ++ (show b) ++ ")*(Blue - " ++ (show b) ++ ")" ++
               ")AS Dist FROM " ++
               "Images ORDER BY Dist DESC") []

   disconnect conn

   let entry = head res

   return (fromSqlValues entry)

allRows str = do
   conn <- connection

   rows <- quickQuery' conn ("SELECT rowid,* FROM " ++ str) []

   disconnect conn

   return rows


