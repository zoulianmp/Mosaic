
module DataAccess where

import           System.FilePath
import           Codec.Picture.Types
import           Data.List (intersperse)

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

testRow1 = ImageRow
   "path1.png"
   "path2.png"
   (PhotoVal
      (PixelRGB8 120 1 200)
      (PixelRGB8 20 10 20)
      (PixelRGB8 120 240 100)
      (PixelRGB8 220 70 200)
   )
   "test1"

photoVal1 = (PhotoVal
               (PixelRGB8 120 1 200)
               (PixelRGB8 20 10 20)
               (PixelRGB8 120 240 100)
               (PixelRGB8 220 70 200)
            )

testRow2 = ImageRow
   "path1.png"
   "path2.png"
   (PhotoVal
      (PixelRGB8 120 240 100)
      (PixelRGB8 220 70 200)
      (PixelRGB8 120 1 200)
      (PixelRGB8 20 10 20)
   )
   "test1"


toSqlValues (ImageRow f t pv s) =
   [toSql f, toSql t]
   ++ (photoValToSql pv)
   ++ [toSql s]

photoValToSql (PhotoVal tl tr bl br) =
   (pixelRGB8ToSql tl)
   ++ (pixelRGB8ToSql tr)
   ++ (pixelRGB8ToSql bl)
   ++ (pixelRGB8ToSql br)

pixelRGB8ToSql (PixelRGB8 r g b) =
   [ toSql ((fromIntegral r) :: Int)
   , toSql ((fromIntegral g) :: Int)
   , toSql ((fromIntegral b) :: Int)
   ]

fromSqlValues entry =
   ImageRow
   (fromSql $ entry !! 1)
   (fromSql $ entry !! 2)
   (PhotoVal
      (PixelRGB8
         (fromIntegral (fromSql (entry !! 3) :: Int))
         (fromIntegral (fromSql (entry !! 4) :: Int))
         (fromIntegral (fromSql (entry !! 5) :: Int))
      )
      (PixelRGB8
         (fromIntegral (fromSql (entry !! 6) :: Int))
         (fromIntegral (fromSql (entry !! 7) :: Int))
         (fromIntegral (fromSql (entry !! 8) :: Int))
      )
      (PixelRGB8
         (fromIntegral (fromSql (entry !! 9) :: Int))
         (fromIntegral (fromSql (entry !! 10) :: Int))
         (fromIntegral (fromSql (entry !! 11) :: Int))
      )
      (PixelRGB8
         (fromIntegral (fromSql (entry !! 12) :: Int))
         (fromIntegral (fromSql (entry !! 13) :: Int))
         (fromIntegral (fromSql (entry !! 14) :: Int))
      )
   )
   (fromSql $ entry !! 15)


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
            "TL_Red INTEGER NOT NULL, " ++
            "TL_Green INTEGER NOT NULL, " ++
            "TL_Blue INTEGER NOT NULL, " ++
            "TR_Red INTEGER NOT NULL, " ++
            "TR_Green INTEGER NOT NULL, " ++
            "TR_Blue INTEGER NOT NULL, " ++
            "BL_Red INTEGER NOT NULL, " ++
            "BL_Green INTEGER NOT NULL, " ++
            "BL_Blue INTEGER NOT NULL, " ++
            "BR_Red INTEGER NOT NULL, " ++
            "BR_Green INTEGER NOT NULL, " ++
            "BR_Blue INTEGER NOT NULL, " ++
            "MosaicIds)") []
   commit conn
   disconnect conn

insertReplace :: (Int,ImageRow) -> IO ()
insertReplace (id, imgRow) = do
   conn <- connection

   createImagesTable

   putStrLn $ show id ++ " insert/replace"

   res <- run conn ("INSERT or REPLACE INTO Images "
            ++ "(rowid, FullImage, TileImage,"
            ++ "TL_Red, TL_Green, TL_Blue,"
            ++ "TR_Red, TR_Green, TR_Blue,"
            ++ "BL_Red, BL_Green, BL_Blue,"
            ++ "BR_Red, BR_Green, BR_Blue,"
            ++ "MosaicIds) VALUES "
            ++ "(" ++ (intersperse ',' $ replicate 16 '?') ++ ")")
            (toSql id : (toSqlValues imgRow))

   id <- quickQuery' conn "SELECT last_insert_rowid()" []

   commit conn

   disconnect conn

   return ()
   

insert :: ImageRow -> IO Int
insert row = do
   conn <- connection

   createImagesTable

   res <- run conn ("INSERT INTO Images "
            ++ "(FullImage, TileImage,"
            ++ "TL_Red, TL_Green, TL_Blue,"
            ++ "TR_Red, TR_Green, TR_Blue,"
            ++ "BL_Red, BL_Green, BL_Blue,"
            ++ "BR_Red, BR_Green, BR_Blue,"
            ++ "MosaicIds) VALUES "
            ++ "(" ++ (intersperse ',' $ replicate 15 '?') ++ ")")
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


--getClosestColor :: PhotoVal -> IO ImageRow
getClosestColor
      (PhotoVal
      (PixelRGB8 tl_r tl_g tl_b) (PixelRGB8 tr_r tr_g tr_b)
      (PixelRGB8 bl_r bl_g bl_b) (PixelRGB8 br_r br_g br_b) ) = do
   conn <- connection

   res <- quickQuery' conn (
            "SELECT rowid, FullImage, TileImage,"
            ++ "TL_Red, TL_Green, TL_Blue,"
            ++ "TR_Red, TR_Green, TR_Blue,"
            ++ "BL_Red, BL_Green, BL_Blue,"
            ++ "BR_Red, BR_Green, BR_Blue,"
            ++ "MosaicIds, "
            ++ "MIN( "
            ++ "(TL_Red - "   ++ (show tl_r) ++ ")*(TL_Red - "    ++ (show tl_r) ++ ") + "
            ++ "(TL_Green - " ++ (show tl_g) ++ ")*(TL_Green - "  ++ (show tl_g) ++ ") + "
            ++ "(TL_Blue - "  ++ (show tl_b) ++ ")*(TL_Blue - "   ++ (show tl_b) ++ ") + "
            ++ "(TR_Red - "   ++ (show tr_r) ++ ")*(TR_Red - "    ++ (show tr_r) ++ ") + "
            ++ "(TR_Green - " ++ (show tr_g) ++ ")*(TR_Green - "  ++ (show tr_g) ++ ") + "
            ++ "(TR_Blue - "  ++ (show tr_b) ++ ")*(TR_Blue - "   ++ (show tr_b) ++ ") + "
            ++ "(BL_Red - "   ++ (show bl_r) ++ ")*(BL_Red - "    ++ (show bl_r) ++ ") + "
            ++ "(BL_Green - " ++ (show bl_g) ++ ")*(BL_Green - "  ++ (show bl_g) ++ ") + "
            ++ "(BL_Blue - "  ++ (show bl_b) ++ ")*(BL_Blue - "   ++ (show bl_b) ++ ") + "
            ++ "(BR_Red - "   ++ (show br_r) ++ ")*(BR_Red - "    ++ (show br_r) ++ ") + "
            ++ "(BR_Green - " ++ (show br_g) ++ ")*(BR_Green - "  ++ (show br_g) ++ ") + "
            ++ "(BR_Blue - "  ++ (show br_b) ++ ")*(BR_Blue - "   ++ (show br_b) ++ ")"
            ++ ")AS Dist FROM Images"
            -- ++ "Images WHERE rowid >= 2200 ORDER BY Dist DESC"
            ) []

   disconnect conn

   let entry = head res

   return (fromSqlValues entry)

allRows :: String -> IO [(Int, ImageRow)]
allRows str = do
   conn <- connection

   rows <- quickQuery' conn ("SELECT * FROM " ++ str) []

   disconnect conn

   return $ map (\r  -> (fromSql (r !! 0),  fromSqlValues r)) rows


