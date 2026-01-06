{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), decode, withObject, (.:), (.:?), (.=), object, Value)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (Sum(..))
import Data.Semigroup (Min(..), Max(..))
import Control.Monad (join)
import qualified Data.Foldable as F
import qualified Data.ByteString.Lazy as B


-- 3.0 Dodawanie/Odejmowanie
data FunctorIN = FunctorIN { fVal1 :: Int, fVal2 :: Int, fOp :: String } deriving (Show, Generic)
instance FromJSON FunctorIN where
    parseJSON = withObject "FunctorIN" $ \v -> FunctorIN <$> v .: "val1" <*> v .: "val2" <*> v .: "operation"

-- 3.5 Konkatenacja list
data ThreeListsIN = ThreeListsIN { tl1 :: [Int], tl2 :: [Int], tl3 :: [Int] } deriving (Show, Generic)
instance FromJSON ThreeListsIN where
    parseJSON = withObject "ThreeListsIN" $ \v -> ThreeListsIN <$> v .: "list1" <*> v .: "list2" <*> v .: "list3"

data ListOUT = ListOUT { resList :: [Int] } deriving (Show, Generic)
instance ToJSON ListOUT where toJSON (ListOUT r) = object ["result" .= r]

-- 4.0 Suma lub null
data ListMaybeIN = ListMaybeIN { lmList :: Maybe [Int] } deriving (Show, Generic)
instance FromJSON ListMaybeIN where
    parseJSON = withObject "ListMaybeIN" $ \v -> ListMaybeIN <$> v .:? "list" 
    -- .:? - pozwala na null/brak pola

data IntMaybeOUT = IntMaybeOUT { resInt :: Maybe Int } deriving (Show, Generic)
instance ToJSON IntMaybeOUT where toJSON (IntMaybeOUT r) = object ["result" .= r]

-- 4.5 Combine & Join
data SetHeadIN = SetHeadIN { shElement :: Int, shList :: [Int] } deriving (Show, Generic)
instance FromJSON SetHeadIN where
    parseJSON = withObject "SetHeadIN" $ \v -> SetHeadIN <$> v .: "element" <*> v .: "list"

-- 5.0 FoldMap
data StatsIN = StatsIN { stList :: [Int] } deriving (Show, Generic)
instance FromJSON StatsIN where
    parseJSON = withObject "StatsIN" $ \v -> StatsIN <$> v .: "list"

data StatsOUT = StatsOUT { sCount :: Int, sSum :: Int, sMin :: Int, sMax :: Int } deriving (Show, Generic)
instance ToJSON StatsOUT where
    toJSON (StatsOUT c s mi ma) = object ["count" .= c, "sum" .= s, "min" .= mi, "max" .= ma]

data ErrorOUT = ErrorOUT { errorMsg :: String } deriving (Show, Generic)
instance ToJSON ErrorOUT where toJSON (ErrorOUT e) = object ["error" .= e]

data IntRes = IntRes { valInt :: Int } deriving (Show, Generic)
instance ToJSON IntRes where toJSON (IntRes v) = object ["value" .= v]


data Stats = Stats 
    { statCount :: Sum Int
    , statSum   :: Sum Int
    , statMin   :: Min Int
    , statMax   :: Max Int 
    }

instance Semigroup Stats where
    (Stats c1 s1 min1 max1) <> (Stats c2 s2 min2 max2) = 
        Stats (c1 <> c2) (s1 <> s2) (min1 <> min2) (max1 <> max2)

instance Monoid Stats where
    mempty = Stats (Sum 0) (Sum 0) (Min maxBound) (Max minBound)

toStats :: Int -> Stats
toStats x = Stats (Sum 1) (Sum x) (Min x) (Max x)


readJsonFromFile :: FromJSON a => FilePath -> IO a
readJsonFromFile path = do
    putStrLn $ "Wczytuję plik: " ++ path
    content <- B.readFile path
    case decode content of
        Just obj -> return obj
        Nothing -> error $ "Błąd parsowania JSON w pliku: " ++ path


-- main


main :: IO ()
main = scotty 3000 $ do

    -- Zadanie 3.0: Funktor (fmap na Maybe/Input)
    post "/functorMap" $ do
        input <- liftIO $ readJsonFromFile "zad.3.0.json" :: ActionM FunctorIN
        let v1 = fVal1 input
        let v2 = fVal2 input
        let op = fOp input
        
        let result = fmap (\o -> if o == "+" then v1 + v2 else v1 - v2) (Just op)
        
        case result of
            Just val -> json $ IntRes val
            Nothing  -> json $ ErrorOUT "Unknown operation"

    -- Zadanie 3.5: Monoid (Konkatenacja list - mconcat)
    post "/monoidConcat" $ do
        input <- liftIO $ readJsonFromFile "zad.3.5.json" :: ActionM ThreeListsIN
        let result = mconcat [tl1 input, tl2 input, tl3 input]
        json $ ListOUT result

    -- Zadanie 4.0: Monada (Suma lub nil)
    post "/monadSum" $ do
        input <- liftIO $ readJsonFromFile "zad.4.0.json" :: ActionM ListMaybeIN
        let result = lmList input >>= (\lista -> return (sum lista))
        json $ IntMaybeOUT result

    -- Zadanie 4.5: SetHead (combine & join)
    post "/setHeadMonad" $ do
        input <- liftIO $ readJsonFromFile "zad.4.5.json" :: ActionM SetHeadIN
        let h = shElement input
        let t = shList input 
        
        -- [[h], t] -> [h, ...t]
        let nested = [[h], t] 
        let result = join nested 
        
        json $ ListOUT result

    -- Zadanie 5.0: Statystyki (Monada + foldMap)
    post "/statsFoldMap" $ do
        input <- liftIO $ readJsonFromFile "zad.5.0.json" :: ActionM StatsIN
        let xs = stList input
        
        if length xs < 3 
            then json $ ErrorOUT "Lista musi mieć minimum 3 elementy"
            else do
                let resultStats = F.foldMap toStats xs
                let (Stats (Sum c) (Sum s) (Min mi) (Max ma)) = resultStats
                json $ StatsOUT c s mi ma