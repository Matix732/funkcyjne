{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), decode, withObject, (.:), (.=), object, Value)
import Data.Aeson.Types (Parser)
import Control.Applicative ((<$>), (<*>))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class (liftIO)


-- Zadanie 3.0
data IsSortedIN = IsSortedIN { isList :: [Int], isOrder :: String } deriving (Show, Generic)
instance FromJSON IsSortedIN where
    parseJSON = withObject "IsSortedIN" $ \v -> IsSortedIN
        <$> v .: "list"
        <*> v .: "order"

data IsSortedOUT = IsSortedOUT { isSorted :: Bool } deriving (Show, Generic)
instance ToJSON IsSortedOUT

-- Zadanie 3.5
data ThreeListsIN = ThreeListsIN { tl1 :: [Int], tl2 :: [Int], tl3 :: [Int] } deriving (Show, Generic)
instance FromJSON ThreeListsIN where
    parseJSON = withObject "ThreeListsIN" $ \v -> ThreeListsIN
        <$> v .: "list1"
        <*> v .: "list2"
        <*> v .: "list3"

data SumOUT = SumOUT { sumResult :: [Int] } deriving (Show, Generic)
instance ToJSON SumOUT where
    toJSON (SumOUT res) = object ["result" .= res]

-- Zadanie 4.0
data SetHeadIN = SetHeadIN { shElement :: Int, shList :: [Int] } deriving (Show, Generic)
instance FromJSON SetHeadIN where
    parseJSON = withObject "SetHeadIN" $ \v -> SetHeadIN
        <$> v .: "element"
        <*> v .: "list"

data ListOUT = ListOUT { resultList :: [Int] } deriving (Show, Generic)
instance ToJSON ListOUT where
    toJSON (ListOUT res) = object ["result" .= res]


-- Zadanie 4.5
data AppendIN = AppendIN { apElement :: Int, apIndex :: Int, apList :: [Int] } deriving (Show, Generic)
instance FromJSON AppendIN where
    parseJSON = withObject "AppendIN" $ \v -> AppendIN
        <$> v .: "element"
        <*> v .: "index"
        <*> v .: "list"
-- Zadanie 5.0
data TwoListsIN = TwoListsIN { t2l1 :: [Int], t2l2 :: [Int] } deriving (Show, Generic)
instance FromJSON TwoListsIN where
    parseJSON = withObject "TwoListsIN" $ \v -> TwoListsIN
        <$> v .: "list1"
        <*> v .: "list2"

data SquareOUT = SquareOUT { resultSq :: [Double] } deriving (Show, Generic)
instance ToJSON SquareOUT where
    toJSON (SquareOUT res) = object ["result" .= res]

-- ===============================================
readJsonFromFile :: FromJSON a => FilePath -> IO a
readJsonFromFile path = do
    content <- B.readFile path
    case decode content of
        Just obj -> return obj
        Nothing -> error $ "Błąd JSON w pliku: " ++ path

-- main
main :: IO ()
main = scotty 3000 $ do

    -- Zadanie 3.0 - sprawdzanie czy lista jest posortowana
    post "/isSorted" $ do
        input <- liftIO $ readJsonFromFile "zad3.0.json"
        
        let xs = isList (input :: IsSortedIN)
        let ord = isOrder (input :: IsSortedIN)
        let compareFn = if ord == "abc" then (<=) else (>=)
        let pairs = zip xs (if null xs then [] else tail xs)
        let res = all (uncurry compareFn) pairs
        json $ IsSortedOUT res

    -- Zadanie 3.5 - sumowanie liczb z trzech list
    post "/sumRows" $ do
        input <- liftIO $ readJsonFromFile "zad3.5.json"
        
        let res = zipWith3 (\a b c -> a + b + c) (tl1 input) (tl2 input) (tl3 input)
        json $ SumOUT res

    -- Zadanie 4.0 - SetHead (dodanie na początek)
    post "/setHead" $ do
        input <- liftIO $ readJsonFromFile "zad4.0.json"
        
        let res = (shElement input) : (shList input)
        json $ ListOUT res

    -- Zadanie 4.5 - Append (wstawienie w indeks)
    post "/append" $ do
        input <- liftIO $ readJsonFromFile "zad4.5.json"
        
        let (start, end) = splitAt (apIndex input) (apList input)
        let res = start ++ (apElement input : end)
        json $ ListOUT res

    -- Zadanie 5.0 - SquareLists (zip i map)
    post "/squareLists" $ do
        input <- liftIO $ readJsonFromFile "zad5.0.json"
        
        let sums = zipWith (+) (t2l1 input) (t2l2 input)
        let res = map (\x -> (fromIntegral x) ** 2) sums
        json $ SquareOUT res