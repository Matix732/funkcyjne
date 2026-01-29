{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), object)
import GHC.Generics (Generic)
import Data.Monoid (Sum(..))
import Data.Semigroup (Min(..), Max(..))
import qualified Data.Foldable as F
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

data FunctorIN = FunctorIN { fVal1 :: Int, fVal2 :: Int, fOp :: String } deriving (Show, Generic)
instance FromJSON FunctorIN where parseJSON = withObject "FunctorIN" $ \v -> FunctorIN <$> v .: "val1" <*> v .: "val2" <*> v .: "operation"

data ThreeListsIN = ThreeListsIN { tl1 :: [Int], tl2 :: [Int], tl3 :: [Int] } deriving (Show, Generic)
instance FromJSON ThreeListsIN where parseJSON = withObject "ThreeListsIN" $ \v -> ThreeListsIN <$> v .: "list1" <*> v .: "list2" <*> v .: "list3"

data StatsIN = StatsIN { stList :: [Int] } deriving (Show, Generic)
instance FromJSON StatsIN where parseJSON = withObject "StatsIN" $ \v -> StatsIN <$> v .: "list"

data IntRes = IntRes { valInt :: Int } deriving (Show, Generic)
instance ToJSON IntRes where toJSON (IntRes v) = object ["value" .= v]

data ListOUT = ListOUT { resList :: [Int] } deriving (Show, Generic)
instance ToJSON ListOUT where toJSON (ListOUT r) = object ["result" .= r]

data StatsOUT = StatsOUT { sCount :: Int, sSum :: Int, sMin :: Int, sMax :: Int } deriving (Show, Generic)
instance ToJSON StatsOUT where toJSON (StatsOUT c s mi ma) = object ["count" .= c, "sum" .= s, "min" .= mi, "max" .= ma]

data ErrorOUT = ErrorOUT { errorMsg :: String } deriving (Show, Generic)
instance ToJSON ErrorOUT where toJSON (ErrorOUT e) = object ["error" .= e]

data Stats = Stats { statCount :: Sum Int, statSum :: Sum Int, statMin :: Min Int, statMax :: Max Int }
instance Semigroup Stats where (Stats c1 s1 min1 max1) <> (Stats c2 s2 min2 max2) = Stats (c1 <> c2) (s1 <> s2) (min1 <> min2) (max1 <> max2)
instance Monoid Stats where mempty = Stats (Sum 0) (Sum 0) (Min maxBound) (Max minBound)
toStats :: Int -> Stats
toStats x = Stats (Sum 1) (Sum x) (Min x) (Max x)

main :: IO ()
main = do
    portStr <- lookupEnv "PORT"
    let port = read (fromMaybe "3000" portStr) :: Int
    
    putStrLn $ "Start server on port: " ++ show port

    scotty port $ do
        get "/" $ text "Haskell Azure Service is Running!"

        post "/functorMap" $ do
            input <- jsonData :: ActionM FunctorIN
            let v1 = fVal1 input
            let v2 = fVal2 input
            let op = fOp input
            let result = fmap (\o -> if o == "+" then v1 + v2 else v1 - v2) (Just op)
            case result of
                Just val -> json $ IntRes val
                Nothing  -> json $ ErrorOUT "Unknown operation"

        post "/monoidConcat" $ do
            input <- jsonData :: ActionM ThreeListsIN
            let result = mconcat [tl1 input, tl2 input, tl3 input]
            json $ ListOUT result

        post "/statsFoldMap" $ do
            input <- jsonData :: ActionM StatsIN
            let xs = stList input
            if length xs < 3 
                then json $ ErrorOUT "Lista musi mieć minimum 3 elementy"
                else do
                    let resultStats = F.foldMap toStats xs
                    let (Stats (Sum c) (Sum s) (Min mi) (Max ma)) = resultStats
                    json $ StatsOUT c s mi ma