{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson (ToJSON(..), object, (.=))
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)


data IntRes = IntRes { valInt :: Int } deriving (Show, Generic)
instance ToJSON IntRes where toJSON (IntRes v) = object ["value" .= v]

data DoubleRes = DoubleRes { valDouble :: Double } deriving (Show, Generic)
instance ToJSON DoubleRes where toJSON (DoubleRes v) = object ["value" .= v]

data ComplexRes = ComplexRes 
    { pair1 :: (Int, Double)
    , pair2 :: (Double, Int)
    , triple :: (Double, Double, Double) 
    } deriving (Show, Generic)
instance ToJSON ComplexRes


-- Zad 3.0: Int między 0 a maxBound
genRandomInt :: IO Int
genRandomInt = randomRIO (0, maxBound :: Int)

-- Zad 3.5: Double między 0 a 1 (bez 1)
genRandomDouble :: IO Double
genRandomDouble = randomRIO (0.0, 0.999999999999)

-- main
main :: IO ()
main = scotty 3000 $ do
    
    -- Zadanie 3.0: Losowy Int
    get "/randomInt" $ do
        val <- liftIO genRandomInt
        json $ IntRes val

    -- Zadanie 3.5: Losowy Double [0, 1)
    get "/randomDouble" $ do
        val <- liftIO genRandomDouble
        json $ DoubleRes val

    -- Zadanie 4.0: Struktury z 3.0 i 3.5
    get "/complexRandom" $ do
        i1 <- liftIO genRandomInt
        d1 <- liftIO genRandomDouble
        i2 <- liftIO genRandomInt
        d2 <- liftIO genRandomDouble
        d3 <- liftIO genRandomDouble
        d4 <- liftIO genRandomDouble
        d5 <- liftIO genRandomDouble
        json $ ComplexRes (i1, d1) (d2, i2) (d3, d4, d5)

    -- Zadanie 4.5: Losowy Double przy użyciu fmap (map)
    get "/randomMap" $ do
        val <- liftIO $ fmap (\x -> x) genRandomDouble
        json $ DoubleRes val

    -- Zadanie 5.0: Losowy Double przy użyciu >>= (flatMap)
    get "/randomFlatMap" $ do
        val <- liftIO $ genRandomDouble >>= \x -> return x
        json $ DoubleRes val