{-# LANGUAGE FlexibleContexts #-}

module Main where
import IEEE754
import Control.Monad

main :: IO ()
main = do
    putStrLn $ "+Infinity: " ++ showFloat32Internals plusInfinity
    putStrLn $ "-Infinity: " ++ showFloat32Internals minusInfinity
    putStrLn $ " 0: " ++ showFloat32Internals plusZeroFloat32
    putStrLn $ "-0: " ++ showFloat32Internals minusZeroFloat32
    putStrLn $ "NaN: " ++ showFloat32Internals randonNan
    putStrLn $ "Max: " ++ showFloat32Internals maxBound
    putStrLn $ "Min: " ++ showFloat32Internals minDenormalized 
    forever $ do
        putStrLn ""
        putStrLn "Type some decimal"
        num <- fmap (read :: String -> Double) getLine
        let floatNum = doubleToFloat32 num
        putStrLn $ show floatNum ++ ":"
        putStrLn $ "\t" ++ showFloat32Internals floatNum