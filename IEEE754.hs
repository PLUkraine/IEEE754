{-# LANGUAGE FlexibleContexts #-}
module IEEE754 (
    Float32,
    extractExponent,
    extractMantissa,
    bitsToInt,
    intToBits,
    doubleToFloat32,
    plusInfinity,
    plusZeroFloat32,
    maxDenormalized,
    minusInfinity,
    minusZeroFloat32,
    minDenormalized,
    randonNan,
    showFloat32Internals,
    extractDenormalizedMantissa
    ) where

-- Sign of the number
type Sign = Bool
-- Either 0 or 1
type Bit = Int
-- List of Bits. First bit is the least significant (little endian)
type Bits = [Bit]
-- Sign, Exp, Mantissa
data Float32 = Float32 {getRawSign ::  Sign, getExpList :: Bits, getMantList :: Bits }

-- number of bits in mantissa
n_man :: Int
n_man = 23
-- number of bits in exponent
n_exp :: Int
n_exp = 8
-- nan representation
nan :: String
nan = "NaN"

isZero :: Int -> Bool
isZero x = x == 0 
isOne :: Int -> Bool
isOne x = x == 1

-- Get sign of the number
getSign :: Float32 -> Int
getSign (Float32 s _ _) = if s then -1 else 1
-- Convert list of bits to Int
bitsToInt :: Bits -> Int
bitsToInt = foldr collectNumber 0 where
    collectNumber :: Bit -> Int -> Int
    collectNumber b acc = acc * 2 + b 
-- Convert exponent to Int
getExp :: Float32 -> Int
getExp (Float32 _ e _) = bitsToInt e
-- Convert mantissa to Int
getMant :: Float32 -> Int
getMant (Float32 _ _ m) = bitsToInt m

-- +0 in IEEE753
plusZeroFloat32 :: Float32
plusZeroFloat32 = Float32 False (replicate n_exp 0) (replicate n_man 0)
-- -0 in IEEE753
minusZeroFloat32 :: Float32
minusZeroFloat32 = Float32 True (replicate n_exp 0) (replicate n_man 0)
-- +Inf in IEEE753
plusInfinity :: Float32
plusInfinity = Float32 False (replicate n_exp 1) (replicate n_man 0)
-- -Inf in IEEE753
minusInfinity :: Float32
minusInfinity = Float32 True (replicate n_exp 1) (replicate n_man 0)
-- maximum denormalized value in IEEE754
maxDenormalized :: Float32
maxDenormalized = Float32 False (replicate n_exp 0) (replicate n_man 1)
-- minimum denormalized value in IEEE754
minDenormalized :: Float32
minDenormalized = Float32 False (replicate n_exp 0) (1 : replicate (n_man-1) 0)
-- some NaN value (there are many)
randonNan :: Float32
randonNan = Float32 False (replicate n_exp 1) (1: replicate (n_man-1) 0)

-- Convert assuming number has normalized (1<=x<10) mantissa
fromNormalized :: Floating b => Float32 -> b 
fromNormalized fl = let res = 2.0**(fromIntegral (getExp fl) - 2.0**(fromIntegral n_exp-1)+1) * 
                                                            (1.0 + fromIntegral (getMant fl) / (2.0**fromIntegral  n_man)) in
                                                fromIntegral (getSign fl) * res

-- Convert assuming number has denormalized (0.1<=x<1) mantissa
fromDenormalized :: Floating b => Float32 -> b 
fromDenormalized fl = let res = 2.0**(fromIntegral (getExp fl) - 2.0**(fromIntegral n_exp-1)+2) * 
                                                            (fromIntegral (getMant fl) / (2.0**fromIntegral  n_man)) in
                                                fromIntegral (getSign fl) * res

instance Bounded Float32 where
    -- number before -Inf
    minBound = Float32 True  ([0]++replicate (n_exp-1) 1) (replicate n_man 1)
    -- number before +Inf
    maxBound = Float32 False ([0]++replicate (n_exp-1) 1) (replicate n_man 1)

instance Show Float32 where
    -- Show as regular float value or Inf or NaN
    show fl@(Float32 sign expon mant)
        | all isOne expon = if all isZero mant then showInfinity sign
                            else showNan
        | all isZero expon = if all isZero mant then showZero sign
                             else show $ fromDenormalized fl
        | otherwise = show $ fromNormalized fl where 
            showInfinity :: Sign -> String
            showInfinity s = if s then "-Inf" else "Inf"
            showNan :: String
            showNan = nan
            showZero :: Sign -> String
            showZero s = if s then "-0" else "0"

-- show sign bit, exponent and mantissa bit by bit
showFloat32Internals :: Float32 -> String
showFloat32Internals (Float32 s e m) = unwords [showSign s, show e, show m] where
    showSign sign = if sign then "1" else "0"

-- convert int to list of bits
intToBits :: Integer -> Bits 
intToBits num = go num
    where
    go 0 = []
    go n = fromInteger ( n `mod` 2 ) : go ( n `div` 2 )

-- get exponent from decimal
extractExponent :: Double -> Int
extractExponent = floor . logBase 2

-- get mantissa from decimal
extractMantissa :: Double -> Bits
extractMantissa d = let intPart = intToBits $ truncate d
                        dbl = go (d - fromIntegral (truncate d))
                        r_d = if null intPart then dropWhile isZero dbl else dbl
                    in (reverse intPart) ++ r_d
                    -- in reverse $ take n_man $ tail $ (reverse intPart) ++ (take (n_man+1) r_d)
    where
    go :: Double -> Bits
    go db = let tr = truncate $ 2.0 * db in
        tr : go (2.0*db - fromIntegral tr)

extractDenormalizedMantissa :: Double -> Bits
extractDenormalizedMantissa arg = let
                                    d = arg * 2.0**(2^(n_exp-1) - 2)
                                in go d
    where
    go :: Double -> Bits
    go db = let tr = truncate $ 2.0 * db in
        tr : go (2.0*db - fromIntegral tr)
                                

-- convert Double to Float32
doubleToFloat32 :: Double -> Float32
doubleToFloat32 arg 
    | abs arg > fromNormalized maxBound =   if arg > 0 
                                            then plusInfinity 
                                            else minusInfinity
    | abs arg < fromDenormalized minDenormalized =  if arg > 0 
                                                    then plusZeroFloat32 
                                                    else minusZeroFloat32
    | fromDenormalized minDenormalized <= abs arg 
        && abs arg <= fromDenormalized maxDenormalized = 
            let d = abs arg
                expoLst = replicate n_exp 0
                mant = reverse $ take n_man $ extractDenormalizedMantissa d
            in Float32 (arg < 0) expoLst mant
    | otherwise =
            let d = abs arg
                expo = intToBits $ fromIntegral $ 2^(n_exp-1) - 1 + extractExponent d
                expoLst = take n_exp $ expo ++ repeat 0
                mant = reverse $ take n_man $ tail $ extractMantissa d
            in Float32 (arg < 0) expoLst mant