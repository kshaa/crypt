module Encryptions where

import Codec.Picture
import Codec.Picture.Types
import Data.Word
import System.Environment

type Key = (Float,Float,Float)

mapImgSave :: (Key -> Word8 -> Word8) -> Image PixelRGBA8 -> String -> IO ()
mapImgSave crypt img path = do
    putStrLn "Enter 'a' value: "
    a <- getLine
    putStrLn "Enter 'b' value: "
    b <- getLine
    putStrLn "Enter 'c' value: "
    c <- getLine
    savePngImage (path ++ ".crypted") (ImageRGBA8 $ cryptedPixels (read a) (read b) (read c))
    writeFile (path ++ ".key") $ "Key: a=" ++ show a ++ " b=" ++ show b ++ " c=" ++ show c
    where cryptedPixels a b c = pixelMap (colorMap $ crypt (a, b, c)) $ (img)

-- Encryptions
qd :: Key -> Word8 -> Word8
qd (a, b, c) xw = (a*(xf^2)) + (b*xf) + c
    where xf = fromIntegral xw :: Float
sq = undefined
sn = undefined
cs = undefined
tn = undefined
cg = undefined
lg = undefined
ab = undefined
xr = undefined

-- Decryptions
uqd :: Key -> Word8 -> Word8
uqd (a, b, c) yw = round $ (-b+(sqrt((b^2)-4*a*(c-yf))))/2*a
    where yf = fromIntegral yw :: Float
