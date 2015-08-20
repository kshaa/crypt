module Encryptions where

import Codec.Picture
import System.Environment

qd :: Image PixelRGBA8 -> String -> IO ()
qd img path = do
    putStrLn "Encrypting with formula ax² + bx + c."
    putStrLn "Enter 'a' value: "
    a <- getLine
    putStrLn "Enter 'b' value: "
    b <- getLine
    putStrLn "Enter 'c' value: "
    c <- getLine
    savePngImage (path ++ ".crypted") (ImageRGBA8 $ cryptedPixels (read a) (read b) (read c))
    writeFile (path ++ ".key") $ "Key: a=" ++ show a ++ " b=" ++ show b ++ " c=" ++ show c
    where cryptedPixels a b c = pixelMap (colorMap $ cryptColor a b c) img
          cryptColor a b c x = (a*(x^2)) + (b*x) + c

uqd :: Image PixelRGBA8 -> String -> IO ()
uqd img path = do
    putStrLn "Decrypting with formula x = (-b+(sqrt((b^2)-4*a*(c-y))))/2*a"
    putStrLn "Enter 'a' value: "
    a <- getLine
    putStrLn "Enter 'b' value: "
    b <- getLine
    putStrLn "Enter 'c' value: "
    c <- getLine
    savePngImage (path ++ ".crypted") (ImageRGBA8 $ cryptedPixels (read a) (read b) (read c))
    writeFile (path ++ ".key") $ "Key: a=" ++ show a ++ " b=" ++ show b ++ " c=" ++ show c
    where cryptedPixels a b c = pixelMap (colorMap $ cryptColor a b c) img
          cryptColor a b c y = round $ (-b+(sqrt((b^2)-4*a*(c-y))))/2*a

sq = undefined
sn = undefined
cs = undefined
tn = undefined
cg = undefined
lg = undefined
ab = undefined
xr = undefined
