{-# LANGUAGE DataKinds #-}
module Abs where

import Codec.Picture
import Codec.Picture.RGBA8
import Codec.Picture.Repa
import Control.Monad.IO.Class (liftIO)
import qualified Data.Array.Repa as R
import Options.Declarative

as :: Flag "k" '["key"] "(a,b,c)" "y = ax^2 + bx + c"  (Def "(0.0,1.0,0.0)" String)
   -> Arg "path to image" [String]
   -> Cmd "Image cryption" ()
as key paths = do
    liftIO $ asIO (read $ get key) (get paths)
asIO :: (Float, Float, Float) -> [String] -> IO ()
asIO (a,b,c) paths = do
    let boundify i = R.map round i
        q = R.map ((\x -> a*(x^2) + b*x + c) . fromIntegral)
    xs <- sequence $ map readImageRGBA paths
    liftIO $ sequence_ $ flip map (zip xs paths)
        (\(x, path) -> case x of
            Left err -> putStrLn err
            Right img -> ((liftIO $ putStrLn $ ("Encrypting: " ++) $ show path) >>
                          (liftIO $ savePngImage (path ++ ".crypted") $ imgToImage $ onImg (R.computeS. boundify . q) img) >>
                          (liftIO $ putStrLn $ "Saved to: " ++ path ++ ".crypted")))
