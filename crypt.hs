{-# LANGUAGE DataKinds #-}

import Codec.Picture
import Codec.Picture.RGBA8
import Control.Monad.IO.Class (liftIO)
import Options.Declarative
import Encryptions

main :: IO ()
main = run_ flags

flags :: Flag "m" '["module"] "MODULE" "cryption module to be used" String
      -> Arg "path to image" [String]
      -> Cmd "Image cryption" ()
flags mod args = do
    liftIO $ checkPath (get mod) (head $ get args)

checkPath :: String -> String -> IO ()
checkPath mod path = do
    content <- readImage path
    case content of
        Left str -> error str
        Right img -> checkModule mod path $ fromDynamicImage img

checkModule :: String -> String -> Image PixelRGBA8 -> IO ()
checkModule mod path img = do
    case mod of
        ("quad") -> qd img path
        ("unquad") -> uqd img path
        ("sqrt") -> sq img path
        ("sin") -> sn img path
        ("cos") -> cs img path
        ("tan") -> tn img path
        ("ctg") -> cg img path
        ("lg") -> lg img path
        ("abs") -> ab img path
        ("xor") -> xr img path
        otherwise -> error $ "Module \'" ++ mod ++ "\' doesn't exist, see --help for list of usable modules"

