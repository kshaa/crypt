{-# LANGUAGE DataKinds #-}

import Codec.Picture
import Codec.Picture.RGBA8
import Control.Monad.IO.Class (liftIO)
import Options.Declarative
import Encryptions

main :: IO ()
main = run_ flags

flags :: Flag "m" '["module"] "MODULE" "cryption module to be used, possible modules: quad; sqrt; sin; cos; tan; ctg; lg; abs; xor" String
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
        ("quad")   -> mapImgSave qd img path
        ("unquad") -> mapImgSave uqd img path
        ("sqrt")   -> mapImgSave sq img path
        ("sin")    -> mapImgSave sn img path
        ("cos")    -> mapImgSave cs img path
        ("tan")    -> mapImgSave tn img path
        ("ctg")    -> mapImgSave cg img path
        ("lg")     -> mapImgSave lg img path
        ("abs")    -> mapImgSave ab img path
        ("xor")    -> mapImgSave xr img path
        otherwise  -> error $ "Module \'" ++ mod ++ "\' doesn't exist, see --help for list of usable modules"

