{-# LANGUAGE DataKinds #-}

import Options.Declarative

import Qad
import Sqr
import Sin
import Cos
import Tan
import Ctg
import Log
import Abs
import Xor

main :: IO ()
main = run_ $
    Group "Encryption functions"
    [ subCmd "qad" qd
    , subCmd "sqr" sq
    , subCmd "sin" sn
    , subCmd "cos" cs
    , subCmd "tan" tn
    , subCmd "ctg" cg
    , subCmd "log" lg
    , subCmd "abs" as
    , subCmd "xor" xr
    ]
