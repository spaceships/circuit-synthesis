module Circuit.Conversion where

import Circuit
import Lens.Micro.Platform

--------------------------------------------------------------------------------

class ToAcirc g where
    toAcirc :: Circuit g -> Acirc
    toAcirc = error "no conversion to Acirc defined"

instance ToAcirc BoolGate where
    -- TODO: define me

instance ToAcirc ArithGate where
    toAcirc = id

instance ToAcirc ArithGate2 where
    toAcirc = circ_refmap . each %~ getArithGate

--------------------------------------------------------------------------------

class ToCirc g where
    toCirc :: Circuit g -> Circ
    toCirc = error "no conversion to binary circuit defined"

instance ToCirc ArithGate where

instance ToCirc ArithGate2 where

instance ToCirc BoolGate where
    toCirc = id

--------------------------------------------------------------------------------

class ToAcirc2 g where
    toAcirc2 :: Circuit g -> Acirc2
    toAcirc2 = error "no conversion to Acirc2 defined"

instance ToAcirc2 ArithGate2 where
    toAcirc2 = id

instance ToAcirc2 ArithGate where
    toAcirc2 = circ_refmap . each %~ ArithGate2

instance ToAcirc2 BoolGate where
    -- TODO: define me
