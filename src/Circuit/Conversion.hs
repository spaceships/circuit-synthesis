module Circuit.Conversion where

import Circuit

class ToAcirc g where
    toAcirc :: Circuit g -> Acirc
    toAcirc = error "no conversion to arithmetic circuit defined"

instance ToAcirc BoolGate where
    -- TODO: define me

instance ToAcirc ArithGate where
    toAcirc = id

class ToCirc g where
    toCirc :: Circuit g -> Circ
    toCirc = error "no conversion to binary circuit defined"

instance ToCirc ArithGate where
instance ToCirc BoolGate where
    toCirc = id
