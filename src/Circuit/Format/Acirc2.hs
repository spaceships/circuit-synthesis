{-# LANGUAGE OverloadedStrings #-}

module Circuit.Format.Acirc2 where

import Circuit
import Circuit.Conversion
import qualified Circuit.Format.Acirc as Acirc

import qualified Data.Text as T

showWithTests :: Acirc2 -> [TestCase] -> T.Text
showWithTests c ts = T.append ":binary\n" (Acirc.showWithTests (toAcirc c) ts)

