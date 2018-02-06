{-# LANGUAGE OverloadedStrings #-}

module Circuit.Format.Acirc2 where

import Circuit
import Circuit.Conversion
import qualified Circuit.Format.Acirc as Acirc

import qualified Data.Text as T
import qualified Data.Text.IO as T

showWithTests :: Acirc2 -> [TestCase] -> T.Text
showWithTests c ts = T.append ":binary\n" (Acirc.showWithTests (toAcirc c) ts)

readWithTests :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readWithTests = Acirc.readWithTests

read :: Gate g => FilePath -> IO (Circuit g)
read = Acirc.read

write :: FilePath -> Acirc2 -> IO ()
write fp c = T.writeFile fp $ T.append ":binary\n" (Acirc.showWithTests (toAcirc c) [])
