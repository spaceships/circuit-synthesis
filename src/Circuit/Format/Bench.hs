{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Circuit.Format.Bench where

import Circuit
import Circuit.Parser
import qualified Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans
import Lens.Micro.Platform
import Text.Parsec hiding (spaces)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Wire = String

data BenchSt = BenchSt
               { _b_gates   :: M.Map Wire Ref
               , _b_outputs :: V.Vector Wire
               }
makeLenses ''BenchSt

type ParseBench g = ParseCirc g BenchSt

--------------------------------------------------------------------------------

read :: Gate g => FilePath -> IO (Circuit g)
read file = fst <$> readBench file

readBench :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readBench file = runCircParser (BenchSt M.empty []) parseBench <$> readFile file

parseBench :: Gate g => ParseBench g ()
parseBench = do
    void $ many $ choice
        [ parseComment
        , parseInput
        , parseOutput
        , parseGate
        ]
    eof
    -- mark all the bench outputs as circuit outputs
    s <- getSt
    mapM_ (lift . B.output) (map ((s^.b_gates) M.!) (V.toList (s^.b_outputs)))

parseComment :: Gate g => ParseBench g ()
parseComment = do
    char '#'
    void $ manyTill anyChar (try endLine)

parseInput :: Gate g => ParseBench g ()
parseInput = do
    string "INPUT"
    wire <- between (char '(') (char ')') (many1 alphaNum)
    ref <- lift B.input
    modifySt (b_gates . at wire ?~ ref)
    endLine

parseOutput :: Gate g => ParseBench g ()
parseOutput = do
    string "OUTPUT"
    wire <- between (char '(') (char ')') (many1 alphaNum)
    modifySt $ b_outputs %~ (`V.snoc` wire)
    endLine

parseGate :: Gate g => ParseBench g ()
parseGate = do
    zwire <- many1 alphaNum
    spaces
    char '='
    spaces
    let parseNot = do string "NOT"
                      xwire <- between (char '(') (char ')') (many1 alphaNum)
                      xref  <- view (b_gates . at xwire . non (error "[parseGate] unknown argument")) <$> getSt
                      lift $ B.circNot xref

    let parseAnd = do string "AND"
                      char '('
                      xwire <- many1 alphaNum
                      char ','
                      spaces
                      ywire <- many1 alphaNum
                      char ')'
                      xref <- view (b_gates . at xwire . non (error "[parseGate] unknown argument")) <$> getSt
                      yref <- view (b_gates . at ywire . non (error "[parseGate] unknown argument")) <$> getSt
                      lift $ B.circAnd xref yref

    zref <- parseNot <|> parseAnd <?> "[parseGate] unknown gate!"
    endLine
    modifySt (b_gates . at zwire ?~ zref)
