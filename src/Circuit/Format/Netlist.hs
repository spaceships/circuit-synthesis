{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Circuit.Format.Netlist
    ( Circuit.Format.Netlist.read
    , readNetlist
    ) where

import Circuit
import Circuit.Parser
import Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans
import Lens.Micro.Platform
import Text.Parsec hiding (spaces)
import qualified Data.IntMap as IM
import qualified Control.Monad.State.Strict as S

type Wire = Int
data NetlistOutput = OutputWire Wire
                   | Passthrough Int -- passthrough input

data NetlistSt = NetlistSt
               { _nl_gates    :: IM.IntMap (String, [Wire])
               , _nl_inputs   :: IM.IntMap Wire
               , _nl_outputs  :: IM.IntMap NetlistOutput
               }
makeLenses ''NetlistSt

type ParseNetlist g = ParseCirc g NetlistSt

--------------------------------------------------------------------------------

read :: Gate g => FilePath -> IO (Circuit g)
read file = fst <$> readNetlist file

readNetlist :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readNetlist file = do
    s <- readFile file
    let (_,_,st) = execCircParser (NetlistSt IM.empty IM.empty IM.empty) parseNetlist s
    return (buildNetlist st, [])

--------------------------------------------------------------------------------
-- read the netlist file first: parse it into a NetlistSt, then translate it into
-- a circuit.

buildNetlist :: Gate g => NetlistSt -> Circuit g
buildNetlist st = flip S.evalState IM.empty $ buildCircuitT $ do
    inpRefs <- B.inputs (length (st^.nl_inputs))
    lift $ S.put (IM.fromList (zip (IM.elems (st^.nl_inputs)) inpRefs))
    outRefs <- mapM (build inpRefs) (IM.elems (st^.nl_outputs))
    B.outputs outRefs
  where
    build _ (OutputWire w) = buildRec w
    build inps (Passthrough i) = return (inps !! i)

    buildRec :: Gate g => Int -> BuilderT g (S.State (IM.IntMap Ref)) Ref
    buildRec !w = lift (use (at w)) >>= \case
        Just ref -> return ref
        Nothing  -> do
            let (ty,args) = st ^. nl_gates . at w . non (error ("[buildNetlist] missing wire " ++ show w))
            args' <- mapM buildRec args
            ref <- case ty of
                "AND"  -> circMul (args'!!0) (args'!!1)
                "XOR"  -> circXor (args'!!0) (args'!!1)
                "NOT"  -> circNot (args'!!0)
                "BUFF" -> return (args'!!0)
            lift (at w ?= ref)
            return ref

parseNetlist :: ParseNetlist g ()
parseNetlist = do
    void $ many $ try (spaces >> choice uselessLines >> spaces >> endLine)
    void $ many $ try (spaces >> (binaryGate <|> unaryGate) >> endLine)
    void $ many $ try (spaces >> assignment >> endLine)
    string "endmodule\n"
    eof
  where
    uselessLines = [comment, moduleDeclaration, wireDeclaration, inputDeclaration,
                    outputDeclaration, emptyLine]

emptyLine :: ParseNetlist g ()
emptyLine = spaces <?> "empty line"

comment :: ParseNetlist g ()
comment = commentType "(*" "*)" <|> commentType "/*" "*/" <?> "comment"
  where
    commentText = void $ many (alphaNum <|> oneOf " -_.(),=\":")
    commentType begin end = between (string begin) (string end) commentText

moduleDeclaration :: ParseNetlist g ()
moduleDeclaration = string "module" >> junk

wireDeclaration :: ParseNetlist g ()
wireDeclaration = string "wire" >> junk

inputDeclaration :: ParseNetlist g ()
inputDeclaration = string "input" >> junk

outputDeclaration :: ParseNetlist g ()
outputDeclaration = string "output" >> junk

junk :: ParseCirc g a ()
junk = void $ many (alphaNum <|> oneOf " _-.(),=\":/;*")

binaryGate :: ParseNetlist g ()
binaryGate = do
    ty <- oneOfStr ["AND", "XOR"]
    spaces
    char '_'
    wire <- int
    string "_ (" >> endLine >> spaces >> string ".A(_"
    a <- int
    string "_)," >> endLine >> spaces >> string ".B(_"
    b <- int
    string "_)," >> endLine >> spaces >> string ".Y(_"
    c <- int
    string "_)" >> endLine >> spaces >> string ");"
    let args = [a,b]
    modifySt $ nl_gates . at c ?~ (ty,args)

unaryGate :: ParseNetlist g ()
unaryGate = do
    ty <- string "NOT" <|> string "BUFF" <?> "unkonwn gate"
    spaces
    char '_'
    wire <- int
    string "_ (" >> endLine >> spaces >> string ".A(_"
    a <- int
    string "_)," >> endLine >> spaces >> string ".Y(_"
    c <- int
    string "_)" >> endLine >> spaces >> string ");"
    let args = [a]
    modifySt $ nl_gates . at c ?~ (ty,args)

assignment :: ParseNetlist g ()
assignment = do
    string "assign"
    spaces
    input <|> try output <|> inputToOutput <?> "unknown assignment"
  where
    input = do
        char '_'
        w <- int
        string "_ = pi"
        i <- int
        char ';'
        modifySt $ nl_inputs . at i ?~ w
    output = do
        string "po"
        i <- int
        string " = _"
        w <- int
        string "_;"
        modifySt $ nl_outputs . at i ?~ OutputWire w
    inputToOutput = do
        string "po"
        out <- int
        string " = pi"
        inp <- int
        char ';'
        modifySt $ nl_outputs . at out ?~ Passthrough inp
