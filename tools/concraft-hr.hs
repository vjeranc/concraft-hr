{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import           Control.Applicative ((<$>))
import           Control.Monad (unless, forM_)

import           Data.Double.Conversion.Text (toShortest)

import           System.Console.CmdArgs
import           System.IO (hFlush, stdout)

import qualified Network as N
import qualified Numeric.SGD as SGD

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text      as T
import qualified Data.Text.IO      as T
import qualified Data.List.Split as Split
import qualified Data.Char as Char
import           Data.Tagset.Positional (parseTagset)
import qualified Data.Tagset.Positional as P
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M

import qualified NLP.Concraft.Morphosyntax.Accuracy as Acc
import qualified NLP.Concraft.Guess as Guess

import qualified NLP.Concraft.Croatian as C
import           NLP.Concraft (tagset)
import qualified NLP.Concraft.Croatian.Request as R
import qualified NLP.Concraft.Croatian.Server as S
import qualified NLP.Concraft.Croatian.Morphosyntax as X
import qualified NLP.Concraft.Morphosyntax as X hiding (Word, orth)

import           Paths_concraft_hr (version, getDataFileName)
import           Data.Version (showVersion)

import qualified NLP.Morphosyntax.Analyzer as A

-- | Default port number.
portDefault :: Int
portDefault = 43982


---------------------------------------
-- Command line options
---------------------------------------

-- | A description of the Concraft-hr tool.
concraftDesc :: String
concraftDesc = "Concraft-hr " ++ showVersion version


data Concraft
  =
    -- | Training mode. Requires a path to the training file and output file, all
    -- other arguments are optional. It is recommended that one at least tries to
    -- find a proper regularization value, otherwise it is possible that the model
    -- fits the data a lot. On Croatian dataset the fit with the default regularization
    -- value is close to 100% (not good if you want to tag something other than your
    -- training set).
    Train
    { trainPath	    :: FilePath
    , evalPath      :: Maybe FilePath
    , tagsetPath    :: Maybe FilePath
    , noAna         :: Bool
    , inAnalyzer    :: Maybe FilePath
    , gIterNum      :: Double
    , dIterNum      :: Double   -- Double?
    , gBatchSize    :: Int
    , dBatchSize    :: Int
    , gRegVar       :: Double
    , dRegVar       :: Double
    , gGain0        :: Double
    , dGain0        :: Double
    , gTau          :: Double
    , dTau          :: Double
    , disk          :: Bool
    , outModel      :: FilePath
    , guessNum      :: Int
    , r0            :: Guess.R0T }
    -- | Tagging mode. This one requires a model and all other parameters are optional.
    -- It is recommended to precompute the analysis (set of possible tags) for all words
    -- if testing/evaluating the models, otherwise the analyzer is necessary for the
    -- tagging to work.
  | Tag
    { inModel       :: FilePath
    , inAnalyzer    :: Maybe FilePath
    , noAna         :: Bool
    , marginals     :: Bool }
    -- , guessNum      :: Int }
    -- | Server mode starts the server and requires a path to the model and analyzer.
    -- It is recommended to provide a path to the analyzer as it might
    -- happen that 'Client' started requires additional analysis. If not the analyzer
    -- won't be used but currently it's necessary to provide a path.
  | Server
    { inModel       :: FilePath
    , inAnalyzer    :: Maybe FilePath
    , port          :: Int }
    -- | Client mode starts a client which communicates with the 'Server' in a type-safe
    -- manner. The results are produced to standard output on the client side.
  | Client
    { noAna         :: Bool
    , tagsetPath    :: Maybe FilePath
    , marginals     :: Bool
    , host          :: String
    , port          :: Int }
    -- | Compare mode can be used to produce accuracy results for the two provided
    -- tagged sentences in files.
  | Compare
    { tagsetPath    :: Maybe FilePath
    , refPath       :: FilePath
    , otherPath     :: FilePath
    }
    -- | Prune mode given a path to the model, output model, and threshold value removes
    -- the features from the tagging model which have weights below the threshold (in log-domain).
  | Prune
    { inModel       :: FilePath
    , outModel      :: FilePath
    , threshold     :: Double }
--   | ReAna
--     { format	    :: Format }
  deriving (Data, Typeable, Show)


trainMode :: Concraft
trainMode = Train
    { trainPath = def &= argPos 1 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    , noAna = False &= help "Do not perform reanalysis - (False)"
    , inAnalyzer= def &= typFile &= help "Morphosyntactic analyzer - needed if noana is false"
    -- , discardHidden = False &= help "Discard hidden features"
    , gIterNum = 20 &= help "Number of SGD iterations of guessing model - (20)"
    , dIterNum = 20 &= help "Number of SGD iterations of disambiguation model - (20)"
    , gBatchSize = 50 &= help "Batch size for SGD of guessing model - (50)"
    , dBatchSize = 50 &= help "Batch size for SGD of disambiguation model - (50)"
    , gRegVar = 10.0 &= help "Regularization variance of guessing model - (10.0)"
    , dRegVar = 10.0 &= help "Regularization variance of disambiguation model - (10.0)"
    , gGain0 = 1.0 &= help "Initial gain parameter of guessing model - (1.0)"
    , dGain0 = 1.0 &= help "Initial gain parameter of disambiguation model - (1.0)"
    , gTau = 5.0 &= help "Initial tau parameter of guessing model - (5.0)"
    , dTau = 5.0 &= help "Initial tau parameter for disambiguation model - (5.0)"
    , disk = False &= help "Store SGD dataset on disk - (False)"
    , outModel = def &= typFile &= help "Output Model file"
    , guessNum = 10 &= help "Number of guessed tags for each unknown word - (10)"
    , r0 = Guess.OovChosen &= help "R0 construction method (AnyInterps, AnyChosen, OovChosen) - (OovChosen)" }
     &= help "Training mode - needs training data - outputs a CRF model to a given filepath.\
                \ Values or modes shown between the brackets are default values."


tagMode :: Concraft
tagMode = Tag
    { inModel   = def &= argPos 0 &= typ "MODEL-FILE"
    , inAnalyzer= def &= typFile &= help "Morphosyntactic analyzer - needed if noana is false"
    , noAna     = False &= help "Do not analyse input text - (False)"
    , marginals = False &= help "Tag with marginal probabilities - (False)" }
    -- , guessNum = 10 &= help "Number of guessed tags for each unknown word" }
    &= help "Given tokenized input data it outputs the same tokenized form with tags next to each word.\
                \ Values shown between the brackets are default values."

serverMode :: Concraft
serverMode = Server
    { inModel = def &= argPos 0 &= typ "MODEL-FILE" &= help "Concraft model file"
    , inAnalyzer= def &= typFile &= help "Morphosyntactic analyzer file"
    , port    = portDefault &= help "Port number" }
    &= help "Starts a server which can process requests of tokenized sentences - tagging and analysis."


clientMode :: Concraft
clientMode = Client
    { noAna   = False &= help "Do not perform reanalysis"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    , port    = portDefault &= help "Port number - (43982)"
    , host    = "localhost" &= help "Server host name"
    , marginals = False &= help "Tag with marginal probabilities" }
    &= help "Starts a client which sends requests of tokenized sentences, received from stdin, to server."


compareMode :: Concraft
compareMode = Compare
    { refPath   = def &= argPos 1 &= typ "REFERENCE-FILE"
    , otherPath = def &= argPos 2 &= typ "OTHER-FILE"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    }
    &= help "Compares the two given files -- meant to be used for accuracy checking of the model."


pruneMode :: Concraft
pruneMode = Prune
    { inModel   = def &= argPos 0 &= typ "INPUT-MODEL"
    , outModel  = def &= argPos 1 &= typ "OUTPUT-MODEL"
    , threshold = 0.05 &=
        help "Remove disambiguation features below the threshold - (0.05)" }
    &= help "Prunes (removes) the features of the model (in log-domain) with weights below the given threshold."

argModes :: Mode (CmdArgs Concraft)
argModes = cmdArgsMode $ modes
    [trainMode, tagMode, serverMode, clientMode, compareMode, pruneMode]
    &= summary concraftDesc
    &= program "concraft-hr"


---------------------------------------
-- Main
---------------------------------------


main :: IO ()
main = exec =<< cmdArgsRun argModes


exec :: Concraft -> IO ()


exec Train{..} = do
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/tagset.cfg"
        Just x  -> return x
    tset <- parseTagset tagsetPath' <$> readFile tagsetPath'
    let train0 = parseTrain tset trainPath
    let eval0  = case evalPath of
            Nothing -> return []
            Just x  -> parseTrain tset x
    let alyzer = fmap A.load inAnalyzer
    let ean    = A.create tset A.emptyConf []  -- dirty `undefined` bugfix
    analyzer <- fromMaybe (if noAna then return ean else error errMsg) alyzer
    let af = analyze $ A.getTags analyzer -- analysis function
    let train1 = if noAna then train0 else map af <$> train0
    let eval1  = if noAna then eval0  else map af <$> eval0

    concraft <- C.train (trainConf tset) train1 eval1
    unless (null outModel) $ do
        putStrLn $ "\nSaving model in " ++ outModel ++ "..."
        C.saveModel outModel concraft
  where
    analyze af sent = X.addAnalysis sent (map (af . X.orth . X.word) sent)
    gsgdArgs = SGD.SgdArgs
        { SGD.batchSize = gBatchSize
        , SGD.regVar = gRegVar
        , SGD.iterNum = gIterNum
        , SGD.gain0 = gGain0
        , SGD.tau = gTau }
    dsgdArgs = SGD.SgdArgs
        { SGD.batchSize = dBatchSize
        , SGD.regVar = dRegVar
        , SGD.iterNum = dIterNum
        , SGD.gain0 = dGain0
        , SGD.tau = dTau }
    trainConf tagset = C.TrainConf
        { tagset    = tagset
        , gSgdArgs  = gsgdArgs
        , dSgdArgs  = dsgdArgs
        , reana     = not noAna
        , onDisk    = disk
        , guessNum  = guessNum
        , r0        = r0 }
    errMsg = "Please define the path to analyzer or set --noana!\n\
             \Without the analyzer it isn't known whether the words \
             \are out-of-dictionary or not, so automatically all are\
             \ marked as out-of-dictionary. This can impact the performance."

exec Tag{..} = do
    cft    <- C.loadModel inModel
    let alyzer = fmap A.load inAnalyzer
    let ean    = A.create (tagset cft) A.emptyConf [] -- dirty `undefined` bugfix
    analyzer <- fromMaybe (if noAna then return ean else error errMsg) alyzer
    let tset = tagset cft
    inp <- parseContent tset <$> L.getContents
    out <- R.short analyzer cft $ rq $ buildReq inp
    showData tset $ fmap (addTags inp) out
  where
    -- | The @x@ is sentence of words and possible interpretations.
    -- If the chosen tagging mod isn't marginals, it is expected that
    -- the result of the tagging is a list of tuples @(a,b)@ the
    -- set of possible tags (which were guessed by the guessing model or
    -- as a result of analysis from the 'Analyzer') @a@, and the disambiguated
    -- tag @b@. The 'X.Seg' is then zipped with a tag to prepare the results
    -- for the output.
    addTags x = zip x . map (map snd)
    -- | Builds a request and adds the body which will be sent to the server.
    rq x = R.Request { R.rqBody = x }
    errMsg = "Please define the path to analyzer or set --noana!\n\
             \Without the analyzer it isn't known whether the words \
             \are out-of-dictionary or not, so automatically all are\
             \ marked as out-of-dictionary. This can impact the performance."
    -- | Depending on the no-analysis and marginals boolean values a proper
    -- constructor is chosen. Constructors having an A suffix obviously are
    -- used to indicate that additional analysis is needed.
    buildReq =
        case (noAna, marginals) of
            (True,True)  -> R.Marginal
            (True,False) -> R.PlainTag
            (False,True) -> R.MarginalA
            (False,False)-> R.PlainTagA

exec Server{..} = do
    putStr "Loading model..." >> hFlush stdout
    concraft <- C.loadModel inModel
    putStr "Loading analyzer..." >> hFlush stdout
    let alyzer = fmap A.load inAnalyzer
    analyzer <- fromMaybe (error errMsg) alyzer
    putStrLn " done"
    let portNum = N.PortNumber $ fromIntegral port
    putStrLn $ "Listening on port " ++ show port
    S.runConcraftServer analyzer concraft portNum
    where
        errMsg = "Please define the path to analyzer!\n"

exec Client{..} = do
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/tagset.cfg"
        Just x  -> return x
    tset <- parseTagset tagsetPath' <$> readFile tagsetPath'
    let portNum = N.PortNumber $ fromIntegral port
    inp <- parseContent tset <$> L.getContents
    out <- S.submit host portNum $ rq $ buildReq inp

    showData tset (fmap (addTags inp) out)
  where
    -- | The @x@ is sentence of words and possible interpretations.
    -- If the chosen tagging mod isn't marginals, it is expected that
    -- the result of the tagging is a list of tuples @(a,b)@ the
    -- set of possible tags (which were guessed by the guessing model or
    -- as a result of analysis from the 'Analyzer') @a@, and the disambiguated
    -- tag @b@. The 'X.Seg' is then zipped with a tag to prepare the results
    -- for the output.
    addTags x = zip x . map (map snd)
    -- | Builds a request and adds the body which will be sent to the server.
    rq x = R.Request
        { R.rqBody = x}
    -- | Depending on the no-analysis and marginals boolean values a proper
    -- constructor is chosen. Constructors having an A suffix obviously are
    -- used to indicate that additional analysis is needed.
    buildReq =
        case (noAna, marginals) of
            (True,True)  -> R.Marginal
            (True,False) -> R.PlainTag
            (False,True) -> R.MarginalA
            (False,False)-> R.PlainTagA

exec Compare{..} = do
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/tagset.cfg" -- if no path is given the default one is used
        Just x  -> return x
    tset <- parseTagset tagsetPath' <$> readFile tagsetPath'
    let convert = concat
    xs <- convert <$> parseFile tset refPath
    ys <- convert <$> parseFile tset otherPath
    let s = Acc.weakLB tset xs ys
    putStrLn $ "Number of segments in reference file: " ++ show (Acc.gold s)
    putStrLn $ "Number of correct tags: " ++ show (Acc.good s)
    putStrLn $ "Weak accuracy lower bound: " ++ show (Acc.accuracy s)


exec Prune{..} = do
    cft <- C.loadModel inModel
    C.saveModel outModel $ C.prune threshold cft

---------------------------------------
-- Reading files
---------------------------------------

parseFileG :: ([L.Text] -> X.Sent X.Word P.Tag)
           -> FilePath
           -> IO [X.Sent X.Word P.Tag]
parseFileG packSent path =
    map packSent . parseText <$> L.readFile path

-- | Assumes that the given 'FilePath' is a file containing the training set.
-- It is also assumed that the sentences in the file are clearly separated by
-- at least two whitespaces (be it newlines or spaces) and that each word is
-- in its own row and any tags should follow the word (in the same row) separated
-- by one or more whitespaces.
parseTrain :: P.Tagset -> FilePath -> IO [X.Sent X.Word P.Tag]
parseTrain tset = parseFileG (X.packSentT tset)

-- | It is assumed that the sentences in the file are clearly separated by
-- at least two whitespaces (be it newlines or spaces) and that each word is
-- in its own row and any tags should follow the word (in the same row) separated
-- by one or more whitespaces.
parseFile :: P.Tagset -> FilePath -> IO [X.Sent X.Word P.Tag]
parseFile tset = parseFileG (X.packSent tset)


---------------------------------------
-- Parsing
---------------------------------------

-- | Parses a given 'L.Text'. Function is used to prepare the data for packing
-- into 'X.Sent'. Used by 'parseFile' and 'parseTrain'.
parseText :: L.Text -> [[L.Text]]
parseText = filter (not . null) . Split.splitWhen (L.all Char.isSpace) . L.lines

-- | Used as a function which parses the formated input as described in 'parseTrain' or
-- 'parseFile' but doesn't except a 'FilePath'.
parseContent :: P.Tagset -> L.Text -> [X.Sent X.Word P.Tag]
parseContent tset = map (X.packSent tset) . parseText

---------------------------------------
-- Displaying data
---------------------------------------

-- | Displays the data of tagged sentences.
-- If tagging was done using marginal probabilities all the tags are shown
-- with their corresponding probabilities - format WORD(\tPROB\tTAG)+\n.
-- Otherwise, WORD\tTAG\n format is used and sentences are separated with a newline character.
showData :: P.Tagset
         -> Either [X.Sent X.Word P.Tag] [(X.Sent X.Word P.Tag, [P.Tag])]
         -> IO ()
showData tset (Left xss) =
    forM_ xss (\sentence -> do
        forM_ sentence (\wrd -> do
            T.putStr $ (X.orth . X.word) wrd
            forM_ (M.toList . X.unWMap $ X.tags wrd) (\(tg,prob) -> do
                T.putStr "\t"
                T.putStr $ toShortest prob
                T.putStr "\t"
                T.putStr . T.filter (/=':') . P.showTag tset $ tg
                -- ^^ Text form of a tag is filtered because by default
                -- the separator between POS and attributes is ':'
                -- TODO remove the dependency on the 'P.showTag'
                )
            T.putStr "\n"
            )
        T.putStr "\n"
        )
showData tset (Right xss) =
    forM_ xss (\(sent, tgs) -> do
        forM_ (zip sent tgs) (\(wrd, tg) -> do
            T.putStr $ (X.orth . X.word) wrd
            T.putStr "\t"
            T.putStr . T.filter (/=':') . P.showTag tset $ tg
            T.putStr "\n"
            )
        T.putStr "\n"
    )