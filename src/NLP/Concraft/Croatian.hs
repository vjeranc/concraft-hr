{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.Concraft.Croatian
(
-- * Model
  C.Concraft
, C.saveModel
, C.loadModel

-- * Tagging
, tag
, marginals

-- * Training
, TrainConf (..)
, train

-- * Pruning
, C.prune

) where


import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.Tagset.Positional as P
import qualified Numeric.SGD as SGD

import qualified NLP.Concraft.Morphosyntax as X
import qualified NLP.Concraft.Schema as S
import           NLP.Concraft.Schema (SchemaConf(..), entry, entryWith)
import qualified NLP.Concraft.Guess as G
import qualified NLP.Concraft.Disamb as D
import qualified NLP.Concraft as C

import           NLP.Concraft.Croatian.Morphosyntax


-------------------------------------------------
-- Default configuration
-------------------------------------------------


-- | Default configuration for the guessing observation schema. This configuration
-- serves as a definition for the feature function creation of the conditional random
-- fields model that guesses a set of possible tags for unknown words. Unknown words
-- do not have a set of possible tags before the tagging procedure (analyzer didn't
-- output a single possible tag).
guessSchemaDefault :: SchemaConf
guessSchemaDefault = S.nullConf -- TODO make this configurable from the real world
    { lowPrefixesC  = entryWith [1, 2]      [0]
    , lowSuffixesC  = entryWith [1, 2]      [0]
    , knownC        = entry                 [0]
    , begPackedC    = entry                 [0] }


-- | Default configuration for the guessing observation schema. This configuration
-- serves as a definition for the feature function creation of the conditional random
-- fields model that disambiguates the correct tag from the set of possible ones.
disambSchemaDefault :: SchemaConf
disambSchemaDefault = S.nullConf -- TODO make this configurable from the real world
    { lowOrthC      = entry                         [-2, -1, 0, 1]
    , lowPrefixesC  = oov $ entryWith [1, 2, 3]     [0]
    , lowSuffixesC  = oov $ entryWith [1, 2, 3]     [0]
    , begPackedC    = oov $ entry                   [0] }
  where
    oov (Just body) = Just $ body { S.oovOnly = True }
    oov Nothing     = Nothing


-- | Default tiered tagging configuration. This configuration serves as a
-- definition for the feature function creation of the conditional random
-- fields model.
tiersDefault :: [D.Tier]
tiersDefault = -- TODO make this configurable from the real world
    [tier1
    ,tier2
    ,tier3
    ]
  where
    tier1 = D.Tier True  $ S.fromList ["ntype","vtype","atype","ptype","rtype","ctype","mtype","qtype","xtype"]
    tier2 = D.Tier False $ S.fromList ["scase", "case","gender","animate"]
    tier3 = D.Tier False $ S.fromList
      ["cformation", "mform", "pclitic","person","person1",
       "preferenttype", "psyntactictype","adefiniteness",
       "vform","vnegative", "number", "number1","degree"]


-------------------------------------------------
-- Tagging
-------------------------------------------------


-- | Tag the analysed sentence. it is expected that
-- the result of the tagging is a list of tuples @(a,b)@ the
-- set of possible tags (which were guessed by the guessing model or
-- as a result of analysis from the 'Analyzer') @a@, and the disambiguated
-- tag @b@.
tag :: C.Concraft -> X.Sent Word P.Tag -> [(S.Set P.Tag, P.Tag)]
tag = C.tag


-- | Tag the sentence with marginal probabilities. The resulting sentence
-- contains the probabilites of each tag given in the set.
marginals :: C.Concraft -> X.Sent Word P.Tag -> X.Sent Word P.Tag
marginals concraft sent = addMarginals sent wmaps
  where wmaps = C.marginals concraft sent

-- | Adds the tags recevied by the 'marginals' tagging function.
-- Marginal probabilites of each tag are added
-- (or modified if there was any data previously).
addMarginals :: X.Sent Word P.Tag  -- ^ Sentence of words that can contain tags for each word.
             -> [X.WMap P.Tag]     -- ^ Map of tags as keys to their marginal probabilities.
             -> X.Sent Word P.Tag  -- ^ Sentence with additional or modified tags (probabilities get changed)
addMarginals = zipWith f
    where f seg tgs  = seg { X.tags = X.mkWMap $ M.toList unionWP} -- TODO inefficient but hidden in mkWMap
            where wtagsMap = X.unWMap tgs            -- weighted analysis tags
                  ptagsMap = X.unWMap . X.tags $ seg -- previous tags
                  unionWP  = M.unionWith (flip const) ptagsMap wtagsMap -- if tag exists keep analysis
-------------------------------------------------
-- Training
-------------------------------------------------


-- | Training configuration.
data TrainConf = TrainConf {
      tagset    :: P.Tagset
    -- ^ Tagset.

    , gSgdArgs   :: SGD.SgdArgs
    -- ^ SGD parameters for the guessing model.

    , dSgdArgs   :: SGD.SgdArgs
    -- ^ SGD parameters for the disambiguation model.

    , reana     :: Bool
    -- ^ Perform reanalysis.

    , onDisk    :: Bool
    -- ^ Store SGD dataset on disk.

    , guessNum  :: Int
    -- ^ Number of guessed tags for each word. The guessing model will
    -- output the possible tags with their probabilities. The first 'guessNum',
    -- sorted in the descending order by the probability,
    -- will form the whole set of possible tags.

    , r0        :: G.R0T
    -- ^ `G.r0T` parameter.
    }

-- | Begins the training of the model, if evaluation data is supplied, the
-- periodic report will contain an optimistic accuracy of the model.
-- Optimistic in the sense of taking the given correct tag and set of
-- possible tags as the complete results of the analysis. It is possible that the
-- morphosyntactic analyzer used to provide the set of possible tags doesn't
-- include the correct tag in the set. Hence, the optimistic accuracy report.
train
    :: TrainConf                   -- ^ Training configuration
    -> IO [X.Sent Word P.Tag]      -- ^ Training data
    -> IO [X.Sent Word P.Tag]      -- ^ Evaluation data
    -> IO C.Concraft               -- ^ Trained model
train TrainConf{..} = C.train tagset guessNum guessConf disambConf
  where
    guessConf  = G.TrainConf guessSchemaDefault gSgdArgs onDisk r0
    disambConf = D.TrainConf tiersDefault disambSchemaDefault dSgdArgs onDisk