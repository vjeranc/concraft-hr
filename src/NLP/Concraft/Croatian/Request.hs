{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.Croatian.Request
(
-- * Request
  Request (..)
-- ** Short
, TagWork (..)
, short
) where


import           Control.Applicative ((<$>))
import qualified Data.Binary as B

import           NLP.Concraft.Croatian
import           NLP.Concraft.Croatian.Morphosyntax
import           NLP.Concraft.Morphosyntax          hiding (Word, orth)
import qualified Data.Tagset.Positional as P
import qualified Data.Set as S
import qualified Data.Text as T
import           NLP.Morphosyntax.Analyzer

-------------------------------------------------
-- Configuration
-------------------------------------------------


-- | A request with configuration.
newtype Request t = Request {
    -- | The actual request.
      rqBody    :: t
    }


instance B.Binary t => B.Binary (Request t)  where
    put Request{..} = B.put rqBody
    get = Request <$> B.get



-------------------------------------------------
-- Short request
-------------------------------------------------


-- | A type of tagging work which is requested.
data TagWork
    = Marginal [Sent Word P.Tag]
    | PlainTag [Sent Word P.Tag]
    | MarginalA [Sent Word P.Tag]
    | PlainTagA [Sent Word P.Tag]

instance B.Binary TagWork where
    put (Marginal x)  = B.putWord8 0 >> B.put x
    put (PlainTag x)  = B.putWord8 1 >> B.put x
    put (MarginalA x) = B.putWord8 2 >> B.put x
    put (PlainTagA x) = B.putWord8 3 >> B.put x
    get =
        B.getWord8 >>= \x -> case x of
            0   -> Marginal <$> B.get
            1   -> PlainTag <$> B.get
            2   -> MarginalA <$> B.get
            _   -> PlainTagA <$> B.get

-- | Process the short request with or without analysis.
short :: Analyzer         -- ^ Morphosyntactic analyzer, if analysis needs to be done.
      -> Concraft         -- ^ Trained concraft model, needed for tagging.
      -> Request TagWork  -- ^ Tagwork of sorts, be it in need of tagging with marginals or tagging with reanalysis.
      -> IO (Either [Sent Word P.Tag] [[(S.Set P.Tag, P.Tag)]])
short analyzer concraft Request{..} = do
    let anaf = getTags analyzer
    return $ case rqBody of
                Marginal x  -> Left  $ map (marginals concraft) x
                PlainTag x  -> Right $ map (tag concraft) x
                MarginalA x -> Left  $ map (marginals concraft) (addAna anaf x)
                PlainTagA x -> Right $ map (tag concraft) (addAna anaf x)

-- | Adds the analysis to the list of sentences.
addAna :: (T.Text -> S.Set P.Tag) -- analysis function
       -> [Sent Word P.Tag]       -- sentences without analyzed words
       -> [Sent Word P.Tag]       -- analysis is added
addAna ana x = zipWith addAnalysis x (map (map (ana . orth . word)) x)
