{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Morphosyntax data layer in Croatian.
module NLP.Concraft.Croatian.Morphosyntax
(
  packSent
, packSentT
, addAnalysis
, extractSentences
, transformToConfig
, Word(..)
, ListLike(..)
) where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Tagset.Positional as P
import           GHC.Generics
import           Data.String (IsString)
import           Data.Binary (Binary(..))
import qualified NLP.Concraft.Morphosyntax as X
import           NLP.Concraft.Morphosyntax (Seg(..)) -- for easier handling of Seg
import qualified Data.Map as M
import qualified Data.Set as S

-- | Representation of a word.
data Word = Word {
    orth :: T.Text -- ^ Orthographic (plainly normal) form.
  , oov :: Bool    -- ^ Indicates whether a word is out-of-dictionary or not.
                   -- It is assumed that the word is out-of-dictionary if no
                   -- tags were provided for the word. If additional analysis
                   -- gives a non-empty set of possible tags this value should
                   -- (and is in this tagger) change the value accordingly.
  }
    deriving (Show,Generic, Eq, Ord)

-- | Instance needed for the use of the concraft model.
instance X.Word Word where
    orth = orth
    oov  = oov

instance Binary Word   -- Needed for the Client-Server communication.
instance FromJSON Word -- Needed for the concraft model.
instance ToJSON Word   -- Needed for the concraft model.

-- | Orphan instance needed for Client-Server communcation.
-- Used primarily in 'NLP.Concraft.Croatian.Request'.
-- Could be moved to concraft library.
instance (Binary a, Binary b) => Binary (Seg a b) where
    put (Seg x ts) = put x >> put ts
    get = do
        x <- get
        y <- get
        return $ Seg x y

-- | Used to allow use of same functions on lazy and strict
-- inputs. It is assumed that the function behave as they do
-- in 'T.Text', 'L.Text' or 'String' modules.
class (Data.String.IsString a) => ListLike a where
    tcintersperse :: Char -> a -> a
    tcmap :: (Char -> Char) -> a -> a
    strict :: a -> T.Text
    tcwords :: a -> [a]
    tcsplitOn :: a -> a -> [a]
    tcnull :: a -> Bool
    tclines :: a -> [a]

instance ListLike T.Text where
    tcintersperse = T.intersperse
    tcmap = T.map
    strict = id
    tcwords = T.words
    tcsplitOn = T.splitOn
    tcnull = T.null
    tclines = T.lines

instance ListLike L.Text where
    tcintersperse = L.intersperse
    tcmap = L.map
    strict = L.toStrict
    tcwords = L.words
    tcsplitOn = L.splitOn
    tcnull = L.null
    tclines = L.lines

-- | Transforms a given string to a model suited string.
-- Ex. Nsmnn -> N:s:m:n:n, or Vmp-sf -> V:m:p:9:s:f, all
-- '-' to '9'.
transformToConfig :: ListLike a => a -> a
transformToConfig = tcintersperse ':' . tcmap toNine
  where toNine x = if x `elem` "=-" then '9' else x

-- | Given a sentence and a list of tags for each word this function adds
-- the tags.
addAnalysis :: X.Sent Word P.Tag -> [S.Set P.Tag] -> X.Sent Word P.Tag
addAnalysis = zipWith f
    where f seg tgs = seg { word = (word seg) { oov = M.null unionWP } -- out-of-dictionary if no analysis
                          , tags = X.mkWMap . M.toList $ unionWP}      -- TODO inefficient
            where wtagsMap = M.fromList $ zip (S.toList tgs) [0,0..]   -- weighted analysis tags
                  ptagsMap = X.unWMap . tags $ seg                     -- previous tags
                  unionWP  = M.unionWith const ptagsMap wtagsMap       -- if tag exists discard the analysis

-- | Given a tagset and a list of words it packs them into
-- 'X.Sent' data from, used by the tagging model. It is assumedd
-- that all of the tags do not have any prior probabilites. If
-- this was used on the training set the function wouldn't differentiate
-- correct from possible tags.
packSent :: ListLike a => P.Tagset -> [a] -> X.Sent Word P.Tag
packSent = packSentP [0.0,0.0..]

-- | Packs the training data to sentences with the first tag having
-- the highest probability. Suitable for using on the training set.
packSentT :: ListLike a => P.Tagset -> [a] -> X.Sent Word P.Tag
packSentT = packSentP $ 1.0 : [0.0,0.0..]

-- | Line number saver.
packSentP :: ListLike a => [Double] -> P.Tagset -> [a] -> X.Sent Word P.Tag
packSentP dist tset = map (packSegP tset dist)

-- | Given a string, a tagset returns a segment with
-- possible tags having the default double weight. TODO For language-agnostic
-- use the dependency 'transformToConfig' should be removed and a proper
-- parser for 'P.Tagset' and 'P.Tag' should be written.
packSegP :: ListLike a => P.Tagset -> [Double] -> a -> X.Seg Word P.Tag
packSegP tset dist xs = X.Seg {word=mywrd, tags=wmap}
  where (w:tagxs) = tcwords xs
        wmap      = X.mkWMap $ zip rtags dist
        rtags     = map (P.parseTag tset . strict . transformToConfig) tagxs
        mywrd     = Word {orth=strict w, oov=null rtags}

-- | Extracts sentences from a given input. Rarely used since it's
-- not always the case that we can assume the sentences are separated
-- only by two newline characters.
extractSentences :: ListLike a => a -> [[a]]
extractSentences =
    map tclines . filter (not . tcnull) . tcsplitOn "\n\n"
