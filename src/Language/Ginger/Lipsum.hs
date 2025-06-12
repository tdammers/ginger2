{-# LANGUAGE OverloadedStrings #-}

module Language.Ginger.Lipsum
where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Monad (replicateM)

availableWords :: [Text]
availableWords =
  [ "a"
  , "ab"
  , "accusamus"
  , "accusantium"
  , "ad"
  , "adipiscing"
  , "alias"
  , "aliquam"
  , "aliquid"
  , "amet"
  , "animi"
  , "aperiam"
  , "architecto"
  , "asperiores"
  , "aspernatur"
  , "assumenda"
  , "at"
  , "atque"
  , "aut"
  , "autem"
  , "beatae"
  , "blanditiis"
  , "commodi"
  , "consectetur"
  , "consequatur"
  , "consequuntur"
  , "corporis"
  , "corrupti"
  , "culpa"
  , "cum"
  , "cumque"
  , "cupiditate"
  , "debitis"
  , "delectus"
  , "deleniti"
  , "deserunt"
  , "dicta"
  , "dignissimos"
  , "distinctio"
  , "do"
  , "dolor"
  , "dolore"
  , "dolorem"
  , "doloremque"
  , "dolores"
  , "doloribus"
  , "dolorum"
  , "dquis"
  , "ducimus"
  , "ea"
  , "eaque"
  , "earum"
  , "eius"
  , "eligendi"
  , "enim"
  , "eos"
  , "error"
  , "ert"
  , "esse"
  , "est"
  , "et"
  , "eum"
  , "eveniet"
  , "ex"
  , "excepturi"
  , "exercitationem"
  , "expedita"
  , "explicabo"
  , "facere"
  , "facilis"
  , "fuga"
  , "fugiat"
  , "fugit"
  , "harum"
  , "hic"
  , "id"
  , "illo"
  , "illum"
  , "impedit"
  , "in"
  , "incididunt"
  , "inventore"
  , "ipsa"
  , "ipsam"
  , "ipsum"
  , "irure"
  , "iste"
  , "itaque"
  , "iusto"
  , "labore"
  , "laboriosam"
  , "laborum"
  , "laudantium"
  , "libero"
  , "magnam"
  , "magni"
  , "maiores"
  , "maxime"
  , "minima"
  , "minus"
  , "modi"
  , "molestiae"
  , "molestias"
  , "mollitia"
  , "nam"
  , "natus"
  , "necessitatibus"
  , "nemo"
  , "neque"
  , "nesciunt"
  , "nihil"
  , "nisi"
  , "nobis"
  , "non"
  , "nostrumd"
  , "nulla"
  , "numquam"
  , "obcaecati"
  , "odio"
  , "odit"
  , "officia"
  , "officiis"
  , "omnis"
  , "optio"
  , "pariatur"
  , "perferendis"
  , "perspiciatis"
  , "placeat"
  , "porro"
  , "possimus"
  , "praesentium"
  , "provident"
  , "quae"
  , "quaerat"
  , "quam"
  , "quas"
  , "quasi"
  , "qui"
  , "quia"
  , "quibusdam"
  , "quidem"
  , "quis"
  , "quisquam"
  , "quo"
  , "quod"
  , "quos"
  , "ratione"
  , "recusandae"
  , "reiciendis"
  , "rem"
  , "repellat"
  , "repellendaus"
  , "reprehenderit"
  , "repudiandae"
  , "rerudum"
  , "rerum"
  , "saepe"
  , "sapiente"
  , "sed"
  , "sequi"
  , "similique"
  , "sint"
  , "sit"
  , "soluta"
  , "sunt"
  , "suscipit"
  , "tempora"
  , "tempore"
  , "temporibus"
  , "tenetur"
  , "totam"
  , "ullam"
  , "unde"
  , "ut"
  , "vel"
  , "velit"
  , "veniam"
  , "veritatis"
  , "vero"
  , "vitae"
  , "voluptas"
  , "voluptate"
  , "voluptatem"
  , "voluptates"
  , "voluptatibus"
  , "voluptatum"
  ]

numWords :: Int
numWords = length availableWords

lipsumM :: MonadRandom m => Int -> Int -> Int -> m Text
lipsumM n minWords maxWords =
  Text.unlines <$> lipsumParasM n minWords maxWords

lipsumParasM :: MonadRandom m => Int -> Int -> Int -> m [Text]
lipsumParasM n minWords maxWords =
  replicateM n (lipsumParaM minWords maxWords)

lipsumParaM  :: MonadRandom m => Int -> Int -> m Text
lipsumParaM minWords maxWords = do
  n <- getRandomR (max minWords 0, max maxWords 0)
  Text.unwords <$> lipsumSentencesM n

lipsumSentencesM  :: MonadRandom m => Int -> m [Text]
lipsumSentencesM maxWords
  | maxWords < 1
  = pure []
  | otherwise
  = do
      n <- min (max maxWords 1) <$> getRandomR (1, max maxWords 1)
      s <- lipsumSentenceM n
      let maxWords' = maxWords - n
      (s:) <$> lipsumSentencesM maxWords'

insertCommas :: MonadRandom m => Int -> [Text] -> m [Text]
insertCommas percentile (w:ws) = do
  let l = length ws
  ws' <- mapM
          (\x -> do
            shouldInsert <- (< percentile) <$> getRandomR (0, 99)
            pure $ if shouldInsert then x <> "," else x
          )
          (take (l - 1) ws)
  pure $ w : ws' ++ drop (l - 1) ws
insertCommas _ ws = pure ws

lipsumSentenceM  :: MonadRandom m => Int -> m Text
lipsumSentenceM n = do
  txt <- Text.unwords <$> (lipsumWordsM n >>= insertCommas 10)
  pure $
    Text.toUpper (Text.take 1 txt) <> Text.drop 1 txt <> "."

lipsumWordsM :: MonadRandom m => Int -> m [Text]
lipsumWordsM n = replicateM n lipsumWordM

lipsumWordM :: MonadRandom m => m Text
lipsumWordM = do
  pick <- getRandomR (0, numWords - 1)
  pure $ availableWords !! pick
