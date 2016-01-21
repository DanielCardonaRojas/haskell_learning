{-
 Sometimes json looks like:
 {
     "id": 1
     "comments": {
         "1": {
             "text": "Comment 1"
         },
         "2": {
             "text": "Comment 2"
         }
     }
 }

 Here is a way to parse it
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import qualified Data.HashMap.Strict as HM

data Post = Post Int [Comment] deriving (Show)
data Comment = Comment String  deriving (Show)

instance FromJSON Comment where
    parseJSON (Object v) = Comment <$> v .: "text"

instance FromJSON Post where
    parseJSON (Object v) =
        Post <$> v .: "id"
             <*> (v .: "comments" >>= vals)

vals :: (FromJSON a) => Object -> Parser [a]
vals = mapM parseJSON . HM.elems

-- Just (Post 1 [Comment "Comment 2",Comment "Comment 1"])
parsedPost :: Maybe Post
parsedPost =
    decode "{\"id\":1,\"comments\":{\"1\":{\"text\":\"Comment 1\"},\"2\":{\"text\":\"Comment 2\"}}}"
