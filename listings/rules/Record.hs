{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonoPatBinds            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE UndecidableInstances      #-}

module PRX.Audit.Condition.Record
  ( RecordConditionSYM(has),
  , fromJSONRecordConditionOpen, fromJSONRecordCondition
  , Record(Record), unRecord
  , evalWithData
  )
where

import Control.Category           ((.))
import Control.Monad              (Monad, join, liftM, return)
import Control.Monad.Error.Class  (MonadError(type ErrorType))
import Control.Monad.Reader.Class (MonadReader(type EnvType), asks)
import Control.Monad.Trans.Reader (runReader)
import Data.Aeson.Types           (Value(Object), object, toJSON)
import Data.Bool                  (Bool)
import Data.Function              (($), const, fix)
import Data.List                  (elem)
import Data.Map                   (Map, lookup)
import Data.Maybe                 (Maybe(Just), maybeToList)
import Data.Monoid                ((<>))
import Data.String                (String)
import Data.Text                  (Text)
import PRX.Audit.Condition        ((..:), ConditionSYM, EvalCondition(EvalCondition, evalCondition), ShowCondition(ShowCondition), fromJSONConditionOpen)



-- Clase simántica para lenguajes embebidos con referencias a objetos en un registro

infix 4 `has`

class RecordConditionSYM repr where
  has :: Text -> Text -> repr



-- Representación textual

instance RecordConditionSYM ShowCondition where
  key `has` value
    = ShowCondition . const $ key <> " has " <> value



-- Serialización a JSON

instance RecordConditionSYM Value where
  key `has` value
    = object
      [ ("key"  , toJSON key  )
      , ("value", toJSON value)
      ]



-- Deserialización desde JSON

fromJSONRecordConditionOpen
  ::( MonadError m
    , ErrorType m ~ String
    , ConditionSYM repr
    , RecordConditionSYM repr
    )
  => (Value -> m repr)
  -> (Value -> m repr)

fromJSONRecordConditionOpen self v
  = case v of
    Object o
      | Just key   ← o ..: "key"
      , Just value ← o ..: "value"
      -> return
      $ key `has` value

    _ -> fromJSONConditionOpen self v


fromJSONRecordCondition
  ::( MonadError m
    , ErrorType m ~ String
    , ConditionSYM repr
    , RecordConditionSYM repr
    )
  => Value -> m repr

fromJSONRecordCondition = fix fromJSONRecordConditionOpen



-- Evaluación

newtype Record
  = Record { unRecord :: Map Text [Text] }

instance (MonadReader m, EnvType m ~ Record) => RecordConditionSYM (EvalCondition (m Bool)) where
  key `has` value
    = EvalCondition
    . asks
    $ (value `elem`)
    . join
    . maybeToList
    . lookup key
    . unRecord


evalWithData
  ::( MonadError m
    , ErrorType m ~ String
    )
  => Map Text [Text] -> Value -> m Bool

evalWithData record
  = liftM ((`runReader` Record record) . evalCondition)
  . fromJSONRecordCondition
