{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonoLocalBinds          #-}
{-# LANGUAGE NoMonoPatBinds            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE UndecidableInstances      #-}

module PRX.Audit.Condition
  ( ConditionSYM(false, true, neg, and, or)
  , ShowContext(And, Or, Neg, Root), ShowCondition(ShowCondition), showCondition
  , toJSONCondition
  , fromJSONConditionOpen, fromJSONCondition
  , EvalCondition(EvalCondition, evalCondition)
  , (..:)
  )
where

import Control.Category          ((.))
import Control.Monad             (Monad, liftM, return)
import Control.Monad.Error.Class (MonadError(type ErrorType), throwError)
import Control.Shortcircuit      (andM, orM)
import Data.Aeson.Types          ((.:), (.=), FromJSON, Object, Value(Bool, Object), object, parseMaybe)
import Data.Bool                 (Bool(False, True), not)
import Data.Foldable             (foldl')
import Data.Function             (($), const, fix, id)
import Data.Functor              ((<$>), fmap)
import Data.Maybe                (Maybe(Just))
import Data.Monoid               ((<>))
import Data.String               (String)
import Data.Text                 (Text, intercalate)
import Data.Traversable          (sequence)
import Text.Show                 (show)

import qualified Data.Foldable as F (and, or)



-- Clase simántica para el lenguaje embebido de expresiones booleanas

class ConditionSYM repr where
  false :: repr
  true  :: repr
  neg   :: repr -> repr
  and   :: [repr] -> repr
  or    :: [repr] -> repr



-- Representación textual

data ShowContext = And | Or | Neg | Root

newtype ShowCondition = ShowCondition { unShowCondition :: ShowContext -> Text }

parenthesize :: Text -> Text
parenthesize t = "(" <> t <> ")"

instance ConditionSYM ShowCondition where
  false = ShowCondition $ const "false"
  true  = ShowCondition $ const "true"
  neg e = ShowCondition $ const $ "~ " <> unShowCondition e Neg

  and l = ShowCondition $ \ case
    And  -> intercalate " ∧ " $ (`unShowCondition` And) <$> l
    Root -> intercalate " ∧ " $ (`unShowCondition` And) <$> l
    _    -> parenthesize $ unShowCondition (and l) Root

  or l = ShowCondition $ \ case
    Or   -> intercalate " ∨ " $ (`unShowCondition` Or) <$> l
    Root -> intercalate " ∨ " $ (`unShowCondition` Or) <$> l
    _    -> parenthesize $ unShowCondition (or l) Root

showCondition :: ShowCondition -> Text
showCondition = (`unShowCondition` Root)



-- Serialización a JSON

instance ConditionSYM Value where
  false = Bool false
  true  = Bool true
  neg e = object ["neg" .= e]
  and l = object ["and" .= l]
  or  l = object ["or"  .= l]

toJSONCondition :: Value -> Value
toJSONCondition = id



-- Deserialización desde JSON

(..:) :: FromJSON b => Object -> Text -> Maybe b
o ..: k = parseMaybe (.: k) o


fromJSONConditionOpen
  ::( MonadError m
    , ErrorType m ~ String
    , ConditionSYM repr
    )
  => (Value -> m repr)
  -> (Value -> m repr)

fromJSONConditionOpen self v
  = case v of
    Bool b
      -> return
      $ if b
        then true
        else false

    Object o
      | Just e ← o ..: "neg" -> neg `liftM` self e
      | Just l ← o ..: "and" -> and `liftM` sequence (self <$> l)
      | Just l ← o ..: "or"  -> or  `liftM` sequence (self <$> l)

    x -> throwError $ "Unexpected term in boolean expression serialized as JSON: " <> show x


fromJSONCondition
  ::( MonadError m
    , ErrorType m ~ String
    , ConditionSYM repr
    )
  => Value -> m repr

fromJSONCondition = fix fromJSONConditionOpen



-- Evaluación

instance ConditionSYM Bool where
  false = False
  true  = True
  neg   = not
  and   = F.and
  or    = F.or

newtype EvalCondition a
  = EvalCondition
    { evalCondition :: a
    }

instance ConditionSYM (EvalCondition Bool) where
  false = EvalCondition false
  true  = EvalCondition true
  neg   = EvalCondition . neg . evalCondition
  and   = EvalCondition . and . fmap evalCondition
  or    = EvalCondition . or  . fmap evalCondition



-- Evaluación por cortocircuito en cualquier Monad

instance Monad m => ConditionSYM (EvalCondition (m Bool)) where
  false = EvalCondition $ return false
  true  = EvalCondition $ return true
  neg   = EvalCondition . liftM neg . evalCondition
  and   = EvalCondition . foldl' andM (return true ) . fmap evalCondition
  or    = EvalCondition . foldl' orM  (return false) . fmap evalCondition
