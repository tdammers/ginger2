{-# LANGUAGE DeriveFunctor #-} {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ginger.Interpret
( Eval (..)
, EvalState (..)
, evalE
, evalS
, evalSs
, evalT
, GingerT (..)
, runGingerT
, defEnv
, defContext
, setVar
, lookupVar
, lookupVarMaybe
, stringify
, scoped
 ,bind
, valuesEqual
, RuntimeError (..)
)
where

import Language.Ginger.Interpret.Type
import Language.Ginger.Interpret.DefEnv
import Language.Ginger.Interpret.Eval
import Language.Ginger.RuntimeError
