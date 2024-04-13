{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lang where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift, Exp, Q)
import Control.Monad (void)
import Test.HUnit
import qualified Data.Text as T
import Control.Monad.Writer hiding (fix)
import qualified Control.Monad.State as CMS
import Control.Applicative (liftA2)
import Data.Kind (Type)