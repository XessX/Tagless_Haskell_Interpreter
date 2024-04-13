{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Haskell_Full_check (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Haskell_Full_check"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A tagless haskell interpreter"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
