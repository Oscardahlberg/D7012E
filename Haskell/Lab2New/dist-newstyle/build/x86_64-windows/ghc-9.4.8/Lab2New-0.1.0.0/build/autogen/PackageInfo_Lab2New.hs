{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Lab2New (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Lab2New"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "n"
copyright :: String
copyright = ""
homepage :: String
homepage = "n"
