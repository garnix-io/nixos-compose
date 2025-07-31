module SpecHook where

import System.Environment
import Test.Hspec

hook :: Spec -> Spec
hook = aroundAll_ (withProgName "nixos-compose")
