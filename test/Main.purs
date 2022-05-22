module Test.Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Writer.Class (tell)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Uncurried.Writer (execWriter)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "RWSET" do
    describe "Instances" do
      describe "MonadRec" do
        it "should properly accumulate values"
          let
            action = tailRecM case _ of
              0 ->
                pure $ Done unit
              n ->
                tell "erin!" $> Loop (n - 1)
          in
            execWriter (action 2) `shouldEqual` "erin!erin!"
