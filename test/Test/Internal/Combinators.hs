module Test.Internal.Combinators where
    
import Test.Hspec
import Test.QuickCheck

import Internal.Patterns
import Internal.Combinators
import Internal.Parser
    
test :: SpecWith ()
test = describe "Combinators" $ do
        describe "many" $ do
            it "matches 0 or more of a given parser" $ do
                1 `shouldBe` 1