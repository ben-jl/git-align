import Test.ConstructorSpec  (constructorSpec)
import Test.ParsingSpec (gitParsingSpec)
import Prelude (IO)
main :: IO ()
main = do
    constructorSpec
    gitParsingSpec
