import Test.ConstructorSpec  (constructorSpec)
import Test.ParsingSpec (gitParsingSpec)
import Test.GraphTypeSpec (graphTypeSpec)
import Prelude (IO)
main :: IO ()
main = do
    graphTypeSpec
    constructorSpec
    gitParsingSpec
