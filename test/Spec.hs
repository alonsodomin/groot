import           Test.Groot.Console
import           Test.Groot.Types
import Test.Groot.Data.Filter

main :: IO ()
main = describeFilterInstances >> describeTypes >> describeConsole
