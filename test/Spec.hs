import           Test.Groot.Console
import           Test.Groot.Internal.Data.Filter
import           Test.Groot.Types

main :: IO ()
main = describeFilterInstances >> describeTypes >> describeConsole
