import OCaml.Export
import Shared.Types.Reason.Types
import Test.Hspec

main :: IO ()
main = do
  mkGoldenFiles (Proxy :: Proxy SharedTypesPackage) 100 "test/golden"

  hspec $ runGoldenSpec (Proxy :: Proxy SharedTypesPackage) 100 "test/golden"
