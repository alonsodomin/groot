import Distribution.Package
import Distribution.License (License(..))
import Distribution.PackageDescription (PackageDescription)
import qualified Distribution.PackageDescription as Pckg
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo

customPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
customPostBuild _ _ pckg _ = do
  putStrLn "So this thing has been built"
  generateRpmSpecFile pckg "bla"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postBuild = customPostBuild }

-- RPM

data RpmSpec = RpmSpec
  { rpmName :: String
  , rpmVersion :: String
  , rpmSummary :: String
  , rpmDescription :: String
  , rpmLicense :: String
  } deriving (Eq, Show)

generateRpmSpecFile :: PackageDescription -> FilePath -> IO ()
generateRpmSpecFile pckg _ = print generateSpec
  where generateSpec = RpmSpec {
            rpmName = unPackageName . pkgName . Pckg.package $ pckg
          , rpmVersion = showVersion . pkgVersion . Pckg.package $ pckg
          , rpmSummary = Pckg.synopsis pckg
          , rpmDescription = Pckg.description pckg
          , rpmLicense = describeLicense . Pckg.license $ pckg
        }

        describeLicense :: License -> String
        describeLicense (GPL v) = "GPL " ++ (maybe "" showVersion v)
        describeLicense (Apache v) = "Apache " ++ (maybe "" showVersion v)
        describeLicense lic = show lic
