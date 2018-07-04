module Groot.CLI.Introspect
     ( IntrospectOpts
     , introspectOpts
     , runIntrospect
     ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as BS
import           Data.Semigroup         ((<>))
import           Data.String
import           Data.Yaml
import qualified Options.Applicative    as Opts

import           Groot.Core
import           Groot.Manifest
import           Groot.Types

data IntrospectOpts = IntrospectOpts
  { cluster    :: ClusterRef
  , outputFile :: Maybe FilePath
  } deriving (Eq, Show)

outputFileOpt :: Opts.Parser FilePath
outputFileOpt = Opts.strOption
              ( Opts.long "output"
             <> Opts.short 'o'
             <> Opts.metavar "OUTPUT_FILE"
             <> Opts.help "Output file for the introspected manifest" )

clusterRefArg :: Opts.Parser ClusterRef
clusterRefArg = fromString <$> Opts.argument Opts.str (Opts.metavar "CLUSTER_NAME")

introspectOpts :: Opts.Parser IntrospectOpts
introspectOpts = IntrospectOpts <$> clusterRefArg <*> (Opts.optional outputFileOpt)

runIntrospect :: IntrospectOpts -> GrootIO ()
runIntrospect opts = do
  manifest <- introspectManifest $ cluster opts
  liftIO $ renderManifest (outputFile opts) manifest
  where renderManifest (Just file) = encodeFile file
        renderManifest Nothing     = \x -> do
          yaml     <- pure . BS.unpack . encode $ x
          putStrLn $ yaml
