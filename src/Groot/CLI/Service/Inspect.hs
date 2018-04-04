{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Groot.CLI.Service.Inspect
     ( ServiceInspectOpts
     , serviceInspectOpts
     , runServiceInspect
     ) where

import           Control.Lens                 hiding (argument)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Network.AWS
import qualified Network.AWS.ECS              as ECS
import           Options.Applicative
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import           Groot.AWS
import           Groot.CLI.Common
import           Groot.Core
import           Groot.Data.Text              (ToText, toText)
import           Groot.Exception
import           Groot.Types

data ServiceInspectOpts =
  ServiceInspectOpts (Maybe ClusterRef) ContainerServiceRef
  deriving (Eq, Show)

serviceNameArg :: Parser ContainerServiceRef
serviceNameArg = fromString <$> argument str (metavar "SERVICE")

serviceInspectOpts :: Parser ServiceInspectOpts
serviceInspectOpts = ServiceInspectOpts
                 <$> optional clusterOpt
                 <*> serviceNameArg

defaultIndent :: Int
defaultIndent = 3

pprintService :: ECS.ContainerService -> Doc
pprintService service = Doc.vsep [
    Doc.bold . Doc.dullblue $ maybe Doc.empty (Doc.text . T.unpack) $ service ^. ECS.csServiceName
  , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
        service ^. ECS.csStatus . label ppStatus "Status:"
      , service ^. ECS.csRunningCount . plain "Running:"
      , service ^. ECS.csDesiredCount . plain "Desired:"
      , service ^. ECS.csPendingCount . plain "Pending:"
      , (Just . Doc.blue . Doc.text $ "ARNs:")
      , Just $ Doc.indent defaultIndent (Doc.vsep $ catMaybes [
          service ^. ECS.csServiceARN . label ppArn "Service:"
        , service ^. ECS.csTaskDefinition . label ppArn "Task:"
        , service ^. ECS.csClusterARN . label ppArn "Cluster:"
        , service ^. ECS.csRoleARN . label ppArn "Role:"
      ])
      , service ^. ECS.csCreatedAt . label ppTime "Created:"
      , ppList ppLoadBalancer "Load Balancers:" $ service ^. ECS.csLoadBalancers
      , ppList ppDeployment "Deployments:" $ service ^. ECS.csDeployments
      , ppList ppPlacementStrategy "Placement Strategies:" $ service ^. ECS.csPlacementStrategy
      , ppList ppPlacementConstraint "Placement Constraints" $ service ^. ECS.csPlacementConstraints
    ])
  ]
  where label :: (a -> Doc) -> Text -> Getter (Maybe a) (Maybe Doc)
        label f lb = to (\x -> (\y -> (Doc.blue . Doc.text . T.unpack $ lb) <+> (f y)) <$> x)

        plain :: ToText a => Text -> Getter (Maybe a) (Maybe Doc)
        plain = label (Doc.text . T.unpack . toText)

        ppStatus :: Text -> Doc
        ppStatus txt@"ACTIVE" = Doc.bold . Doc.green . Doc.text . T.unpack $ txt
        ppStatus txt          = Doc.bold . Doc.red . Doc.text . T.unpack $ txt

        ppArn :: Text -> Doc
        ppArn = Doc.underline . Doc.text . T.unpack . toText

        ppTime :: UTCTime -> Doc
        ppTime = Doc.yellow . Doc.text . formatTime defaultTimeLocale "%d/%m/%Y %T"

        ppList :: (a -> Doc) -> Text -> [a] -> Maybe Doc
        ppList _ _   [] = Nothing
        ppList f lbl xs = Just $ Doc.vsep [
              Doc.blue . Doc.text . T.unpack $ lbl
            , Doc.indent defaultIndent (Doc.vsep $ f <$> xs)
          ]

        ppLoadBalancer :: ECS.LoadBalancer -> Doc
        ppLoadBalancer lb = Doc.vsep [
            Doc.bold . Doc.dullblue $ maybe Doc.empty (Doc.text . T.unpack) $ lb ^. ECS.lbContainerName
          , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
                lb ^. ECS.lbContainerPort . plain "Port:"
              , lb ^. ECS.lbLoadBalancerName . plain "ELB:"
              , lb ^. ECS.lbTargetGroupARN . label ppArn "Target Group:"
            ])
          ]

        ppDeployment :: ECS.Deployment -> Doc
        ppDeployment dep = Doc.vsep [
              Doc.bold . Doc.dullblue $ maybe Doc.empty (Doc.text . T.unpack) $ dep ^. ECS.dId
            , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
                  dep ^. ECS.dTaskDefinition . label ppArn "Task:"
                , dep ^. ECS.dRunningCount . plain "Running:"
                , dep ^. ECS.dDesiredCount . plain "Desired:"
                , dep ^. ECS.dUpdatedAt . label ppTime "Updated:"
                , dep ^. ECS.dCreatedAt . label ppTime "Created:"
              ])
            ]
        
        ppPlacementStrategy :: ECS.PlacementStrategy -> Doc
        ppPlacementStrategy ps = Doc.cyan $ Doc.hsep $ catMaybes [
              Doc.text . T.unpack . toText <$> ps ^. ECS.psType
            , Doc.text . T.unpack <$> ps ^. ECS.psField
          ]

        ppPlacementConstraint :: ECS.PlacementConstraint -> Doc
        ppPlacementConstraint pc = Doc.cyan $ Doc.hsep $ catMaybes [
             Doc.text . T.unpack . toText <$> pc ^. ECS.pcType
           , Doc.text . T.unpack <$> pc ^. ECS.pcExpression
         ]

runServiceInspect :: ServiceInspectOpts -> GrootM IO ()
runServiceInspect (ServiceInspectOpts clusterRef serviceRef) = do
  env <- ask
  xs <- runResourceT . runAWS env $ runMaybeT $ findService serviceRef clusterRef
  case xs of
    Nothing -> throwM $ serviceNotFound serviceRef clusterRef
    Just  s -> liftIO . Doc.putDoc $ pprintService s
