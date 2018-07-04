{-# LANGUAGE OverloadedStrings #-}

module Groot.CLI.Service.Inspect
     ( ServiceInspectOpts
     , serviceInspectOpts
     , runServiceInspect
     ) where

import           Control.Lens               hiding (argument)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import           Network.AWS
import qualified Network.AWS.ECS            as ECS
import           Options.Applicative

import           Groot.CLI.Common
import           Groot.Console
import           Groot.Core
import           Groot.Exception
import           Groot.Internal.Data.Text   (styled, toText, yellowStyle)
import           Groot.Internal.PrettyPrint (Doc, defaultIndent, (<+>))
import qualified Groot.Internal.PrettyPrint as Doc
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

pprintService :: ECS.ContainerService -> Doc
pprintService service = Doc.vsep [
    Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ service ^. ECS.csServiceName
  , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
        Doc.field Doc.status "Status:" <$> service ^. ECS.csStatus
      , Doc.field' "Running:" <$> service ^. ECS.csRunningCount
      , Doc.field' "Desired:" <$> service ^. ECS.csDesiredCount
      , Doc.field' "Pending:" <$> service ^. ECS.csPendingCount
      , (Just . Doc.blue . Doc.pretty $ ("ARNs:" :: Text))
      , Just $ Doc.indent defaultIndent (Doc.vsep $ catMaybes [
          Doc.field ppArn "Service:" <$> service ^. ECS.csServiceARN
        , Doc.field ppArn "Task:" <$> service ^. ECS.csTaskDefinition
        , Doc.field ppArn "Cluster:" <$> service ^. ECS.csClusterARN
        , Doc.field ppArn "Role:" <$> service ^. ECS.csRoleARN
      ])
      , Doc.field Doc.defaultTime "Created:" <$> service ^. ECS.csCreatedAt
      , Doc.listField ppLoadBalancer "Load Balancers:" $ service ^. ECS.csLoadBalancers
      , Doc.listField ppDeployment "Deployments:" $ service ^. ECS.csDeployments
      , Doc.listField ppPlacementStrategy "Placement Strategies:" $ service ^. ECS.csPlacementStrategy
      , Doc.listField ppPlacementConstraint "Placement Constraints" $ service ^. ECS.csPlacementConstraints
    ])
  ]
  where ppArn :: Text -> Doc
        ppArn = Doc.underline . Doc.pretty

        ppLoadBalancer :: ECS.LoadBalancer -> Doc
        ppLoadBalancer lb = Doc.vsep [
            Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ lb ^. ECS.lbContainerName
          , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
                Doc.field' "Port:" <$> lb ^. ECS.lbContainerPort
              , Doc.field' "ELB:" <$> lb ^. ECS.lbLoadBalancerName
              , Doc.field ppArn "Target Group:" <$> lb ^. ECS.lbTargetGroupARN
            ])
          ]

        ppDeployment :: ECS.Deployment -> Doc
        ppDeployment dep = Doc.vsep [
              Doc.bold . Doc.dullblue $ maybe mempty Doc.pretty $ dep ^. ECS.dId
            , Doc.indent defaultIndent (Doc.vsep $ catMaybes [
                  Doc.field ppArn "Task:" <$> dep ^. ECS.dTaskDefinition
                , Doc.field' "Running:" <$> dep ^. ECS.dRunningCount
                , Doc.field' "Desired:" <$> dep ^. ECS.dDesiredCount
                , Doc.field Doc.defaultTime "Updated:" <$> dep ^. ECS.dUpdatedAt
                , Doc.field Doc.defaultTime "Created:" <$> dep ^. ECS.dCreatedAt
              ])
            ]

        ppPlacementStrategy :: ECS.PlacementStrategy -> Doc
        ppPlacementStrategy ps = Doc.cyan $ Doc.hsep $ catMaybes [
              Doc.pretty . toText <$> ps ^. ECS.psType
            , Doc.pretty <$> ps ^. ECS.psField
          ]

        ppPlacementConstraint :: ECS.PlacementConstraint -> Doc
        ppPlacementConstraint pc = Doc.cyan $ Doc.hsep $ catMaybes [
             Doc.pretty . toText <$> pc ^. ECS.pcType
           , Doc.pretty <$> pc ^. ECS.pcExpression
         ]

runServiceInspect :: ServiceInspectOpts -> GrootIO ()
runServiceInspect (ServiceInspectOpts clusterRef serviceRef) = GrootT $ do
  env <- ask
  case clusterRef of
    Nothing -> putInfo $ "Scanning clusters for service " <> (styled yellowStyle $ toText serviceRef)
    _       -> pure ()
  xs <- runResourceT . runAWS env $ runMaybeT $ findService serviceRef clusterRef
  case xs of
    Nothing -> throwM $ serviceNotFound serviceRef clusterRef
    Just  s -> liftIO . Doc.putDoc $ pprintService s
