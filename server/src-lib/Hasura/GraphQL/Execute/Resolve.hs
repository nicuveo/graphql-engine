-- | Implements /variable resolution/ for GraphQL queries, which annotates the
-- use site of each GraphQL variable with its value.
module Hasura.GraphQL.Execute.Resolve
  ( resolveVariables
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict.Extended           as Map
import qualified Language.GraphQL.Draft.Syntax          as G

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH

import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types

resolveVariables
  :: forall m fragments
   . (MonadError QErr m, Traversable fragments)
  => [G.VariableDefinition]
  -> GH.VariableValues
  -> G.SelectionSet fragments G.Name
  -> m (G.SelectionSet fragments Variable)
resolveVariables definitions jsonValues selSet = do
  variables <- Map.fromListOn getName <$> traverse buildVariable definitions
  traverse (traverse (resolveVariable variables)) selSet
  where
    buildVariable :: G.VariableDefinition -> m Variable
    buildVariable G.VariableDefinition{ G._vdName, G._vdType, G._vdDefaultValue } = do
      let defaultValue = fromMaybe G.VNull _vdDefaultValue

      value <- case Map.lookup _vdName jsonValues of
        Just jsonValue -> pure $ JSONValue jsonValue
        Nothing
          | G.isNullable _vdType -> pure $ GraphQLValue defaultValue
          | otherwise -> throw400 ValidationFailed $
            "expecting a value for non-nullable variable: " <>> _vdName

      pure $! Variable
        { vInfo = if G.isNullable _vdType
            then VIOptional _vdName defaultValue
            else VIRequired _vdName
        , vValue = value
        }

    resolveVariable :: HashMap G.Name Variable -> G.Name -> m Variable
    resolveVariable variables name = case Map.lookup name variables of
      Just variable -> pure variable
      Nothing       -> throw400 ValidationFailed $ "unbound variable " <>> name
