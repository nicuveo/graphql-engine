module Hasura.GraphQL.Schema.Table
  ( tableSelectColumnsEnum
  , tableUpdateColumnsEnum
  , tablePermissions
  , tableSelectPermissions
  , tableUpdatePermissions
  , tableDeletePermissions
  , tableSelectFields
  , tableColumns
  , tableSelectColumns
  , tableUpdateColumns
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.GraphQL.Parser         (Kind (..), Parser)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column  (qualifiedObjectToName)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
-- TODO move code around so that we don't import an internal module here
import           Hasura.RQL.DML.Internal
import           Hasura.Session

-- | Table select columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used as a parameter for "distinct", among others. Maps to
-- the table_select_column object.
--
-- Return Nothing if there's no column the current user has "select"
-- permissions for.
tableSelectColumnsEnum
  :: (MonadSchema n m, MonadRole r m, MonadTableInfo r m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Maybe (Parser 'Both n PGCol))
tableSelectColumnsEnum table selectPermissions = do
  tableName <- qualifiedObjectToName table
  columns   <- tableSelectColumns table selectPermissions
  let enumName    = tableName <> $$(G.litName "_select_column")
      description = Just $ G.Description $
        "select columns of table \"" <> G.unName tableName <> "\""
  pure $ P.enum enumName description <$> nonEmpty
    [ ( define $ pgiName column
      , pgiColumn column
      )
    | column <- columns
    ]
  where
    define name =
      P.mkDefinition name (Just $ G.Description "column name") P.EnumValueInfo

-- | Table update columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used for conflict resolution in "insert" mutations, among
-- others. Maps to the table_update_column object.
--
-- Return Nothing if there's no column the current user has "update"
-- permissions for.
tableUpdateColumnsEnum
  :: (MonadSchema n m, MonadRole r m, MonadTableInfo r m)
  => QualifiedTable
  -> UpdPermInfo
  -> m (Maybe (Parser 'Both n PGCol))
tableUpdateColumnsEnum table updatePermissions = do
  tableName <- qualifiedObjectToName table
  columns   <- tableUpdateColumns table updatePermissions
  let enumName    = tableName <> $$(G.litName "_update_column")
      description = Just $ G.Description $
        "update columns of table \"" <> G.unName tableName <> "\""
  pure $ P.enum enumName description <$> nonEmpty
    [ ( define $ pgiName column
      , pgiColumn column
      )
    | column <- columns
    ]
  where
    define name =
      P.mkDefinition name (Just $ G.Description "column name") P.EnumValueInfo

-- TODO deduplicate with @askPermInfo'@?
tablePermissions
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> m (Maybe RolePermInfo)
tablePermissions table = do
  roleName  <- askRoleName
  tableInfo <- askTableInfo table
  pure $ if roleName == adminRoleName
    then Just $ mkAdminRolePermInfo $ _tiCoreInfo tableInfo
    else Map.lookup roleName $ _tiRolePermInfoMap tableInfo

tableSelectPermissions
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> m (Maybe SelPermInfo)
tableSelectPermissions table = (_permSel =<<) <$> tablePermissions table

tableUpdatePermissions
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> m (Maybe UpdPermInfo)
tableUpdatePermissions table = (_permUpd =<<) <$> tablePermissions table

tableDeletePermissions
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> m (Maybe DelPermInfo)
tableDeletePermissions table = (_permDel =<<) <$> tablePermissions table

tableSelectFields
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo
  -> m [FieldInfo]
tableSelectFields table permissions = do
  -- TODO: memoize this?
  tableFields <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  filterM canBeSelected $ Map.elems tableFields
  where
    canBeSelected (FIColumn columnInfo) =
      pure $ Set.member (pgiColumn columnInfo) (spiCols permissions)
    canBeSelected (FIRelationship relationshipInfo) =
      isJust <$> tableSelectPermissions (riRTable relationshipInfo)
    canBeSelected (FIComputedField computedFieldInfo) =
      case _cfiReturnType computedFieldInfo of
        CFRScalar _ ->
          pure $ Set.member (_cfiName computedFieldInfo) $ spiScalarComputedFields permissions
        CFRSetofTable tableName ->
          isJust <$> tableSelectPermissions tableName
    canBeSelected (FIRemoteRelationship _) = pure True   -- TODO: Are there permissions for remote join fields?

tableColumns
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => QualifiedTable
  -> m [PGColumnInfo]
tableColumns table =
  mapMaybe columnInfo . Map.elems . _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _             = Nothing

tableSelectColumns
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo
  -> m [PGColumnInfo]
tableSelectColumns table permissions =
  mapMaybe columnInfo <$> tableSelectFields table permissions
  where
    columnInfo (FIColumn ci) = Just ci
    columnInfo _             = Nothing

tableUpdateColumns
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => QualifiedTable
  -> UpdPermInfo
  -> m [PGColumnInfo]
tableUpdateColumns table permissions = do
  -- TODO: memoize this?
  -- TODO: check that colums that have presets are NOT included in this
  tableFields <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  pure $ mapMaybe isUpdatable $ Map.elems tableFields
  where
    isUpdatable (FIColumn columnInfo) =
      if Set.member (pgiColumn columnInfo) (upiCols permissions)
      then Just columnInfo
      else Nothing
    isUpdatable _ = Nothing
