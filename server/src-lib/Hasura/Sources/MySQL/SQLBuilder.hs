module Hasura.Sources.MySQL.SQLBuilder (mySQLBuilder) where

import           Hasura.Prelude

import qualified Data.Text          as T
import qualified Text.Builder       as TB

import           Hasura.SQL.Builder
import           Hasura.SQL.Text
import           Hasura.SQL.Types


mySQLBuilder :: SQLBuilder
mySQLBuilder = SQLBuilder
  myLiteral
  myIdentifier

myLiteral :: T.Text -> TB.Builder
myLiteral = TB.text . pgFmtLit

myIdentifier :: Identifier -> TB.Builder
myIdentifier = TB.text . myFmtIden . toTxt

myFmtIden :: T.Text -> T.Text
myFmtIden x =
  "`" <> T.replace "`" "``" (trimNullChars x) <> "`"
