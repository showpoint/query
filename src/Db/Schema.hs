{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
module Db.Schema where
  import Database.Persist
  import Database.Persist.TH
  import Data.ByteString
  import Core.Types
  import Obj.Type

  share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Object
      type Type
      name Name
      ObjectUniq type name
    Data
      objId ObjectId
      type PropType
      name Name
      data ByteString
      DataTypeNameUniq objId type name
    Usage
      name Name
      propId DataId
  |]
