{-# LANGUAGE TemplateHaskell #-}
module Obj.Type where
  import Database.Persist.TH

  data Type
    = Table
    | Class
    deriving (Eq, Ord, Show, Read)

  data PropType
    = ClassDecl
    | Method
    | Property
    deriving (Eq, Ord, Show, Read)

  derivePersistField "Type"
  derivePersistField "PropType"
