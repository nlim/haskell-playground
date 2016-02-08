{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}


module Groundhogging where



import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

data Customer = Customer {
    customerName :: String,
    phone :: String
} deriving Show

data Product = Product {
    productName :: String,
    quantity :: Int,
    customer :: DefaultKey Customer
}
deriving instance Show Product


mkPersist defaultCodegenConfig [groundhog|
- entity: Customer               # Name of the datatype
  constructors:
  - name: Customer
    fields:
      - name: customerName
        # Set column name to "name" instead of "customerName"
        dbName: name
    uniques:
      - name: NameConstraint
        fields: [customerName] # Inline format of list
- entity: Product
|]

prog :: IO ()
prog = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration $ do
    migrate (undefined :: Customer)
    migrate (undefined :: Product)
  johnKey <- insert $ Customer "John Doe" "0123456789"
  get johnKey >>= liftIO . print
  orangeKey <- insert $ Product "Oranges" 3 johnKey
  appleKey  <- insert $ Product "Apples" 5 johnKey
  liftIO $ putStrLn $ "OrangeKey" ++ (show orangeKey)
  liftIO $ putStrLn $ "AppleKey" ++ (show appleKey)
  janeKey <- insert $ Customer "Jane Doe" "9876543210"
  _ <- insert $ Product "Oranges" 4 janeKey
  johnOrders <- select $ (CustomerField ==. johnKey) `orderBy` [Asc ProductNameField]
  liftIO $ putStrLn $ "Products for John: " ++ show johnOrders

