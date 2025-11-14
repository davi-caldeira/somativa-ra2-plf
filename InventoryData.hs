module InventoryData where

import Data.Map (Map)

-- Definição dos tipos de dados do sistema
data Item = Item
  { itemID :: String,
    name :: String,
    quantity :: Int,
    category :: String
  }
  deriving (Show, Read)

type Inventario = Map String Item

data AcaoLog = Add | Remove | Update | QueryFail deriving (Show, Read)

data StatusLog = Sucesso | Falha String deriving (Show, Read)

data LogEntry = LogEntry
  { timestamp :: String,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
  }
  deriving (Show, Read)
