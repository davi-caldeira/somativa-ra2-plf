module InventoryLogic where

import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import InventoryData

type ResultadoOperacao = (Inventario, LogEntry)

-- Adiciona um novo item ao inventário (falha se ID já existe)
addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem time iid nome qtde cat inv =
  if Map.member iid inv
    then Left "Item já existe no inventário"
    else
      let item = Item iid nome qtde cat
          newInv = Map.insert iid item inv
          timeStr = show time
          detalhes = "item " ++ iid ++ " qty " ++ show qtde
          logEntry = LogEntry timeStr Add detalhes Sucesso
       in Right (newInv, logEntry)

-- Remove uma quantidade de um item (falha se item não existe ou estoque insuficiente)
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time iid qtde inv =
  case Map.lookup iid inv of
    Nothing -> Left "Item não encontrado"
    Just item ->
      if quantity item < qtde
        then Left "Estoque insuficiente"
        else
          let newQty = quantity item - qtde
              newItem = item {quantity = newQty}
              newInv =
                if newQty == 0
                  then Map.delete iid inv
                  else Map.insert iid newItem inv
              timeStr = show time
              detalhes = "item " ++ iid ++ " qty " ++ show qtde
              logEntry = LogEntry timeStr Remove detalhes Sucesso
           in Right (newInv, logEntry)

-- Atualiza os dados de um item existente (nome, categoria e quantidade)
updateItem :: UTCTime -> String -> String -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateItem time iid novoNome novaCat novaQtde inv =
  case Map.lookup iid inv of
    Nothing -> Left "Item não encontrado"
    Just item ->
      let updatedItem = Item iid novoNome novaQtde novaCat
          newInv = Map.insert iid updatedItem inv
          timeStr = show time
          detalhes =
            "item "
              ++ iid
              ++ " atualizado (nome="
              ++ novoNome
              ++ ", cat="
              ++ novaCat
              ++ ", qty="
              ++ show novaQtde
              ++ ")"
          logEntry = LogEntry timeStr Update detalhes Sucesso
       in Right (newInv, logEntry)
