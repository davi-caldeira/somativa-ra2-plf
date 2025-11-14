module InventoryReport where

import qualified Data.Map as Map
import InventoryData

-- Filtra todas as entradas de log que terminaram com falha
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro entries = filter (\e -> case status e of Falha _ -> True; _ -> False) entries

-- Retorna uma lista com o total de operações realizadas por cada item
historicoPorItem :: [LogEntry] -> [(String, Int)]
historicoPorItem entries =
  let ids =
        [ iid
          | LogEntry _ ac det _ <- entries,
            ac /= QueryFail,
            let ws = words det,
            let iid =
                  if not (null ws) && ws !! 0 == "item"
                    then ws !! 1
                    else if not (null ws) then ws !! 0 else ""
        ]
   in Map.toList $ Map.fromListWith (+) [(iid, 1) | iid <- ids, iid /= ""]

-- Identifica o item que teve a maior movimentação de estoque (soma de quantidades)
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado entries =
  let changesList =
        [ (iid, qtde)
          | LogEntry _ ac det st <- entries,
            st == Sucesso,
            ac `elem` [Add, Remove],
            let ws = words det,
            length ws >= 4,
            let iid = if ws !! 0 == "item" then ws !! 1 else ws !! 0,
            let qtde = read (if ws !! 0 == "item" then ws !! 3 else ws !! 1) :: Int
        ]
      totals = Map.fromListWith (+) [(iid, abs qtde) | (iid, qtde) <- changesList]
   in if Map.null totals
        then Nothing
        else
          Just $
            Map.foldrWithKey
              (\iid total best -> if total >= snd best then (iid, total) else best)
              ("", 0)
              totals
