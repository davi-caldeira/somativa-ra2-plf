module Main where

import Control.Exception (IOException, catch)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime)
import InventoryData
import InventoryLogic
import InventoryReport
import System.IO (hFlush, stdout)

-- Ponto de entrada principal
main :: IO ()
main = do
  inv <- carregarInventario
  logs <- carregarLog
  putStrLn "Sistema de Inventário iniciado."
  loop inv logs

-- Loop principal de interação com o usuário
loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case words input of
    [] -> loop inv logs
    ("sair" : _) -> do
      putStrLn "Saindo do sistema."
      return ()
    ("exit" : _) -> do
      putStrLn "Saindo do sistema."
      return ()
    ("add" : iid : nome : qtdeStr : cat : _) -> do
      currentTime <- getCurrentTime
      let qtde = read qtdeStr :: Int
      case addItem currentTime iid nome qtde cat inv of
        Left err -> do
          let timeStr = show currentTime
          let detalhes = "item " ++ iid ++ " qty " ++ qtdeStr
          let logEntry = LogEntry timeStr Add detalhes (Falha err)
          appendFile "Auditoria.log" (show logEntry ++ "\n")
          putStrLn $ "Erro: " ++ err
          loop inv (logs ++ [logEntry])
        Right (newInv, logEntry) -> do
          writeFile "Inventario.dat" (show (Map.elems newInv))
          appendFile "Auditoria.log" (show logEntry ++ "\n")
          putStrLn "Item adicionado com sucesso."
          loop newInv (logs ++ [logEntry])
    ("remove" : iid : qtdeStr : _) -> do
      currentTime <- getCurrentTime
      let qtde = read qtdeStr :: Int
      case removeItem currentTime iid qtde inv of
        Left err -> do
          let timeStr = show currentTime
          let detalhes = "item " ++ iid ++ " qty " ++ qtdeStr
          let logEntry = LogEntry timeStr Remove detalhes (Falha err)
          appendFile "Auditoria.log" (show logEntry ++ "\n")
          putStrLn $ "Erro: " ++ err
          loop inv (logs ++ [logEntry])
        Right (newInv, logEntry) -> do
          writeFile "Inventario.dat" (show (Map.elems newInv))
          appendFile "Auditoria.log" (show logEntry ++ "\n")
          putStrLn "Item removido com sucesso."
          loop newInv (logs ++ [logEntry])
    ("update" : iid : novoNome : novaCat : novaQtdeStr : _) -> do
      currentTime <- getCurrentTime
      let novaQtde = read novaQtdeStr :: Int
      case updateItem currentTime iid novoNome novaCat novaQtde inv of
        Left err -> do
          let timeStr = show currentTime
          let detalhes =
                "item "
                  ++ iid
                  ++ " atualizado (nome="
                  ++ novoNome
                  ++ ", cat="
                  ++ novaCat
                  ++ ", qty="
                  ++ novaQtdeStr
                  ++ ")"
          let logEntry = LogEntry timeStr Update detalhes (Falha err)
          appendFile "Auditoria.log" (show logEntry ++ "\n")
          putStrLn $ "Erro: " ++ err
          loop inv (logs ++ [logEntry])
        Right (newInv, logEntry) -> do
          writeFile "Inventario.dat" (show (Map.elems newInv))
          appendFile "Auditoria.log" (show logEntry ++ "\n")
          putStrLn "Item atualizado com sucesso."
          loop newInv (logs ++ [logEntry])
    ("listar" : _) -> do
      putStrLn "Itens no inventário:"
      mapM_ printItem (Map.elems inv)
      loop inv logs
    ("report" : _) -> do
      putStrLn "Relatório de Auditoria:"
      let erros = logsDeErro logs
      if null erros
        then putStrLn "Nenhum erro registrado."
        else do
          putStrLn "Ocorrências de erro registradas:"
          mapM_ printLog erros
      case itemMaisMovimentado logs of
        Nothing -> putStrLn "Nenhuma movimentação registrada."
        Just (iid, total) ->
          putStrLn $
            "Item mais movimentado: "
              ++ iid
              ++ " (quantidade movida: "
              ++ show total
              ++ ")"
      let hist = historicoPorItem logs
      putStrLn "Total de operações por item:"
      mapM_ (\(iid, count) -> putStrLn $ "- " ++ iid ++ ": " ++ show count ++ " operações") hist
      loop inv logs
    _ -> do
      putStrLn "Comando desconhecido. Comandos: add, remove, update, listar, report, sair."
      loop inv logs

-- Função auxiliar para exibir um Item de forma legível
printItem :: Item -> IO ()
printItem (Item iid nome qtde cat) =
  putStrLn $ "- ID: " ++ iid ++ ", Nome: " ++ nome ++ ", Quantidade: " ++ show qtde ++ ", Categoria: " ++ cat

-- Função auxiliar para exibir uma entrada de log de forma legível
printLog :: LogEntry -> IO ()
printLog (LogEntry time acao detalhes status) = do
  let statusStr = case status of
        Sucesso -> "Sucesso"
        Falha msg -> "Falha (" ++ msg ++ ")"
  putStrLn $ "[" ++ time ++ "] " ++ show acao ++ " - " ++ statusStr ++ " - " ++ detalhes

-- Carrega o inventário do arquivo Inventario.dat (ou inicia vazio se o arquivo não existir)
carregarInventario :: IO Inventario
carregarInventario =
  catch
    ( do
        content <- readFile "Inventario.dat"
        let items = read content :: [Item]
        let inv = Map.fromList [(itemID it, it) | it <- items]
        return inv
    )
    ( \e -> do
        let _ = e :: IOException
        return Map.empty
    )

-- Carrega a lista de logs do arquivo Auditoria.log (ou lista vazia se não existir)
carregarLog :: IO [LogEntry]
carregarLog =
  catch
    ( do
        content <- readFile "Auditoria.log"
        let ls = lines content
        let entries = [read line :: LogEntry | line <- ls]
        return entries
    )
    ( \e -> do
        let _ = e :: IOException
        return []
    )
