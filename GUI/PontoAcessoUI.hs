module GUI.PontoAcessoUI where

import Data.Char
import GUIHelper
import PontoAcesso
import Parque

paMenu :: IO String
paMenu = do { drawHeader "Pontos de Acesso"
            ; drawItem 1 "Adicionar"
            ; drawItem 2 "Remover"
            ; drawItem 3 "Alterar"
            ; drawBlankLine
            ; drawItem 4 "Listar"
            ; drawItem 5 "Listar Todos"
            ; drawBlankLine
            ; drawItem 6 "Registar Entrada"
            ; drawItem 7 "Registar Saida"
            ; drawBlankLine
            ; drawItem 0 "Voltar"
            ; drawBlankLine
            ; drawCloseLine
            ; putStr "Opcao: "
            ; input <- getLine
            ; return input
            }

-- Inserir            
paPrintInserir :: ListaPontoAcesso -> IO ListaPontoAcesso
paPrintInserir l = do
    { drawCloseLine
    ; drawBlankLine
    ; id <- drawGetInt "ID: "
    ; putStr "Nome: "
    ; no <- getLine
    ; putStr "Descricao: "
    ; de <- getLine
    ; cap <- drawGetInt "Capacidade: "
    ; numA <- drawGetInt "Numero de Ambulancias: "
	; numT <- drawGetInt "Numero de Taxis: "
	; numL <- drawGetInt "Numero de Ligeiros: "
	; numM <- drawGetInt "Numero de Motas: "
    ; pl <- drawGetBool "Permite Ligeiros: "
    ; pa <- drawGetBool "Permite Ambulancias: "
    ; pm <- drawGetBool "Permite Motas: "
    ; pt <- drawGetBool "Permite Taxis: "
    ; pmora <- drawGetBool "Permite Moradores: "
    ; pturi <- drawGetBool "Permite Turistas: "
    ; pmoto <- drawGetBool "Permite Motoristas: "
    ; pfc <- drawGetBool "Permite Funcionario da Camara: "
    ; pfl <- drawGetBool "Permite Funcionario de Limpeza: "
    ; drawBlankLine
    ; putStrLn "Ponto de Acesso Inserido com sucesso!"
    ; return (paInserir (PAcesso id no de (Parque cap [] numA numT numL numM) (VPerm pl pa pm pt) (UPerm pmora pturi pmoto pfc pfl)) l)
    }

-- Remover
paPrintRemover :: ListaPontoAcesso -> IO ListaPontoAcesso
paPrintRemover l = do
    { drawCloseLine
    ; drawBlankLine
    ; putStr "Nome do Ponto de Acesso a remover: "
    ; nome <- getLine
    ; return (paRemoverByName nome l)
    }
    
-- Alterar
paPrintAlterar :: ListaPontoAcesso -> IO ListaPontoAcesso
paPrintAlterar [] = return []
paPrintAlterar [PontoAcesso_Empty] = return [PontoAcesso_Empty]
paPrintAlterar ((PontoAcesso_Empty):t) = paPrintAlterar t
paPrintAlterar l@((PAcesso i _ _ p vp up):t) = do
    { drawCloseLine
    ; drawBlankLine
    ; putStr "Nome do Ponto de Acesso a Alterar: "
    ; nome <- getLine
    ; drawBlankLine
    ; putStr "Novo Nome: "
    ; novoNome <- getLine
    ; putStr "Nova Descricao: "
    ; novaDesc <- getLine
    ; putStr "Nova Capacidade: "
    ; novacap <- getLine
    ; putStr "Permite Ligeiros?: "
    ; novaPermPass <- getLine
    ; putStr "Permite Ambulancias?: "
    ; novaPermAmbu <- getLine
    ; putStr "Permite Taxis?: "
    ; novaPermTaxi <- getLine
    ; putStr "Permite Motas?: "
    ; novaPermMota <- getLine
    ; return (pAlterarById i (PAcesso i novoNome novaDesc p vp up) l)
    }

-- Listar
paPrintPontoAcesso :: PontoAcesso -> IO ()
paPrintPontoAcesso PontoAcesso_Empty = return ()
paPrintPontoAcesso p@(PAcesso id nome desc parque _ _) = do
    { drawCloseLine
    ; drawBlankLine
    ; drawValue "ID" id
    ; drawValue "Nome" nome
    ; drawValue "Descricao" desc
    ; drawValue "Capacidade" (pqGetOcupacao parque)
    ; paPrintPerms (paGetPermsVeiculo p) (paGetPermsUtente p)
    ; drawBlankLine
    ; drawCloseLine
    ; return ()
    }

paPrintPerms :: VeiculoPerms -> UtentePerms -> IO ()
paPrintPerms VPerm_Empty UPerm_Empty = drawValue "Permissoes" "Sem Permissoes"
paPrintPerms VPerm_Empty _ = drawValue "Permissoes" "Sem Permissoes"
paPrintPerms _ UPerm_Empty = drawValue "Permissoes" "Sem Permissoes"
paPrintPerms (VPerm a b c d) (UPerm e f g h i) = do
    { putStrLn "Veiculos:"
    ; drawValue "Ligeiros" a
    ; drawValue "Ambulancia" b
    ; drawValue "Mota" c
    ; drawValue "Taxi" d
    ; drawBlankLine
    ; putStrLn "Utentes:"
    ; drawValue "Morador" e
    ; drawValue "Turista" f
    ; drawValue "Motorista" g
    ; drawValue "Funcionario da Camara" h
    ; drawValue "Funcionario de Limpeza" i
    ; return ()
    }
    
-- Listar Todos    
paPrintTodosPontoAcesso :: ListaPontoAcesso -> IO ()
paPrintTodosPontoAcesso [] = return ()
paPrintTodosPontoAcesso (h:t) = do { paPrintPontoAcesso h; paPrintTodosPontoAcesso t }
                     
paPrintLotados :: ListaPontoAcesso -> IO ()
paPrintLotados [] = return ()
paPrintLotados ((PontoAcesso_Empty):t) = paPrintTodosPontoAcesso (filter (\(PAcesso _ _ _ x _ _)->pqIsLotado x) t)
paPrintLotados l = paPrintTodosPontoAcesso (filter (\(PAcesso _ _ _ x _ _)->pqIsLotado x) l)

paPrintVagos :: ListaPontoAcesso -> IO ()
paPrintVagos [] = return ()
paPrintVagos ((PontoAcesso_Empty):t) = paPrintVagos t
paPrintVagos l@(h@(PAcesso id nome desc _ _ _):t) | (paIsVago nome l) = do { putStrLn $ "Ponto Acesso: " ++ show id ++ "\nNome: " ++ nome ++ "\nDescricao: " ++ desc
                                                                           ; paPrintVagos t
                                                                           }
                                                | otherwise = paPrintVagos t












                   