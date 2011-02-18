module Main where

-- Imports
import System.Cmd
import Data.Char
import Estruturas.BSTree
import System.Time

import GUIHelper
import GUI.PontoAcessoUI
import GUI.UtenteUI
import GUI.VeiculoUI

import PontoAcesso
import Parque
import Utente
import Veiculo

type APP_DATA = ListaPontoAcesso
type APP_DATA_UTENTES = BST_Utentes

main :: IO ()
main = cicloP ([PontoAcesso_Empty], BSTree_Empty)
          
menuP :: IO String
menuP = do { drawHeader "Menu Principal"
           ; drawItem 1 "Utentes"
           ; drawItem 2 "Veiculos"
           ; drawItem 3 "Pontos de Acesso"
           ; drawItem 4 "Relatorios"
           ; drawItem 5 "Carregar"
           ; drawItem 6 "Gravar"
           ; drawBlankLine
           ; drawItem 0 "Sair"
           ; drawBlankLine
           ; drawCloseLine
           ; putStr "Opcao: "
           ; input <- getLine
           ; return input
           }

cicloP :: (APP_DATA, APP_DATA_UTENTES) -> IO ()
cicloP (app, appUT) = do { op <- menuP
               ; case op of
                    '1':_ -> do { r' <- cicloUt appUT
                                ; cicloP (app, r')
                                }
                    '2':_ -> do { r' <- cicloVe appUT
                                ; cicloP (app, r')
                                }
                    '3':_ -> do { r' <- cicloPa app
                                ; cicloP (r', appUT)
                                }
                    '4':_ -> do { r' <- cicloRe app
                                ; cicloP (app, appUT)
                                }
                    '5':_ -> do { pontos <- readFile "pontos.dat"
                                ; utentes <- readFile "utentes.dat"
                                ; putStrLn "Dados Carregados com sucesso."
                                ; pause
                                ; cicloP (read pontos::ListaPontoAcesso, read utentes :: BST_Utentes)
                                }
                    '6':_ -> do { writeFile "pontos.dat" (show app)
                                ; writeFile "utentes.dat" (show appUT)
                                ; putStrLn "Dados Gravados com sucesso."
                                ; pause
                                ; cicloP (app, appUT)
                                }
                    '0':_ -> return ()
                    otherwise -> do { putStrLn "Opcao Invalida"
                                    ; cicloP (app, appUT)
                                    }
               }


-------------------------------------------------------------------------------               
-- Utentes      
-------------------------------------------------------------------------------                             
menuUt :: IO String
menuUt = do { drawHeader "Utentes"
            ; drawItem 1 "Adicionar"
            ; drawBlankLine
            ; drawItem 2 "Alterar"
            ; drawBlankLine
            ; drawItem 3 "Listar"
            ; drawItem 4 "Listar Todos"
            ; drawBlankLine
            ; drawItem 0 "Voltar"
            ; drawBlankLine
            ; drawCloseLine
            ; putStr "Opcao: "
            ; input <- getLine
            ; return input
            }
                                     
cicloUt :: APP_DATA_UTENTES -> IO APP_DATA_UTENTES
cicloUt app = do { op <- menuUt
                 ; case op of
                    '1':_ -> do { app <- utenteAdicionarMenu app
                                ; pause
                                ; cicloUt app
                                }
                    '2':_ -> do { opAlt <- utenteAlterarMenu
                                ; app <- utenteAlterarEscolha app opAlt
                                ; pause
                                ; cicloUt app
                                }
                    '3':_ -> do { putStr "BI do Utente a Listar: "
                                ; bi <- getInt --From UtenteUI
                                ; maybe utenteNotFound utenteListar (getUtenteById app bi)
                                ; pause
                                ; cicloUt app
                                }
                    '4':_ -> do { utenteListarTodos app
                                ; pause
                                ; cicloUt app
                                }
                    '0':_ -> do { putStrLn "Sair"
                                ; return app
                                }
                    _ -> do { putStrLn "Opcao Invalida"
                                    ; cicloUt app
                                    }
                 }

-------------------------------------------------------------------------------                 
-- Veiculos   
-------------------------------------------------------------------------------              
menuVe :: IO String
menuVe = do { drawHeader "Veiculos"
            ; drawItem 1 "Adicionar"
            ; drawItem 2 "Remover"
            ; drawItem 3 "Alterar"
            ; drawBlankLine
            ; drawItem 4 "Listar um veiculo"
            ; drawItem 5 "Listar Todos de um utilizador"
            ; drawBlankLine
            ; drawItem 0 "Voltar"
            ; drawBlankLine
            ; drawCloseLine
            ; putStr "Opcao: "
            ; input <- getLine
            ; return input
            }
                                     
cicloVe :: APP_DATA_UTENTES -> IO APP_DATA_UTENTES
cicloVe app = do { op <- menuVe
                 ; case op of
                    '1':_ -> do { app <- veiculoAdicionarMenu app
                                ; pause
                                ; cicloVe app
                                }
                    '2':_ -> do { 
                                ; app <- veiculoRemover app
                                ; pause
                                ; cicloVe app
                                }
                    '3':_ -> do { opAlt <- veiculoAlterarMenu
                                ; app <- veiculoAlterarEscolha app opAlt
                                ; pause
                                ; cicloVe app
                                }
                    '4':_ -> do { putStr "BI do Utente a Listar: "
                                ; bi <- getInt --From UtenteUI
                                ; putStr $ "Introduza a matricula: "
                                ; ma2 <- getLine
                                ; maybe utenteNotFound (\user-> veiculoFromUtenteListar user ma2)(getUtenteById app bi)
                                ; pause
                                ; cicloVe app
                                }
                    '5':_ -> do { putStr "BI do Utente a Listar: "
                                ; bi <- getInt --From UtenteUI
                                ; maybe utenteNotFound (\(Utente _ _ _ _ _ lista _) -> veiculoListarTodos lista)(getUtenteById app bi)
                                ; pause
                                ; cicloVe app
                                }
                    '0':_ -> do { putStrLn "Sair"
                                ; return app
                                }
                    otherwise -> do { putStrLn "Opcao Invalida"
                                    ; cicloVe app
                                    }
                 }

-------------------------------------------------------------------------------                 
-- Pontos de Acesso                 
-------------------------------------------------------------------------------
cicloPa:: APP_DATA -> IO APP_DATA
cicloPa app@(lpa) = do { op <- paMenu
                 ; case op of
                    '1':_ -> do { r' <- paPrintInserir lpa
                                ; cicloPa r'
                                }
                    '2':_ -> do { case paIsEmpty lpa of
                                    True -> do { putStrLn "Lista esta vazia"
                                               ; pause
                                               ; cicloPa lpa
                                               }
                                    _ -> do { r' <- paPrintRemover lpa
                                            ; cicloPa r'
                                            }
                                }
                    '3':_ -> do { case paIsEmpty lpa of
                                    True -> do { putStrLn "Lista esta vazia"
                                               ; pause
                                               ; cicloPa lpa
                                               }
                                    _ -> do { r' <- paPrintAlterar lpa
                                            ; cicloPa r'
                                            }
                                }
                    '4':_ -> do { putStr "Nome do Ponto de Acesso a Listar: "
                                ; nome <- getLine
                                ; paPrintPontoAcesso (paGetByName nome lpa)
                                ; pause
                                ; cicloPa app
                                }
                    '5':_ -> do { drawHeader "Listagem de Ponto de Acesso"
                                ; paPrintTodosPontoAcesso lpa
                                ; pause
                                ; cicloPa app
                                }
                    '6':_ -> do { drawHeader "Registar Entrada"
                                ; putStr "Nome do Ponto de Acesso: "
                                ; ponto <- getLine
                                ; case and [(paExiste ponto lpa), (paIsVago ponto lpa)] of
                                    True -> do { putStr "Matricula: "
                                               ; mat <- getLine
                                               ; tipoV <- drawGetInt "Tipo de Veiculo: "
                                               ; tipoU <- drawGetInt "Tipo de Utente: "
                                               ; case and [(paPermiteVeiculo (paGetByName ponto lpa) tipoV), (paPermiteUtente (paGetByName ponto lpa) tipoU)] of
                                                    True -> do { hora <- drawGetHora "Hora de Entrada: "
                                                               ; paWriteToLog (show hora) "Entrada"
                                                               ; cicloPa (paRegistarEntrada app (paGetByName ponto app) ("",mat, "", PASSAGEIROS, (show hora)))
                                                               }
                                                    _ -> do { putStrLn "Sem permissoes"
                                                            ; cicloPa lpa
                                                            }
                                               }
                                    _ -> do { putStr "O ponto de acesso nao existe ou encontra-se lotado"
                                            ; cicloPa lpa
                                            }
                                }
                    '7':_ -> do { drawHeader "Registar Saida"
                                ; putStr "Nome do Ponto de Acesso: "
                                ; ponto <- getLine
                                ; putStr "Matricula da Viatura: "
                                ; mat <- getLine
                                ; hora <- drawGetHora "Hora de Saida: "
                                ; money <- drawGetInt ("Valor a pagar " ++ (show (paGetValor (paGetByName ponto lpa) hora mat)) ++ "€: ")
                                ; paWriteToLog (show hora) "Saida"
                                ; cicloPa (paRegistarSaida app (paGetByName ponto app) mat)
                                }
                    '0':_ -> do { putStrLn "Sair"
                                ; return app
                                }
                    otherwise -> do { putStrLn "Opcao Invalida"
                                    ; cicloPa app
                                    }
                 }
                 
-------------------------------------------------------------------------------                 
-- Relatorios    
-------------------------------------------------------------------------------           
menuRe :: IO String
menuRe = do { drawHeader "Relatorios"
            ; drawItem 1 "Pontos de Acesso Lotados"
            ; drawItem 2 "Pontos de Acesso Vagos"
           -- ; drawItem 3 "Total Lugares Ocupados/Vagos"
            ; drawItem 4 "Registo de Entradas e Saidas"
            ; drawBlankLine
            ; drawItem 0 "Voltar"
            ; drawBlankLine
            ; drawCloseLine
            ; putStr "Opcao: "
            ; input <- getLine
            ; return input
            }
                                
cicloRe :: APP_DATA -> IO APP_DATA
cicloRe app = do { op <- menuRe
                 ; case op of
                    '1':_ -> do { case paIsEmpty app of
                                    True -> do { putStrLn "Sem pontos de acesso Lotados"
                                               ; pause
                                               ; cicloRe app
                                               }
                                    _ -> do { paPrintLotados app
                                            ; pause
                                            ; cicloRe app
                                            }
                                }
                    '2':_ -> do { paPrintVagos app
                                ; pause
                                ; cicloRe app
                                }
                    --'3':_ -> do { putStrLn "Total Lugares Ocupados"
                    --            ; cicloRe app
                    --            }
                    '4':_ -> do { putStrLn "Registo"
                                ; cicloRe app
                                }
                    '0':_ -> do { putStrLn "Sair"
                                ; return app
                                }
                    otherwise -> do { putStrLn "Opcao Invalida"
                                    ; cicloRe app
                                    }
                 }