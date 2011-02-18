module TesterUtente where

import Estruturas.BSTree
import GUI.UtenteUI
import GUI.VeiculoUI
import Utente
import GUIHelper
import PontoAcesso
import System.Cmd

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
           ; drawItem 5 "Gravar"
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
                    '3':_ -> do { r' <- cicloUt appUT
                                ; cicloP (app, r')
                                }
                    '4':_ -> do { r' <- cicloUt appUT
                                ; cicloP (app, r')
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
                                ; system "pause"
                                ; cicloUt app
                                }
                    '2':_ -> do { opAlt <- utenteAlterarMenu
                                ; app <- utenteAlterarEscolha app opAlt
                                ; cicloUt app
                                }
                    '3':_ -> do { putStr "BI do Utente a Listar: "
                                ; bi <- getInt --From UtenteUI
                                ; maybe utenteNotFound utenteListar (getUtenteById app bi)
                                ; system "pause"
                                ; cicloUt app
                                }
                    '4':_ -> do { utenteListarTodos app
                                ; system "pause"
                                ; cicloUt app
                                }
                    '0':_ -> do {
								--r' <- utenteListar app
                                ; return app
                                }
                    _ -> do { putStrLn "Opcao Invalida"
                                    ; putStr $ "Valor -> " ++ op
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
