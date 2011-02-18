module GUI.UtenteUI where

import Data.Maybe
import GUIHelper
import Utente
import Estruturas.BSTree

--------------------------------------------------------------------------------------
-- small useful functions
--------------------------------------------------------------------------------------
utenteNotFound :: IO ()
utenteNotFound = putStr "\n\tUtente nao foi encontrado.\n\n"

getInt :: IO Int
getInt = do
  input <- getLine
  case reads input of
    [(value,"")] | value > 0 -> return value
    _ -> do
            putStrLn $ "Introduza um numero maior que 0."
            getInt
            
getIntBetween :: Int -> Int -> IO Int
getIntBetween min max = do
  input <- getLine
  case reads input of
    [(value,"")] | value >= min  && value <= max -> return value
    _ -> do
            putStrLn $ "Introduza um numero "++ show min ++" e " ++ show max ++"."
            getInt
 
getUtente :: BST_Utentes -> IO Utente
getUtente BSTree_Empty = return Utente_Empty
getUtente arvore = do{
    putStr "\nIndique o BI do utente: ";
    bi <- getInt;
    return (fromMaybe Utente_Empty (getUtenteById arvore bi));
}
--------------------------------------------------------------------------------------
-- end of small useful functions
--------------------------------------------------------------------------------------

utenteMenu :: IO String
utenteMenu = do { drawHeader "Utente"
            ; drawItem 1 "Adicionar"
            ; drawItem 2 "Remover"
            ; drawBlankLine
            ; drawItem 3 "Alterar"
            ; drawBlankLine
            ; drawItem 4 "Listar"
            ; drawItem 5 "Listar Todos"
            ; drawBlankLine
            ; drawBlankLine
            ; drawItem 0 "Voltar"
            ; drawBlankLine
            ; drawCloseLine
            ; putStr "Opcao: "
            ; input <- getLine
            ; return input
            }

-- Inserir            
utenteAdicionarMenu :: BST_Utentes -> IO BST_Utentes
utenteAdicionarMenu arvore = do
     drawCloseLine
     drawBlankLine
     putStr "nome: "
     no <- getLine
     putStr "bi: "
     bi <- getInt
     putStr "Tipo: "
     tp <- getTipo
     putStr "Profissao: "
     pr <- getLine
     putStr "Morada: "
     mo <- getLine

     let u = (Utente no bi tp pr mo [] NAO)
         newTree = addElement arvore u
     case newTree of
         BSTree_Element_Exists -> do
                 putStrLn "\tJa existe um utente com o mesmo BI. Novo utente nao inserido!"
                 return arvore
         _ -> do
                putStrLn "\n\tUtente Inserido com sucesso!\n"
                return newTree
    
    

intToTipo :: Int -> Tipo
intToTipo i 
        | i == 1 = MORADOR
        | i == 2 = TURISTA
        | i == 3 = FUNCIONARIO_CAMARA
        | i == 4 = MOTORISTA
        | i == 5 = FUNCIONARIO_LIMPEZA
        

getTipo :: IO Tipo
getTipo =
  do putStr "\n[1] - Morador "
     putStr "\n[2] - Turista "
     putStr "\n[3] - Funcionario da Camara "
     putStr "\n[4] - Motorista "
     putStr "\n[5] - Funcionario das Limpezas \n"
     input <- getLine
     case reads input of
        [(value,"")] | value >= 1 && value <= 5 -> return (intToTipo value)
        _ -> do putStrLn $ "Introduza um numero entre 1 e 5."
                getTipo      

--Alterar
utenteAlterarMenu :: IO Int
utenteAlterarMenu = do {
    drawHeader "Alterar Dados do Utente";
    drawItem 1 "Nome";
    drawItem 2 "Tipo";
    drawItem 3 "Profissao";
    drawItem 4 "Morada";
    drawItem 5 "Penalizado? ";
    putStr "Opcao> ";
    input <- getIntBetween 1 5;
    return input
    }
    
utenteAlterarEscolha :: BST_Utentes -> Int -> IO BST_Utentes
utenteAlterarEscolha arvore op = do
    utente' <- getUtente arvore
    case utente' of
            -- or Utente _ _ _ _ _ _ _ -> case op of
            Utente {} -> case op of
                            1 -> do
                                    utenteAux <- utenteAlterarNome utente' 
                                    putStr "\n\tAlteracao efectuada com sucesso.\n"
                                    return $ replaceElement arvore utenteAux
                            2 -> do
                                    utenteAux <- utenteAlterarTipo utente'
                                    putStr "\n\tAlteracao efectuada com sucesso.\n"                                    
                                    return $ replaceElement arvore utenteAux
                            3 -> do
                                    utenteAux <- utenteAlterarProfissao utente'
                                    putStr "\n\tAlteracao efectuada com sucesso.\n"
                                    return $ replaceElement arvore utenteAux
                            4 -> do  
                                    utenteAux <- utenteAlterarMorada utente'
                                    putStr "\n\tAlteracao efectuada com sucesso.\n"
                                    return $ replaceElement arvore utenteAux
                            5 -> do
                                    utenteAux <- utenteAlterarPenalizacao utente'
                                    putStr "\n\tAlteracao efectuada com sucesso.\n"
                                    return $ replaceElement arvore utenteAux
            _ -> do 
                    putStr $ "\n\tNao existem utilizadores no sistema ou nao existe nenhum utilizador com o BI inserido.\n"
                    return arvore

utenteAlterarNome :: Utente -> IO Utente
utenteAlterarNome Utente_Empty = return Utente_Empty
utenteAlterarNome u@(Utente nome b c d e f g) = do{
    drawCloseLine;
    drawBlankLine;
    putStr "Novo Nome do Utente: " ;
    nome' <- getLine;
    return (Utente nome' b c d e f g);
    }

utenteAlterarTipo :: Utente -> IO Utente
utenteAlterarTipo Utente_Empty = return Utente_Empty
utenteAlterarTipo u@(Utente a b tipo d e f g) = do{
    drawCloseLine;
    drawBlankLine;
    putStr "Novo Tipo do Utente: " ;
    tipo' <- getTipo;
    return (Utente a b tipo' d e f g);
    }
    
utenteAlterarProfissao :: Utente -> IO Utente
utenteAlterarProfissao Utente_Empty = return Utente_Empty
utenteAlterarProfissao u@(Utente a b c profissao e f g) = do{
    drawCloseLine;
    drawBlankLine;
    putStr "Nova Profissao do Utente: " ;
    profissao' <- getLine;
    return (Utente a b c profissao' e f g);
    }
    
utenteAlterarMorada :: Utente -> IO Utente
utenteAlterarMorada Utente_Empty = return Utente_Empty
utenteAlterarMorada u@(Utente a b c d morada f g) = do{
    drawCloseLine;
    drawBlankLine;
    putStr "Nova Morada do Utente: " ;
    morada' <- getLine;
    return (Utente a b c d morada' f g);
    }
    
utenteAlterarPenalizacao :: Utente -> IO Utente
utenteAlterarPenalizacao Utente_Empty = return Utente_Empty
utenteAlterarPenalizacao u@(Utente a b c d e f penalizacao) = do
    drawCloseLine;
    drawBlankLine;
    penalizacao' <- case penalizacao of 
                        SIM -> do
                                putStr "\n\tO utente ESTA penalizado, tal penalizacao ira ser retirada.\n"
                                return NAO 
                                
                            
                        NAO -> do 
                                putStr "\n\tO utente NAO ESTA penalizado, ira ser penalizado.\n"
                                return SIM 
                                
                     
    return (Utente a b c d e f penalizacao')

    
--Listar
utenteListar :: Utente -> IO ()
utenteListar Utente_Empty = return ()
utenteListar (Utente nome bi tipo profissao morada _ penalizado) = do
    { drawCloseLine
    ; drawBlankLine
    ; drawValue "Nome " nome
    ; drawValue "Bi " bi
    ; drawValue "Tipo " tipo
    ; drawValue "Profissao " profissao
    ; drawValue "Morada " morada
    ; drawValue "Esta penalizado? " penalizado
    ; drawBlankLine
    ; drawCloseLine
    ; return ()
    }
    

--ListarTodos
utenteListarTodos :: BST_Utentes -> IO ()
utenteListarTodos BSTree_Empty = return ()
utenteListarTodos (Leaf utente left right) = do{
                    utenteListar utente;
                    utenteListarTodos left;
                    utenteListarTodos right;
                    return ()
                    }