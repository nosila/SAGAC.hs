module GUI.VeiculoUI where

import GUIHelper
import Data.Maybe
import GUI.UtenteUI
import Utente
import Estruturas.BSTree
import Veiculo

-- Inserir            
veiculoAdicionarMenu :: BST_Utentes -> IO BST_Utentes
veiculoAdicionarMenu arvore = do
                                drawCloseLine
                                drawBlankLine
                                utente' <- getUtente arvore
                                case utente' of
                                    Utente a b c d e lista f ->
                                            do
                                                putStr "Marca: "
                                                mr <- getLine
                                                putStr "Modelo: "
                                                md <- getLine
                                                putStr "Matricula: "
                                                ma <- getLine --NAO PODE ESTAR VAZIA!!
                                                putStr "Tipo: "
                                                tp <- getTipoVeiculoUI
                                                drawBlankLine
                                                let (lista',status) = addVeiculo lista (mr,md,ma,tp, "agora")
                                                case status of
                                                    True -> do
                                                                let arvore' = replaceElement arvore (Utente a b c d e lista' f)
                                                                putStrLn "\tVeiculo adicionado com sucesso!"
                                                                return arvore'
                                                    False -> do
                                                                putStrLn "\tJa existe um veiculo com a mesma matricula!"
                                                                return arvore
                                                
                                    _ -> do 
                                        putStr $ "\n\tNao existem utilizadores no sistema ou nao existe nenhum utilizador com o BI inserido.\n"
                                        return arvore     
				
intToTipoVeiculo :: Int -> Tipo_Veiculo
intToTipoVeiculo i 
        | i == 1 = PASSAGEIROS
        | i == 2 = MOTA
        | i == 3 = AMBULANCIA
        | i == 4 = TAXI
        

getTipoVeiculoUI :: IO Tipo_Veiculo
getTipoVeiculoUI =
  do putStr "\n[1] - Passageiros "
     putStr "\n[2] - Mota "
     putStr "\n[3] - Ambulancia "
     putStr "\n[4] - Taxi "
     input <- getLine
     case reads input of
        [(value,"")] | value >= 1 && value <= 4 -> return (intToTipoVeiculo value)
        _ -> do putStrLn $ "Introduza um numero entre 1 e 5."
                getTipoVeiculoUI   

--Remover
veiculoRemoverFromUtente :: Utente -> String -> Utente
veiculoRemoverFromUtente (Utente a b c d e lista f) ma2 = 
            do
                let lista' = filter (\(_,_,ma1,_,_)-> ma1 /= ma2) lista
                (Utente a b c d e lista' f)

veiculoRemover :: BST_Utentes -> IO BST_Utentes
veiculoRemover arvore = do
        utente' <- getUtente arvore
        case utente' of
            Utente {} ->do
                            putStr $ "Introduza a matricula: "
                            ma2 <- getLine
                            let status = verificaMatriculaString (getListaVeiculos utente') ma2
                            if status 
                                then do
                                        putStrLn "\n\tNao existe nenhum carro com essa matricula! "
                                        return arvore
                                else do
                                        let utenteAux =veiculoRemoverFromUtente utente' ma2
                                        putStr "\n\tAlteracao efectuada com sucesso.\n"                                    
                                        return $ replaceElement arvore utenteAux
            
            _ -> do 
                    putStr $ "\n\tNao existem utilizadores no sistema ou nao existe nenhum utilizador com o BI inserido.\n"
                    return arvore

--Alterar
veiculoAlterarMenu :: IO Int
veiculoAlterarMenu = do {
    drawHeader "Alterar Dados do Veiculo";
    drawItem 1 "Marca";
    drawItem 2 "Modelo";
    drawItem 3 "Tipo";
    drawBlankLine;
    drawCloseLine;
    putStr "Opcao> ";
    input <- getIntBetween 1 3;
    return input
    }
    
veiculoAlterarEscolha :: BST_Utentes -> Int -> IO BST_Utentes
veiculoAlterarEscolha arvore op = do
    utente' <- getUtente arvore
    case utente' of
            -- or Utente _ _ _ _ _ _ _ -> case op of
            Utente {} -> 
                do
                    putStr $ "Insira a matricula: "
                    ma2 <- getLine
                    let status = verificaMatriculaString (getListaVeiculos utente') ma2
                    if status 
                        then do
                                putStrLn "\n\tNao existe nenhum carro com essa matricula! "
                                return arvore
                        else do
                                case op of
                                    1 -> do
                                            utenteAux <- veiculoAlterarMarca utente' ma2
                                            putStr "\n\tAlteracao efectuada com sucesso.\n"
                                            return $ replaceElement arvore utenteAux
                                    2 -> do
                                            utenteAux <- veiculoAlterarModelo utente' ma2
                                            putStr "\n\tAlteracao efectuada com sucesso.\n"                                    
                                            return $ replaceElement arvore utenteAux
                                    3 -> do
                                            utenteAux <- veiculoAlterarTipo utente' ma2
                                            putStr "\n\tAlteracao efectuada com sucesso.\n"
                                            return $ replaceElement arvore utenteAux
            _ -> do 
                    putStr $ "\n\tNao existem utilizadores no sistema ou nao existe nenhum utilizador com o BI inserido.\n"
                    return arvore

veiculoAlterarMarca :: Utente -> String -> IO Utente
veiculoAlterarMarca Utente_Empty _ = return Utente_Empty
veiculoAlterarMarca u@(Utente a b c d e lista g) ma2 = do
    drawCloseLine
    drawBlankLine
    let v@(_,lb, lc, ld, le) = fromJust $ getVeiculo lista ma2
    let u'@(Utente an bn cn dn en fn gn) = veiculoRemoverFromUtente u ma2
    putStr "Nova Marca do Veiculo: " 
    ma' <- getLine
    return (Utente an bn cn dn en ((ma',lb, lc, ld, le):fn) gn)
    

veiculoAlterarModelo :: Utente -> String -> IO Utente
veiculoAlterarModelo Utente_Empty _ = return Utente_Empty
veiculoAlterarModelo u@(Utente a b c d e lista g) ma2 = do
    drawCloseLine
    drawBlankLine
    let v@(la,_, lc, ld, le) = fromJust $ getVeiculo lista ma2
    let u'@(Utente an bn cn dn en fn gn) = veiculoRemoverFromUtente u ma2
    putStr "Novo Modelo do Veiculo: " 
    mo' <- getLine
    return (Utente an bn cn dn en ((la,mo', lc, ld, le):fn) gn)
   
veiculoAlterarTipo :: Utente -> String -> IO Utente
veiculoAlterarTipo Utente_Empty _ = return Utente_Empty
veiculoAlterarTipo u@(Utente a b c d e lista g) ma2 = do
    drawCloseLine
    drawBlankLine
    let v@(la,lb, lc, _, le) = fromJust $ getVeiculo lista ma2
    let u'@(Utente an bn cn dn en fn gn) = veiculoRemoverFromUtente u ma2
    putStr "Novo Tipo do Veiculo: " 
    tp' <- getTipoVeiculoUI
    return (Utente an bn cn dn en ((la,lb, lc, tp', le):fn) gn)
    
--Listar
getVeiculoFromUtente :: Utente -> String -> Veiculo
getVeiculoFromUtente (Utente _ _ _ _ _ lista _) ma2 = head $ filter (\(_,_,ma1,_,_)-> ma1 == ma2) lista

veiculoFromUtenteListar :: Utente -> String -> IO ()
veiculoFromUtenteListar u@(Utente _ _ _ _ _ lista _) ma2 = veiculoListar $ getVeiculoFromUtente u ma2


veiculoListar :: Veiculo -> IO ()
veiculoListar (mr,md,ma,tp, hora) = do
        drawCloseLine
        drawBlankLine
        drawValue "Marca " mr
        drawValue "Modelo " md
        drawValue "Matricula " ma
        drawValue "Tipo " tp
        drawValue "Hora " hora
        drawBlankLine
        drawCloseLine
        return ()
    
    
--ListarTodos
veiculoListarTodos :: ListaVeiculos -> IO ()
veiculoListarTodos [] = return ()
veiculoListarTodos (v:lista) = do
                                veiculoListar v
                                veiculoListarTodos lista