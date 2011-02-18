module Utente where

import Veiculo
import Estruturas.BSTree

{-
	===============================================
	============== *TESTE Utente* ================
	===============================================
-}

lista1 :: ListaVeiculos
lista1 = [v1, v2]

lista2 :: ListaVeiculos
lista2 = [v3]

utente1 :: Utente
utente1 = Utente "Alison Fernandes" 6129 MORADOR "Estudante" "Rua de cima" lista1 NAO

utente2 :: Utente
utente2 = Utente "Hugo Ruivo" 6110 MORADOR "Estudante" "Rua de cima" lista2 NAO

utente3 :: Utente
utente3 = Utente "Joao pinto" 6120 TURISTA "Estudante" "Rua de cima" lista2 NAO

utente4 :: Utente
utente4 = Utente "Zebedeu Arvoredo" 6200 MOTORISTA "Estudante" "Rua de cima" lista1 NAO



arvore :: BST_Utentes
arvore = (Leaf utente1 BSTree_Empty BSTree_Empty)


{-
	===============================================
	============ *FIM TESTE Utente* ===============
	===============================================
-}

-------------------------------------------------------------------------

{-
	===============================================
	================== *Utente* ===================
	===============================================
-}
type Nome = String
type BI = Int
data Tipo = MORADOR | TURISTA | FUNCIONARIO_CAMARA | MOTORISTA | FUNCIONARIO_LIMPEZA
	deriving (Show,Read)
type Profissao = String
type Morada = String
type ListaVeiculos = [Veiculo]
data Penalizado = SIM | NAO
	deriving (Show,Read)

data Utente = Utente_Empty | Utente Nome BI Tipo Profissao Morada ListaVeiculos Penalizado
	deriving (Show, Read)

type BST_Utentes = BSTree Utente

{-
instance Eq Utente where 
	 Utente _ id1 _ _ _ _ _ == Utente _ id2 _ _ _ _ _  = id1 == id2
-}



{- === *LISTA VEICULOS* === -------------------------------------------}

getListaVeiculos :: Utente -> ListaVeiculos
getListaVeiculos (Utente _ _ _ _ _ lista _) = lista

getVeiculo :: ListaVeiculos -> String -> Maybe Veiculo
getVeiculo [] _ = Nothing
getVeiculo (v@(_,_,ma1,_,_):lista) ma2
        | ma1 == ma2 = Just v
        | otherwise = getVeiculo lista ma2

--Retorna True caso o veiculo seja adicionado
addVeiculo:: ListaVeiculos -> Veiculo -> (ListaVeiculos,Bool)
addVeiculo lista v@(_,_,_,_,_) 
            | verificaMatricula lista v = (v:lista,True)
            | otherwise = (lista,False)

--Retorna True caso a matricula nao exista na lista
verificaMatricula:: ListaVeiculos -> Veiculo -> Bool
verificaMatricula [] _ = True
verificaMatricula ((_,_,m1,_,_):lista) v2@(_,_,m2,_,_) 
                | m1 == m2 = False
                | otherwise = verificaMatricula lista v2
--Retorna True caso a matricula nao exista na lista
verificaMatriculaString:: ListaVeiculos -> String -> Bool
verificaMatriculaString [] _ = True
verificaMatriculaString ((_,_,m1,_,_):lista) m2 
                | m1 == m2 = False
                | otherwise = verificaMatriculaString lista m2                

printVeiculo :: Veiculo -> String
printVeiculo v@(marca,modelo,matricula,tipo,hora) = "Marca: " ++ marca ++ " Modelo: " ++ modelo ++ " Tipo Veiculo: "++ show tipo ++ " Hora_Entrada: " ++ hora

{- === *FIM LISTA VEICULOS* === -----------------------------------------}

{- === *ARVORE UTENTES* === -------------------------------------------}
getUtenteById:: BST_Utentes -> Int -> Maybe Utente
getUtenteById BSTree_Empty _ = Nothing
getUtenteById tree@(Leaf utente@(Utente _ id1 _ _ _ _ _ ) left right) id2
              | id1 == id2 = Just utente
              | id1 > id2 = getUtenteById left id2
              | id1 < id2 = getUtenteById right id2

getLeafUtenteById :: BST_Utentes -> Int -> BST_Utentes
getLeafUtenteById BSTree_Empty _ = BSTree_Empty
getLeafUtenteById tree@(Leaf utente@(Utente _ id1 _ _ _ _ _ ) left right) id2
              | id1 == id2 = tree 
              | id1 > id2 = getLeafUtenteById left id2
              | id1 < id2 = getLeafUtenteById right id2
{- === *FIM ARVORE UTENTES* === -----------------------------------------}

{- 
   =========
   ==
   == *Instancias*
   ==
   =========
-}
instance Eq Utente where 
	(==)   = iguala
	
instance Ord Utente where
	(>) = maior
	(<) = menor
	(>=) = maiorIgual
	(<=) = menorIgual

	
iguala :: Utente -> Utente -> Bool 
iguala (Utente _ id1 _ _ _ _ _) (Utente _  id2 _ _ _ _ _)  = id1 == id2

maior :: Utente -> Utente -> Bool
maior (Utente _ id1 _ _ _ _ _) (Utente _  id2 _ _ _ _ _)  = id1 > id2

menor :: Utente -> Utente -> Bool
menor (Utente _ id1 _ _ _ _ _) (Utente _  id2 _ _ _ _ _)  = id1 < id2


maiorIgual :: Utente -> Utente -> Bool
maiorIgual (Utente _ id1 _ _ _ _ _) (Utente _  id2 _ _ _ _ _)  = id1 >= id2

menorIgual :: Utente -> Utente -> Bool
menorIgual (Utente _ id1 _ _ _ _ _) (Utente _  id2 _ _ _ _ _)  = id1 <= id2
{- 
   =========
   ==
   == +FIM Instancias*
   ==
   =========
-}



{-
	===============================================
	================ *FIM Utente* =================
	===============================================
-}