module PontoAcesso
( paInserir
, paRemover
, paRemoverById
, paRemoverByName
, pAlterarById
, paAlterarByName
, paListar
, paListarTodos
, paPesquisar
, paGetIdByName
, paGetByName
, paGetPermsVeiculo
, paGetPermsUtente
, ListaPontoAcesso
, PontoAcesso (..)
, VeiculoPerms (..)
, UtentePerms(..)
, pontos
, paRegistarEntrada
, paRegistarSaida
, paTemPerms
, paExiste
, paIsLotado
, paIsVago
, paWriteToLog
, paIsEmpty
, paGetValor
, paPermiteVeiculo
, paPermiteUtente
) where

import System.Time
import Parque
import Veiculo

-- Types
type ID 			    = Int
type Nome 			    = String
type Descricao 		    = String

type Passageiros        = Bool
type Ambulancia         = Bool
type Mota               = Bool
type Taxi               = Bool

type Morador            = Bool
type Turista            = Bool
type Motorista          = Bool
type FuncionarioCamara  = Bool
type FuncionarioLimpeza = Bool

data VeiculoPerms = VPerm_Empty | VPerm Passageiros Ambulancia Mota Taxi
					deriving (Show, Read)
data UtentePerms  = UPerm_Empty | UPerm Morador Turista Motorista FuncionarioCamara FuncionarioLimpeza
					deriving (Show, Read)
data PontoAcesso = PontoAcesso_Empty | PAcesso ID Nome Descricao Parque VeiculoPerms UtentePerms
    deriving (Show, Read)

type ListaPontoAcesso = [PontoAcesso]    	  -- Lista

pontos :: ListaPontoAcesso
pontos = [(PAcesso 1 "Ponto A" "Desc Ponto A" Parque_Empty (VPerm True True True False) UPerm_Empty),
          (PAcesso 2 "Ponto B" "Desc Ponto B" Parque_Empty VPerm_Empty UPerm_Empty),
          (PAcesso 3 "Ponto C" "Desc Ponto C" Parque_Empty (VPerm True True True False) UPerm_Empty),
          (PAcesso 4 "Ponto D" "Desc Ponto D" Parque_Empty VPerm_Empty UPerm_Empty)]

-- Functions
paInserir :: PontoAcesso -> ListaPontoAcesso -> ListaPontoAcesso
paInserir PontoAcesso_Empty _ = []
paInserir p [] = [p]
paInserir p l = l ++ [p]

paRemover :: PontoAcesso -> ListaPontoAcesso -> ListaPontoAcesso
paRemover PontoAcesso_Empty _ = []
paRemover _ [] = []
paRemover p@(PAcesso id _ _ _ _ _) l = filter (\(PAcesso x _ _ _ _ _)-> x/=id) l

paRemoverById :: ID -> ListaPontoAcesso -> ListaPontoAcesso
paRemoverById _ [] = []
paRemoverById _ ((PontoAcesso_Empty):t) = []
paRemoverById id l = filter (\(PAcesso x _ _ _ _ _)-> x /= id) l

paRemoverByName :: String -> ListaPontoAcesso -> ListaPontoAcesso
paRemoverByName _ [] = []
paRemoverByName nome ((PontoAcesso_Empty):t) = filter (\(PAcesso _ x _ _ _ _)-> x /= nome) t
paRemoverByName _ l@(h@(PAcesso _ n _ Parque_Empty VPerm_Empty UPerm_Empty):t) = filter (\(PAcesso _ x _ _ _ _)-> x /= n) l
paRemoverByName nome l = filter (\(PAcesso _ x _ _ _ _)-> x /= nome) l

{-paAlterarById :: ID -> PontoAcesso -> ListaPontoAcesso -> ListaPontoAcesso
paAlterarById _ _ ((PontoAcesso_Empty):t) = []
paAlterarById _ PontoAcesso_Empty [] = error "erro1"
paAlterarById _ PontoAcesso_Empty _ = error "erro2"
paAlterarById _ _ [] = error "erro3"
paAlterarById n p@(PAcesso id nom des _ _ _) l@(p2@(PAcesso id2 _ _ _ _ _):t) | n == id2 = (paRemoverById n l)++[p]
                                                                              | otherwise = p2:(paAlterarById n p t)-}
                                                                              
pAlterarById :: ID -> PontoAcesso -> ListaPontoAcesso -> ListaPontoAcesso
pAlterarById _ PontoAcesso_Empty l = l
pAlterarById _ p [] = [p]
pAlterarById _ p ((PontoAcesso_Empty):t) = [p]
pAlterarById n p@(PAcesso id nom desc _ _ _) l@(p2@(PAcesso id2 _ _ _ _ _):t) | n == id2 = (paRemoverById n l)++[p]
                                                                              | otherwise = p2:(pAlterarById n p t)
                                                                              
paAlterarByName :: Nome -> PontoAcesso -> ListaPontoAcesso -> ListaPontoAcesso
paAlterarByName _ PontoAcesso_Empty _ = []
paAlterarByName _ _ [] = []
paAlterarByName n p@(PAcesso id nom des _ _ _) (p2@(PAcesso _ nome _ _ _ _):t) | n == nome = t
											   | otherwise = p2:(paAlterarByName n p t)
                                              
                                          
paListar :: PontoAcesso -> Nome
paListar PontoAcesso_Empty = ""
paListar (PAcesso id nome desc _ _ _) = "ID: " ++ (show id) ++ " - Nome: " ++ nome ++ " - Descricao: " ++ desc

paListarTodos :: ListaPontoAcesso -> [Nome]
paListarTodos [] = []
paListarTodos (p:t) = (paListar p):paListarTodos t

paPesquisar :: ID -> ListaPontoAcesso -> Nome
paPesquisar _ [] = ""
paPesquisar id (h@(PAcesso id1 _ _ _ _ _):t) | id == id1 = paListar h
							   | otherwise = paPesquisar id t

paGetIdByName :: Nome -> ListaPontoAcesso -> ID
paGetIdByName _ [] = 0
paGetIdByName nome (h@(PAcesso id n _ _ _ _):t) = if nome == n then id else (paGetIdByName nome t)

paGetByName :: Nome -> ListaPontoAcesso -> PontoAcesso
paGetByName _ [] = PontoAcesso_Empty
paGetByName n ((PontoAcesso_Empty):t) = (paGetByName n t)
paGetByName n (h@(PAcesso id nome _ _ _ _):t) | n == nome = h
                                              | otherwise = (paGetByName n t)									  

paGetPermsVeiculo :: PontoAcesso -> VeiculoPerms
paGetPermsVeiculo PontoAcesso_Empty = VPerm_Empty
paGetPermsVeiculo (PAcesso _ _ _ _ VPerm_Empty _) = VPerm_Empty
paGetPermsVeiculo (PAcesso _ _ _ _ p@(VPerm a b c d) _) = p

paGetPermsUtente :: PontoAcesso -> UtentePerms
paGetPermsUtente PontoAcesso_Empty = UPerm_Empty
paGetPermsUtente (PAcesso _ _ _ _ _ UPerm_Empty) = UPerm_Empty
paGetPermsUtente (PAcesso _ _ _ _ _ p@(UPerm a b c d e)) = (UPerm a b c d e)

paRegistarEntrada :: ListaPontoAcesso -> PontoAcesso -> Veiculo -> ListaPontoAcesso
paRegistarEntrada [] PontoAcesso_Empty _ = []
paRegistarEntrada ((PontoAcesso_Empty):t) p v = paRegistarEntrada t p v
paRegistarEntrada l p@(PAcesso id nome desc parque vperm uperm) v = (paRemoverById id l)++[(PAcesso id nome desc (pqAddVeiculo v parque) vperm uperm)]

paRegistarSaida :: ListaPontoAcesso -> PontoAcesso -> Matricula -> ListaPontoAcesso
paRegistarSaida [] PontoAcesso_Empty _ = []
paRegistarSaida ((PontoAcesso_Empty):t) p m = paRegistarSaida t p m
paRegistarSaida l@(h:t) p@(PAcesso id nome desc parque vperm uperm) m | pqHasVeiculo m parque = (paRemoverById id l)++[(PAcesso id nome desc (pqRemoveVeiculo m parque) vperm uperm)]
                                                                      | otherwise = l

paTemPerms :: PontoAcesso -> VeiculoPerms -> Bool
paTemPerms PontoAcesso_Empty VPerm_Empty = False

paPermiteVeiculo :: PontoAcesso -> Int -> Bool
paPermiteVeiculo PontoAcesso_Empty _ = False
paPermiteVeiculo (PAcesso _ _ _ _ VPerm_Empty _) _ = False
paPermiteVeiculo (PAcesso _ _ _ _ (VPerm a b c d) _) t = if t==1 then a else if t==2 then b else if t==3 then c else if t==4 then d else False

paPermiteUtente :: PontoAcesso -> Int -> Bool
paPermiteUtente PontoAcesso_Empty _ = False
paPermiteUtente (PAcesso _ _ _ _ _ UPerm_Empty) _ = False
paPermiteUtente (PAcesso _ _ _ _ _ (UPerm a b c d e)) t = if t==1 then a else if t==2 then b else if t==3 then c else if t==4 then d else if t==5 then e else False

paExiste :: Nome -> ListaPontoAcesso -> Bool
paExiste _ [] = False
paExiste nome ((PontoAcesso_Empty):t) = paExiste nome t
paExiste nome (h@(PAcesso _ n _ _ _ _):t) | nome == n = True
                                          | otherwise = paExiste nome t

paIsLotado :: String -> ListaPontoAcesso -> Bool
paIsLotado _ [] = True
paIsLotado nome ((PontoAcesso_Empty):t) = paIsLotado nome t
paIsLotado nome (h@(PAcesso _ n _ p _ _):t) | nome == n = pqIsLotado p
                                            | otherwise = paIsLotado nome t
                                          
paIsVago :: String -> ListaPontoAcesso -> Bool
paIsVago _ [] = False
paIsVago nome ((PontoAcesso_Empty):t) = paIsVago nome t
paIsVago nome (h@(PAcesso _ n _ p _ _):t) | nome == n = pqIsVago p
                                          | otherwise = paIsVago nome t
                                          
paWriteToLog :: String -> String -> IO ()
paWriteToLog time tipo = appendFile "log.dat" ("" ++ time ++ " - " ++ tipo ++ "\n")

paIsEmpty :: ListaPontoAcesso -> Bool
paIsEmpty [] = True
paIsEmpty [PontoAcesso_Empty] = True
paIsEmpty l | length l >= 1 = False
            | otherwise = True
            
paGetValor :: PontoAcesso -> Int -> Matricula -> Int
paGetValor PontoAcesso_Empty _ _= 0
paGetValor (PAcesso _ _ _ p _ _) fim mat = (fim - (pqGetHoraEntrada mat p)) * 2 -- Euros