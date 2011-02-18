module Parque
( pqGetOcupacao
, pqIsLotado
, pqIsVago
, pqAddVeiculo
, pqRemoveVeiculo
, pqHasVeiculo
, pqGetHoraEntrada
, Parque(..) ) where

-- Imports
import Veiculo

-- Types
type Capacidade     = Int
type ListaVeiculo 	= [Veiculo]
type NumAmbulancia 	= Int
type NumTaxi 		= Int
type NumPassageiros = Int
type NumMota 		= Int
data Parque = Parque_Empty | Parque Capacidade ListaVeiculo NumAmbulancia NumTaxi NumPassageiros NumMota
    deriving (Show, Read)

parque :: Parque
parque = Parque_Empty

-- Functions
pqGetOcupacao :: Parque -> Int
pqGetOcupacao Parque_Empty = 0
pqGetOcupacao (Parque _ l _ _ _ _) = length l

pqIsLotado :: Parque -> Bool
pqIsLotado Parque_Empty = True
pqIsLotado p@(Parque c _ _ _ _ _) = pqGetOcupacao p == c

pqIsVago :: Parque -> Bool
pqIsVago Parque_Empty = False
pqIsVago p@(Parque c _ _ _ _ _) = pqGetOcupacao p < c

pqAddVeiculo :: Veiculo -> Parque -> Parque
pqAddVeiculo _ Parque_Empty = Parque_Empty
pqAddVeiculo v p@(Parque c l na nt np nm) 
            | pqIsVago p = (Parque c (v:l) na nt np nm)
			| otherwise = p
            
pqRemoveVeiculo :: Matricula -> Parque -> Parque
pqRemoveVeiculo _ Parque_Empty = Parque_Empty
pqRemoveVeiculo m p@(Parque c l a1 a2 a3 a4) = (Parque c (filter (\(_,mat,_,_,_)->mat/=m) l) a1 a2 a3 a4)

pqHasVeiculo :: Matricula -> Parque -> Bool
pqHasVeiculo _ Parque_Empty = False
pqHasVeiculo _ (Parque _ [] _ _ _ _) = False
pqHasVeiculo m p@(Parque _ l _ _ _ _) | length (filter (\(_,mat,_,_,_)->mat==m) l) > 0 = True
                               | otherwise = False
                               
pqGetHoraEntrada :: Matricula -> Parque -> Int
pqGetHoraEntrada _ Parque_Empty = 0
pqGetHoraEntrada _ (Parque _ [] _ _ _ _) = 0
pqGetHoraEntrada mat p@(Parque _ l _ _ _ _) = read(unwords (map (\(_,_,_,_,x)->x) (filter (\(_,m,_,_,_)->mat==m) l)))::Int
