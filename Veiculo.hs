module Veiculo where



import System.Time
import Data.List
--import 'Estruturas.DLL'

{-
	===============================================
	============== *TESTE Veiculo* ================
	===============================================
-}
v1 :: Veiculo
v1 = ("Honda","EK9", "SPOON-01", PASSAGEIROS, "Sei la")

v2 :: Veiculo
v2 = ("Mazda","RX7FC", "FC-01", PASSAGEIROS, "Nao faco ideia")

v3 :: Veiculo
v3 = ("Nissan","Skyline", "NISMO-01", PASSAGEIROS, "Nao faco ideia tambem")


{-
	===============================================
	============ *FIM TESTE Veiculo* ==============
	===============================================
-}

------------------------------------------------------------------

{-
	===============================================
	================= *Veiculo* ===================
	===============================================
-}

type Marca = String
type Modelo = String
type Matricula = String
data Tipo_Veiculo = PASSAGEIROS | MOTA | AMBULANCIA | TAXI
     deriving (Read, Show)
type Hora_Entrada = String

type Veiculo = (Marca,Modelo, Matricula, Tipo_Veiculo, Hora_Entrada)





{-
	===============================================
	================= *Veiculo* ===================
	===============================================
-}