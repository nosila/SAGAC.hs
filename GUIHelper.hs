module GUIHelper where

-- Imports
import System.Cmd
import Data.Char
import Data.Maybe

-- Types
type Titulo = String
type Index  = Int
type Text   = String

-- Linha em branco para o calculo de espacos por preencher
blankLine = "                                                                             "
clearScreen = system "cls"  

pause :: IO ()
pause = do 
            putStr $ "Press any key to continue . . . "
            getLine
            return ()

-- Desenha um Header
drawHeader :: Titulo -> IO ()
drawHeader t = do { clearScreen
                  ; putStr txt
                  } where txt = unlines ["|-----------------------------------------------------------------------------|",
                                         "|                                     SAGAC                                   |",
                                         "|                                                                             |",
                                         "| - "++t++(take (74 - (length t)) blankLine)++"|",
                                         "|-----------------------------------------------------------------------------|",
                                         "|                                                                             |"]

-- Desenha um Item para seleccao                                             
drawItem :: Index -> Text -> IO ()
drawItem i t = do { putStr txt
                  } where txt = "|   ["++ show i ++"] - "++t++(take (68 - (length t)) blankLine)++"|\n"
                  
drawBlankLine = putStr "|                                                                             |\n"
drawCloseLine = putStr "|-----------------------------------------------------------------------------|\n"            

drawValue:: Show( a) => String -> a -> IO ()
drawValue p v = putStr ("|   "++p++": "++(show v)++(take (72 - ((length p) + (length (show v)))) blankLine)++"|\n")
          
drawGetInt :: String -> IO Int
drawGetInt q = do
    putStrLn $ q
    input <- getLine
    case reads input of
        [(value, "")] | value > 0 -> return value
        _ -> drawGetInt q
          
drawGetBool :: String -> IO Bool
drawGetBool q = do
    putStrLn $ q
    input <- getLine
    case reads (show input ) of
        [(value, "")] | value == "True" || value == "Sim" || value == "S" || value == "1" -> return True
                      | value == "False" || value == "Nao" || value == "N" || value == "0" -> return False
        _ -> drawGetBool q
        
drawGetHora :: String -> IO Int
drawGetHora q = do
    putStrLn $ q
    input <- getLine
    case reads input of
        [(value, "")] | value >= 0 && value <= 23 -> return value
        _ -> drawGetHora q
        
drawGetMoney :: String -> Int -> IO Int
drawGetMoney q n = do
    putStrLn $ q
    input <- getLine
    case reads input of
        [(value, "")] | value == n -> return value
        _ -> drawGetMoney q n