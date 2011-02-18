import Time

{-
checkTeste :: PontoAcesso -> IO String
checkTeste PontoAcesso_Empty = return "Ponto de Acesso Vazio"
checkTeste p | True = return "Inserido"
             | otherwise = return "Erro"
             where
                show "hello"
-}



main = do
            t_inicial <- getClockTime
            i <- getLine
            t_final <- getClockTime
            putStrLn $ show $ tdSec $ diffClockTimes t_final t_inicial
            return ()