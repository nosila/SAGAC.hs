module TesteIO where

type Turma = [(Int, Float, String)]

haha :: Turma
haha = [(1, 2, "Ponto Norte"),
		(2, 3, "Ponto Sul"),
		(3, 2, "Ponto Este"),
		(4, 5, "Ponto Oeste")]

menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
          where menutxt = unlines ["",
                                   "Inserir Aluno .........1",
                                   "Listar Alunos .........2",
                                   "Procurar Aluno ........3",
                                   "",
                                   "Sair ..................0"]
                                   
ciclo :: Turma -> IO ()
ciclo t = do { op <- menu
             ; case op of
                '1':_ -> do { t'<- insereAluno t
                            ; ciclo t'
                            }
                '2':_ -> do { listar t
                            ; ciclo t
                            }
                '0':_ -> return ()
                otherwise -> do { putStrLn "Opcao Invalida"
                                ; ciclo t
                                }
             }

insereAluno :: Turma -> IO Turma
insereAluno t = do { putStr "Numero: "
                   ; nu <- getLine
                   ; putStr "Nota: "
                   ; nt <- getLine
                   ; putStr "Nome: "
                   ; no <- getLine
                   ; return ((read nu, read nt, no):t)
                   }

listar :: Turma -> IO ()
listar turma@(h@(a,b,c):t) = do
    putStr ("Numero: " ++ (show a) ++ " Nota: " ++ (show b) ++ " Nome: " ++ c)
    return ()
                 