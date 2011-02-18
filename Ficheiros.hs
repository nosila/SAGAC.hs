module Ficheiros where

type Nome = String
type Idade = Int
type Nota = Float
type Aluno = (Nome, Idade, Nota)

type Turma = [Aluno]

turma :: Turma
turma = [("Aluno1", 19, 20),
         ("Aluno2", 20, 20)]

