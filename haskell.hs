import Prelude

answer :: Int
answer = 42

difList :: [Int] -> [Int] -> [Int]
difList [] [] = []
difList a b = [e | e <- a, not (elem e b)]

interList :: [Int] -> [Int] -> [Int]
interList [] [] = []
interList a b = [e | e <- a, (elem e b)]

unionList :: [Int] -> [Int] -> [Int]
unionList [] [] = []
unionList a b = [e | e <- a ++ b]

unionListNoRep :: [Int] -> [Int] -> [Int]
unionListNoRep [] [] = []
unionListNoRep a b = removerRep (unionList a b)
                                 where removerRep [] = []
                                       removerRep (a:as) | elem a as = removerRep as
                                                         | otherwise = a : removerRep as

returnLast :: [Int] -> Int
returnLast [] = 0
returnLast (x:xs) | length (x:xs) == 1 = x
                  | otherwise = returnLast xs

returnNth :: [Int] -> Int -> Int
returnNth a 1 = head a
returnNth (a:as) n | n > 1 = returnNth as (n-1)

ultimoElemento [a] = a
ultimoElemento (a:as) = ultimoElemento as

revertList :: [Int] -> [Int]
revertList [] = []
revertList (a:as) = (revertList as) ++ [a]

decrescenteList :: [Int] -> [Int]
decrescenteList [] = []
decrescenteList (a:as) = decrescenteList [e | e <- as, e > a] ++ [a] ++ decrescenteList [e | e <- as, e < a]

isDec :: [Int] -> Bool
isDec x = x == (decrescenteList x)

isDec2 :: [Int] -> Bool
isDec2 [a] = True
isDec2 (a:b:xs) = a > b && (isDec xs)


fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z [] = z
fold f z (x:xs) = f x (foldr f z xs)

-- isDecF :: (a -> b -> b) -> b -> [a] -> Bool
-- isDecF _ _ [] = True
-- isDecF f z xs = fold (&&) (map (\(a,b) -> a>=b) (zip xs (tail xs)))

-- ehDescrescente3 lista =  fold (&&) (map (\(a,b) -> a>=b) (zip lista (tail lista)) )

{--------------------------------------Modelo de Dados---------------------------------------------------}
type Medicamento = String

type Quantidade = Int

type Horario = Int

type EstoqueMedicamentos = [(Medicamento, Quantidade)]

type Prescricao = (Medicamento, [Horario])

type Receituario = [Prescricao]

type PlanoMedicamento = [(Horario, [Medicamento])]

type Plantao = [(Horario, [Cuidado])]

data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

instance Show Cuidado where
  show (Comprar m q) =
    "Comprar "
      ++ Prelude.show q
      ++ " comprimido(s) do medicamento: "
      ++ m
  show (Medicar m) = "Ministrar medicamento: " ++ m

med1 :: Medicamento
med1 = "Adera"

med2 :: Medicamento
med2 = "Alprazolam"

med3 :: Medicamento
med3 = "Donepezila"

med4 :: Medicamento
med4 = "Lactulona"

med5 :: Medicamento
med5 = "Mirtazapina"

med6 :: Medicamento
med6 = "Pantoprazol"

med7 :: Medicamento
med7 = "Patz"

med8 :: Medicamento
med8 = "Quetiapina"

med9 :: Medicamento
med9 = "Xarelto"

estoque1 :: EstoqueMedicamentos
estoque1 = [(med4, 10), (med6, 5), (med7, 0)]

estoque2 :: EstoqueMedicamentos
estoque2 = [(med4, 10), (med6, 5), (med7, 10)]

estoque3 :: EstoqueMedicamentos
estoque3 = [(med4, 10), (med6, 50), (med7, 10), (med8, 20)]

receituario1 :: Receituario
receituario1 = [(med4, [8, 17]), (med6, [6]), (med7, [22])]

receituario2 :: Receituario
receituario2 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 22, 23])]

receituarioInvalido1 :: Receituario
-- Invalido: horário não está ordenado de forma crescente
receituarioInvalido1 = [(med4, [22, 10])]

receituarioInvalido2 :: Receituario
-- Invalido: medicamentos não estão ordenbados de forma crescente
receituarioInvalido2 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med2, [8, 22, 23])]

receituarioInvalido3 :: Receituario
-- Invalido: medicamentos não estão ordenbados de forma crescente
receituarioInvalido3 = [(med4, [8, 17]), (med3, [6]), (med7, [22])]

receituarioInvalido4 :: Receituario
-- Invalido: horário não estão ordenbados de forma crescente
receituarioInvalido4 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 23, 22])]

plano1 :: PlanoMedicamento
plano1 = [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]

plano2 :: PlanoMedicamento
plano2 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med7, med8]), (23, [med8])] :: [(Int, [String])]

planoInvalido1 :: PlanoMedicamento
-- Invalido: horário não está ordenado de forma crescente
planoInvalido1 = [(8, [med4]), (6, [med6]), (17, [med4]), (22, [med7])]

planoInvalido2 :: PlanoMedicamento
-- Invalido: medicamentos não estão ordenbados de forma crescente
planoInvalido2 = [(6, [med6]), (8, [med8, med4]), (17, [med4]), (22, [med7, med8]), (23, [med8])] :: [(Int, [String])]

planoInvalido3 :: PlanoMedicamento
-- Invalido: medicamentos não estão ordenbados de forma crescente
planoInvalido3 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (22, [med8, med7]), (23, [med8])] :: [(Int, [String])]

planoInvalido4 :: PlanoMedicamento
-- Invalido: horário não está ordenado de forma crescente
planoInvalido4 = [(6, [med6]), (8, [med4, med8]), (17, [med4]), (23, [med8]), (22, [med7, med8])] :: [(Int, [String])]

plantao1 :: Plantao
plantao1 =
  [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med4]),
    (22, [Medicar med7])
  ]

plantao2 :: Plantao
plantao2 =
  [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantao3 :: Plantao
plantao3 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido1 :: Plantao
plantaoInvalido1 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    (22, [Medicar med7]),
    -- Invalido: não está em horario crescente
    (17, [Medicar med4, Comprar med7 30])
  ]

plantaoInvalido2 :: Plantao
plantaoInvalido2 =
  [ (6, [Medicar med6, Medicar med9]),
    (8, [Medicar med2, Medicar med4]),
    -- Invalido: comprar o mesmo medicamento no horário
    (17, [Medicar med4, Comprar med4 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido3 :: Plantao
plantaoInvalido3 =
  [ (6, [Medicar med6, Medicar med9]),
    -- Invalido: ordem dos medicamentos não está ordenada
    (8, [Medicar med4, Medicar med2]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]

plantaoInvalido4 :: Plantao
plantaoInvalido4 =
  [ (6, [Medicar med6]),
    -- Invalido: comprar o mesmo medicamento no horário
    (8, [Comprar med4 20, Medicar med4, Medicar med8]),
    (17, [Medicar med4]),
    (22, [Medicar med7, Medicar med8]),
    (23, [Medicar med8])
  ]


{--------------------------------------Modelo de Dados---------------------------------------------------}