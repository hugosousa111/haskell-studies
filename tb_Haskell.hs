-- Lista 01 -------------------------------

-- Questão 01-----------------------------------

inverteLetra :: Char -> Char
inverteLetra var 
 | var == 'a' = 'A'
 | var == 'b' = 'B'
 | var == 'c' = 'C'
 | var == 'd' = 'D'
 | var == 'e' = 'E'
 | var == 'f' = 'F'
 | var == 'g' = 'G'
 | var == 'h' = 'H'
 | var == 'i' = 'I'
 | var == 'j' = 'J'
 | var == 'k' = 'K'
 | var == 'l' = 'L'
 | var == 'm' = 'M'
 | var == 'n' = 'N'
 | var == 'o' = 'O'
 | var == 'p' = 'P'
 | var == 'q' = 'Q'
 | var == 'r' = 'R'
 | var == 's' = 'S'
 | var == 't' = 'T'
 | var == 'u' = 'U'
 | var == 'v' = 'V'
 | var == 'x' = 'X'
 | var == 'w' = 'W'
 | var == 'y' = 'Y'
 | var == 'z' = 'Z'
 | otherwise = var

 --Questao 02----------------------------------

caracNum :: Char -> Int
caracNum c 
 | c == '1' = 1
 | c == '2' = 2
 | c == '3' = 3
 | c == '4' = 4
 | c == '5' = 5
 | c == '6' = 6
 | c == '7' = 7
 | c == '8' = 8
 | c == '9' = 9
 | otherwise = 0

--Questao 03----------------------------------------

potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x n = potencia x (n-1) * x

--Questao 04--------------------------------------

power :: Int -> Int -> Int
power x 0 = 1
power x n = power x (n-1) * x

--Questao 05-------------------------------------

qa_5 :: Int -> Int -> Int -> Int -> Int
qa_5 a b c d = ( div (a+b+c+d) 4)

qb_5 :: Int -> Int -> Int -> (Int, Int) 
qb_5 a b c = (max (max a b) c, min (min a b) c)

qc_5 :: Int -> Int -> Int -> (Int, Int, Int) 
qc_5 a b c = (min (min a b) c, if a > min (min a b) c && a < max (max a b) c then a else if b > min (min a b) c && b < max (max a b) c then b else c , max (max a b) c)

--Questao 06-------------------------------------

repVar :: Int -> String -> String 
repVar 0 s = ""
repVar n s = s ++ repVar (n-1) s

--Questao 07-----------------------------------

unico :: [Int] -> [Int]
unico [] = []
unico [a] = [a]
unico (cab:corp) = cab:(unico $ filter (/=cab) corp)

--Questao 08-------------------------------------

intercala :: var -> [var] -> [[var]]
intercala n [] = [[n]]
intercala n (cab:corp) = (n:cab:corp) : [cab:z | z <- intercala n corp]

--Questao 09---------------------------------

func_conta_10 :: [Int] -> Int
func_conta_10 [] = 0 
func_conta_10 (a:b) | (a > 10) = 1 + func_conta_10 b | otherwise = func_conta_10 b

--Questao 10------------------------------------

func_pega_10 :: [Int] -> [Int]
func_pega_10 [] = [] 
func_pega_10 (a:b) | (a > 10) = [a]++func_pega_10 b | otherwise = func_pega_10 b

--Questao 11--------------------------------

multiplica_listas :: [Int] -> [Int] -> [Int]
multiplica_listas _ [] = []
multiplica_listas [] _ = []
multiplica_listas (a:b) (c:d) = [a*c] ++ multiplica_listas [a] d ++ multiplica_listas b (c:d)

-------------------------------------------