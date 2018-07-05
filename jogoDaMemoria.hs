import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import System.Random

main :: IO()
main = do
	menu

-- Imprime o menu principal recebe a opcao selecionada
menu :: IO()
menu = do 
	system "clear"
	putStrLn(" Menu:\n\n" ++
            "1. Iniciar Jogo\n" ++
            "2. Ranking\n" ++
            "3. Sair\n\n")
	dados <- getLine
	let opcao = read(dados)
	if  opcao == 1 then 
		nivel
	else if opcao == 2 then
		ranking
	else if opcao == 3 then
		putStrLn "oi"
	else
		menu

-- Verifica se o elemento encontra-se no vetor
containsVetor :: [String] -> String -> Bool
containsVetor [] _ = False
containsVetor (x:xs) y | x == y = True
					   | xs == [] = False
					   | x /= y = containsVetor xs y

-- Exibicao do Ranking
ranking :: IO()
ranking = do
	putStrLn("Marcos 2m")

-- Exibe menu de niveis e recebe o nivel selecionado
nivel :: IO()
nivel = do
	system "clear"
	putStrLn(" Escolha um dos niveis:\n\n" ++
			"1. Iniciante\n" ++
			"2. Intermediario\n" ++
			"3. Avancado\n\n")
	dados <- getLine
	let nivels = read(dados)
	if nivels == 1 then
		matrizPrint nivels caracteres
	else if nivels == 2 then
		matrizPrint nivels caracteres
	else if nivels == 3 then
		matrizPrint nivels caracteres
	else
		nivel

-- Retorna uma lista de caracteres
caracteres :: [String]
caracteres = [ "◒", "◕", "◔", "◐", "☎", "☂", "☀", "☢", "☣", "☹", "☯", "☩", "☠", "☸", "♛", "♚", "♜", "♝", "♞", "♡", "✿", "✻", "⊳", "⊖", "➸", "➱", "❤" , "✸", "✖", "✔", "♫", "♬", "∞", "✂", "✈"]

 -- Converte Integer para Int
integerToInt :: Integer -> Int
integerToInt x = fromIntegral x :: Int

-- Retorna uma lista de caracteres aleatorios a partir quantidade de pares do nivel
caracteresRandom :: [String] -> [String] -> Int -> IO [String]
caracteresRandom carac retorno i | i > 0 = do
							x <- randomRIO(0, 34)
							let rand = integerToInt x
							let elem = carac !! rand
							if containsVetor retorno elem then
								caracteresRandom carac retorno i 
							else
								caracteresRandom carac (elem:retorno) (i-1)
						| i <= 0 = do
							return retorno

-- Retorna uma matriz vazia de tamanho x
geraMatriz :: Int -> [[String]]
geraMatriz x = replicate x (replicate x "")

-- Insere um elemento na matriz
insereElementoMatriz :: Int -> Int -> Int -> String -> [[String]] -> [[String]] -> [[String]]
insereElementoMatriz tamanho linha coluna elem matriz retorno = do
	if tamanho > 0 && linha /= tamanho && (retorno !! 0) == [] then insereElementoMatriz (tamanho-1) linha coluna elem matriz (((matriz !! (tamanho-1)) ++ (retorno !! 0)):[])
	else if tamanho > 0 && linha /= tamanho then insereElementoMatriz (tamanho-1) linha coluna elem matriz ((matriz !! (tamanho-1)):retorno)
	else if tamanho > 0 && linha == tamanho && retorno !! 0 == [] then insereElementoMatriz (tamanho-1) linha coluna elem matriz (((insereElementoVetor (coluna-1) elem (matriz !! (linha-1))) ++ (retorno !! 0)):[])
	else if tamanho > 0 && linha == tamanho then insereElementoMatriz (tamanho-1) linha coluna elem matriz ((insereElementoVetor (coluna-1) elem (matriz !! (linha-1))):retorno)
	else retorno

-- Insere um elemnento na lista
insereElementoVetor :: Int -> String -> [String] -> [String]
insereElementoVetor pos elem array = ((take pos array) ++ [elem] ++ (drop (pos+1) array))

--  matrizRandomica :: Int -> Int -> [String] -> [[String]] -> IO [[String]]
--  matrizRandomica i j (x:xs) matrizRetorno	| i > 0 = do

-- Retorna uma posicao aleatoria
posicaoRandom :: Int -> IO [Int]
posicaoRandom tamanho = do
	x <- randomRIO(1, tamanho)
	y <- randomRIO(1, tamanho)
	return [x, y]

-- Verifica se uma posicao esta vazia
posicaoValida :: [Int] -> [[String]] -> IO Bool
posicaoValida posicao  matriz = do
	let linha = (posicao!!0)-1
	let coluna = (posicao!!1)-1
	if (matriz !! linha !! coluna == "") then 
		return True
	else return False

--Insere um elemento de acordo com a validacao da posicao 
insereElemento :: Int -> String -> [[String]] -> IO [[String]]
insereElemento tamanho elemento matriz = do
	posicao <- (posicaoRandom tamanho)
	let linha = fromEnum(posicao !! 0)
	let coluna = fromEnum(posicao !! 1)
	condicao <- posicaoValida posicao matriz
	if (condicao) then
		return(insereElementoMatriz tamanho linha coluna elemento matriz [[]])
	else
		insereElemento tamanho elemento matriz					
 
 -- Insere um elemento em duas posicoes diferentes na matriz						
inserePares :: Int -> String  -> [[String]] -> IO [[String]]
inserePares tamanho elem matriz = do
	 novaMatriz <- insereElemento tamanho elem matriz
	 retorno <- insereElemento tamanho elem novaMatriz
	 return retorno

-- Cria a matriz do usuario de acordo com o nivel
matrizUsuario :: Int -> String
matrizUsuario 1 = matrizRepresentacao (replicate 4 (replicate 4 "X")) 1
matrizUsuario 2 = matrizRepresentacao (replicate 6 (replicate 6 "X")) 1
matrizUsuario 3 = matrizRepresentacao (replicate 8 (replicate 8 "X")) 1

-- Imprime a representacao de uma matriz
matrizPrint :: Int -> [String] -> IO()
matrizPrint x carac = do
				putStrLn (primeiraLinha x)
				putStrLn (matrizUsuario x)

-- Retorna uma string de posicoes da primeira linha
primeiraLinha :: Int -> String
primeiraLinha x | x == 1 = "   1 2 3 4"
				| x == 2 = "   1 2 3 4 5 6"
				| x == 3 = "   1 2 3 4 5 6 7 8"
				| otherwise = ""

-- Retorna a representacao de uma matriz na forma de String
matrizRepresentacao :: [[String]] -> Int -> String
matrizRepresentacao [] z = ""
matrizRepresentacao (x:xs) z = " " ++ show z ++ vetorRepresentacao x ++ "\n" ++ matrizRepresentacao xs (z+1) 

--Retorna a representacao de uma lista na forma de String
vetorRepresentacao :: [String] -> String
vetorRepresentacao [] = ""
vetorRepresentacao (x:xs) = " " ++ x ++ vetorRepresentacao xs