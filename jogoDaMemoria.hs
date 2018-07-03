import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import System.Random

main :: IO()
main = do
	menu

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

containsVetor :: [String] -> String -> Bool
containsVetor [] _ = False
containsVetor (x:xs) y | x == y = True
					   | xs == [] = False
					   | x /= y = containsVetor xs y

ranking :: IO()
ranking = do
	putStrLn("Marcos 2m")

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
		vetorPrint caracteres 25
	else if nivels == 3 then
		matrizPrint nivels caracteres
	else
		nivel

caracteres :: [String]
caracteres = [ "◒", "◕", "◔", "◐", "☎", "☂", "☀", "☢", "☣", "☹", "☯", "☩", "☠", "☸", "♛", "♚", "♜", "♝", "♞", "♡", "✿", "✻", "⊳", "⊖", "➸", "➱", "❤" , "✸", "✖", "✔", "♫", "♬", "∞", "✂", "✈"]

vetorPrint :: [String] -> Int -> IO()
vetorPrint carac x = do 
				vetor <- caracteresRandom carac [] x
				putStrLn(vetorRepresentacao vetor)

integerToInt :: Integer -> Int
integerToInt x = fromIntegral x :: Int

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

geraMatriz :: Int -> [[String]]
geraMatriz x = replicate x (replicate x "")

insereElementoMatriz :: Int -> Int -> Int -> String -> [[String]] -> [[String]] -> [[String]]
insereElementoMatriz tamanho linha coluna elem matriz retorno | tamanho > 0 && linha /= tamanho && (retorno !! 0) == [] = insereElementoMatriz (tamanho-1) linha coluna elem matriz (((matriz !! (linha-1)) ++ (retorno !! 0)):[])
														| tamanho > 0 && linha /= tamanho = insereElementoMatriz (tamanho-1) linha coluna elem matriz ((matriz !! (linha-1)):retorno)
														| tamanho > 0 && linha == tamanho && retorno !! 0 == [] = insereElementoMatriz (tamanho-1) linha coluna elem matriz (((insereElementoVetor (coluna-1) elem (matriz !! (linha-1))) ++ (retorno !! 0)):[])
										   				| tamanho > 0 && linha == tamanho = insereElementoMatriz (tamanho-1) linha coluna elem matriz ((insereElementoVetor (coluna-1) elem (matriz !! (linha-1))):retorno)
										   				| tamanho <= 0 = retorno

insereElementoVetor :: Int -> String -> [String] -> [String]
insereElementoVetor pos elem array = ((take pos array) ++ [elem] ++ (drop (pos+1) array))

matriz :: Int -> String
matriz 1 = matrizRepresentacao (replicate 4 (replicate 4 "X")) 1
matriz 2 = matrizRepresentacao (replicate 6 (replicate 6 "X")) 1
matriz 3 = matrizRepresentacao (replicate 8 (replicate 8 "X")) 1

matrizPrint :: Int -> [String] -> IO()
matrizPrint x carac = do
				putStrLn (primeiraLinha x)
				putStrLn (matriz x)

primeiraLinha :: Int -> String
primeiraLinha x | x == 1 = "   1 2 3 4"
				| x == 2 = "   1 2 3 4 5 6"
				| x == 3 = "   1 2 3 4 5 6 7 8"
				| otherwise = ""

matrizAleatoria :: [String] -> Int -> [[String]]
matrizAleatoria carc x | x == x = [["x"]]
					   | otherwise = [[""]]

matrizRepresentacao :: [[String]] -> Int -> String
matrizRepresentacao [] z = ""
matrizRepresentacao (x:xs) z = " " ++ show z ++ vetorRepresentacao x ++ "\n" ++ matrizRepresentacao xs (z+1) 

vetorRepresentacao :: [String] -> String
vetorRepresentacao [] = ""
vetorRepresentacao (x:xs) = " " ++ x ++ vetorRepresentacao xs