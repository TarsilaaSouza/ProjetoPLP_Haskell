
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
	let caracteres = [ "◒", "◕", "◔", "◐", "☎", "☂", "☀", "☢", "☣", "☹", "☯", "☩", "☠", "☸", "♛", "♚", "♜", "♝", "♞", "♡", "✿", "✻", "⊳", "⊖", "➸", "➱", "❤" , "✸", "✖", "✔", "♫", "♬", "∞", "✂", "✈"]
	
	if nivels == 1 then
		matrizPrint nivels caracteres
	else if nivels == 2 then
		vetorPrint caracteres nivels
	else if nivels == 3 then
		matrizPrint nivels caracteres
	else
		nivel


vetorPrint :: [String] -> Int -> IO()
vetorPrint carac x = do 
				vetor <- vetorRandom carac [] x
				putStrLn(matrizY vetor)

integerToInt :: Integer -> Int
integerToInt x = fromIntegral x :: Int

vetorRandom :: [String] -> [String] -> Int -> IO [String]
vetorRandom carc mart i | i > 0 = do
							x <- randomRIO(0, 34)
							let rand = integerToInt x
							let elem = carc !! rand
							vetorRandom carc (elem:mart) (i-1)
						| i <= 0 = do
							return mart 


matriz :: Int -> String
matriz 1 = matrizP (replicate 4 (replicate 4 "X")) 1
matriz 2 = matrizP (replicate 6 (replicate 6 "X")) 1
matriz 3 = matrizP (replicate 8 (replicate 8 "X")) 1

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

matrizP :: [[String]] -> Int -> String
matrizP [] z = ""
matrizP (x:xs) z = " " ++ show z ++ matrizY x ++ "\n" ++ matrizP xs (z+1) 

matrizY :: [String] -> String
matrizY [] = ""
matrizY (x:xs) = " " ++ x ++ matrizY xs

