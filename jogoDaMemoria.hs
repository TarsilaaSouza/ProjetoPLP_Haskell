import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.Time

main :: IO()
main = do
	menu

-- Imprime o menu principal recebe a opcao selecionada
menu :: IO()
menu = do 
	criaArquivo
	system "clear"
	putStrLn(" Menu:\n\n" ++
            "1. Iniciar Jogo\n" ++
            "2. Ranking\n" ++
            "3. Sair\n\n")
	dados <- getLine
	let opcao = read(dados)
	if  opcao == 1 then do
		putStrLn("Digite seu nome: ")
		nome <- getLine
		nivel nome
	else if opcao == 2 then
		printranking
	else if opcao == 3 then do
		system "clear"
		putStrLn "\n\n\nBye\n\n\n"
	else
		menu

-- Verifica se o elemento encontra-se no vetor
containsVetor :: [String] -> String -> Bool
containsVetor [] _ = False
containsVetor (x:xs) y | x == y = True
					   | xs == [] = False
					   | x /= y = containsVetor xs y

-- Cadastra recorde no Ranking
ranking :: String -> IO()
ranking recorde = do
	arq <- openFile "ranking.txt" WriteMode
	hPutStrLn arq (recorde)
	hClose arq

-- printa Ranking
printranking :: IO()
printranking  = do
	arq <- openFile "ranking.txt" ReadMode
	dados <- hGetLine arq
	hClose arq
	putStrLn(dados)

-- Exibe menu de niveis e recebe o nivel selecionado
nivel :: String -> IO()
nivel nome = do
	system "clear"
	putStrLn(" Escolha um dos niveis:\n\n" ++
			"1. Iniciante\n" ++
			"2. Intermediario\n" ++
			"3. Avancado\n\n")
	dados <- getLine
	let nivels = read(dados)
	if nivels == 1 then
		iniciaJogo nivels 4 nome
	else if nivels == 2 then
		iniciaJogo nivels 6 nome
	else if nivels == 3 then
		iniciaJogo nivels 8 nome
	else
		nivel nome

-- Cria arquivo se nao existir
criaArquivo :: IO()
criaArquivo = do 
				{catch (ler_arquivo) tratar_erro}
				where
					ler_arquivo = do {

						arq <- openFile "ranking.txt" ReadMode;
						dados <- hGetLine arq;
						hClose arq;
					}

					tratar_erro erro = if isDoesNotExistError erro then do{
						arq <- openFile "ranking.txt" WriteMode;
						hPutStrLn arq "[]";
						hClose arq;
						return()
					}
					else
						ioError erro

-- Inicializa o jogo
iniciaJogo :: Int -> Int -> String -> IO()
iniciaJogo nivel tamanho nome= do
	let matrizDoUsuario = (matrizUsuario tamanho)
	matrizSistema <- geraMatriz tamanho
	time <- getCurrentTime
	jogo tamanho nivel 0 matrizDoUsuario matrizSistema time

	tempoDaPartida <- getCurrentTime
	let tempoPercorrido = (diffUTCTime tempoDaPartida time)
	let recorde = nome ++": "++show tempoPercorrido
	ranking (recorde)
	putStrLn "\n"

-- Controla o jogo
jogo :: Int -> Int -> Int -> [[String]] -> [[String]] -> UTCTime -> IO()
jogo tamanho nivel paresEcontrados matrizUsuario matrizSistema tempo = do
	let x = tamanho * tamanho
	let xD =  intToDouble x
	let g = xD / 2
	let pares = round g
	let tempoParada = fromIntegral 5 :: NominalDiffTime
	tempoDoJogo <- getCurrentTime
	let tempoPercorrido = (diffUTCTime tempoDoJogo tempo)
	if (paresEcontrados /= pares && tempoParada > tempoPercorrido) then do
		
		matrizPrint nivel matrizUsuario
		putStrLn "\nEscolha a linha e a coluna, respectivamente, do primeiro elemento:"
		posicao1 <- getLine
		putStrLn "\nEscolha a linha e a coluna, respectivamente, do segundo elemento:"
		posicao2 <- getLine
		
		if (length(posicao1) == 3) && (length(posicao2) == 3) then do
			let posl1 = read([posicao1!!0])
			let posc1 = read([posicao1!!2])
			let posl2 = read([posicao2!!0])
			let posc2 = read([posicao2!!2])
			if (verificaPares posl1 posc1 posl2 posc2 matrizSistema) then do
				let posicao = (posl1, posc1)
				let elem = matrizSistema !! (posl1 -1) !! (posc1 -1)
				cond <- posicaoValida posicao matrizUsuario
				if cond then do
					matrizM <- modificaMatriz tamanho elem posicao matrizUsuario
					let posicao2 = (posl2, posc2)
					let elem2 = matrizSistema !! (posl2 -1) !! (posc2 -1)
					matrizM2 <- modificaMatriz tamanho elem2 posicao2 matrizM
					jogo tamanho nivel (paresEcontrados+1) matrizM2 matrizSistema tempo
				else do
					putStrLn("Posicao Ja encontrada")
					jogo tamanho nivel paresEcontrados matrizUsuario matrizSistema tempo
			else do
				let posicao = (posl1, posc1)
				let elem = matrizSistema !! (posl1 -1) !! (posc1 -1)
				matrizM <- modificaMatriz tamanho elem posicao matrizUsuario
				let posicao2 = (posl2, posc2)
				let elem2 = matrizSistema !! (posl2 -1) !! (posc2 -1)
				matrizM2 <- modificaMatriz tamanho elem2 posicao2 matrizM
				matrizPrint nivel matrizM2
				jogo tamanho nivel paresEcontrados matrizUsuario matrizSistema tempo
		else do
			system "clear"
			putStrLn "Preencha os dados com posicoes validas"
			jogo tamanho nivel paresEcontrados matrizUsuario matrizSistema tempo
	else
		putStrLn "Fim de jogo"

-- Retorna uma lista de caracteres
caracteres :: [String]
caracteres = [ "◒", "◕", "◔", "◐", "☎", "☂", "☀", "☢", "☣", "☹", "☯", "☩", "☠", "☸", "♛", "♚", "♜", "♝", "♞", "♡", "✿", "✻", "⊳", "⊖", "➸", "➱", "❤" , "✸", "✖", "✔", "♫", "♬", "∞", "✂", "✈"]

 -- Converte Integer para Int
integerToInt :: Integer -> Int
integerToInt x = fromIntegral x :: Int

 -- Converte Integer/Int para Double
intToDouble :: Int -> Double
intToDouble x = fromIntegral x :: Double

-- Retorna uma lista de caracteres aleatorios a partir quantidade de pares do nivel
caracteresRandom :: [String] -> [String] -> Int -> IO [String]
caracteresRandom carac retorno i | i > 0 = do
							rand <- randomRange(0, 34)
							let elem = carac !! rand
							if containsVetor retorno elem then
								caracteresRandom carac retorno i 
							else
								caracteresRandom carac (elem:retorno) (i-1)
						| i <= 0 = do
							return retorno

-- Retorna uma matriz randomica de tamanho x
geraMatriz :: Int -> IO [[String]]
geraMatriz tamanho = do 
		let z = replicate tamanho (replicate tamanho "")
		let x = [(x,y)|x <- [1..tamanho], y <- [1..tamanho]]
		let j = tamanho * tamanho
		let u = (intToDouble j) / 2
		let t = round(u)
		ca <- caracteresRandom caracteres [] t
		result <- preencheMatriz tamanho ca x z
		return result

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

-- Verifica se uma posicao esta ja foi encontrada
posicaoValida :: (Int,Int) -> [[String]] -> IO Bool
posicaoValida posicao  matriz = do
	let linha = (fst posicao) -1
	let coluna = (snd posicao)-1
	if (matriz !! linha !! coluna == "X") then 
		return True
	else return False

-- Insere um elemnento na lista
removeElementoVetor :: Int -> [(Int,Int)] -> [(Int,Int)]
removeElementoVetor pos array = ((take pos array) ++ (drop (pos+1) array))

--Preenche uma Matriz fazia randomicamente
preencheMatriz :: Int -> [String] -> [(Int,Int)] -> [[String]] -> IO [[String]]
preencheMatriz tamanho (x:xs) posicoes matriz = do
	posicao <- randomRange(0, (length posicoes -1))
	let linha = (fst (posicoes !! posicao))
	let coluna = (snd (posicoes !! posicao))
	let posicoes2 = removeElementoVetor posicao posicoes
	let novaMatriz = insereElementoMatriz tamanho linha coluna x matriz [[]]
	posicao2 <- randomRange(0, (length posicoes2 -1))
	let l = (fst (posicoes2 !! posicao2))
	let c = (snd (posicoes2 !! posicao2))
	let posicoes3 = removeElementoVetor posicao2 posicoes2
	let novaMatriz2 = insereElementoMatriz tamanho l c x novaMatriz [[]]
	if( xs /= []) then do
		r <- preencheMatriz tamanho xs posicoes3 novaMatriz2
		return r
	else
		return novaMatriz2


-- Cria a matriz do usuario de acordo com o tamanho
matrizUsuario :: Int -> [[String]]
matrizUsuario x = replicate x (replicate x "X")

modificaMatriz :: Int -> String -> (Int,Int) -> [[String]] -> IO [[String]]
modificaMatriz tamanho elem posicao matriz = do
	let linha1 = fst posicao
	let coluna1 = snd posicao
	let matrizMod = insereElementoMatriz tamanho linha1 coluna1 elem matriz [[]]
	return matrizMod

-- Imprime a representacao de uma matriz
matrizPrint :: Int -> [[String]] -> IO()
matrizPrint nivel matriz = do
				putStrLn (primeiraLinha nivel)
				putStrLn (matrizRepresentacao matriz 1)

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

--Gera um numero entre 0 e 1 pseudo aleatorio
myRandom :: IO Double
myRandom = do 
	x <- getCurrentTime
	let z = show x
	let num1 = [z!!26, z!!27]
	let num1Int = read num1
	let rand = num1Int / 99
	return rand

--Gera um numero pseudo aleatorio entre a e b
randomRange :: (Int,Int) -> IO Int
randomRange (a, b) = do
	rand <- myRandom
	let mat = b - a
	let matt = intToDouble mat
	let ra = matt * rand
	let a2 = intToDouble a
	let resul = ra + a2
	let result = round resul :: Int
	return result

--Verifica se duas posicoes possuem elementos iguais
verificaPares ::  Int -> Int -> Int -> Int -> [[String]] -> Bool
verificaPares linha1 coluna1 linha2 coluna2 matriz 
	| linha1 == linha2 && coluna1 == coluna2 = False
	| matriz !! (linha1 -1) !! (coluna1 -1) == matriz !! (linha2 -1) !! (coluna2 -1) = True
	| otherwise = False