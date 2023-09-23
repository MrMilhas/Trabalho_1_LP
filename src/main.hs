import System.Random (getStdRandom, randomR, randomRIO, random, randomRs, newStdGen)    

get_random_nums n0 = [a,b,c,d]
    where
        (a,n1) = randomR (0,9) n0
        (b,n2) = randomR (0,9) n1
        (c,n3) = randomR (0,9) n2
        (d,n4) = randomR (0,9) n3

random_num :: Int
random_num = randomRIO (0,9)

get_random_sequence = [random_num, random_num, random_num, random_num]

{--
    parameter 1: sequência do jogador 1;
    parameter 2: tentativa do jogador 2;
    definition: Função que recebe a tentativa do jogador 2 e a
                sequência do jogador 1 e retorna a relação de
                acertos e erros do jogador 2;
--}
verify_sequence xs ys = do
    putStrLn ("Teste")


{--
    parameter 1: número de tentativas;
    parameter 2: sequência do jogador 1;
    definition: Função que recebe a tentativa do jogador 2 e verifica
                a corretude da sequência;
--}
player_2 :: (Eq t, Num t) => t -> String -> IO ()
player_2 0 xs = putStrLn ("Número de tentativas esgotadas! Senha correta: "++ (show xs))
player_2 n xs = do
    putStrLn "Digite uma tentativa de 4 dígitos: "
    line <- getLine
    if line <= xs then (putStrLn "Parabéns! Você acertou!!") 
    else do 
        verify_sequence (xs) (line) 
        player_2 (n-1) (xs) 


{--
    definition: Função principal para inicializar o jogo. O jogador 1
                digita sequência e o número máximo de tentativas e 
                chama a função para as operações do jogador 2.
--}
game = do
    putStrLn "---------------------------------------------"
    putStrLn "----------------- Jogador 1 -----------------"
    let line = get_random_sequence
    putStrLn ""
    putStrLn "---------------------------------------------"
    putStrLn "----------------- Jogador 2 -----------------"
    player_2 4 (show line)
