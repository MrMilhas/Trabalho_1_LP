--import System.Random (uniformR)

{--
    parameter 1: sequência do jogador 1;
    parameter 2: tentativa do jogador 2;
    definition: Função que recebe a tentativa do jogador 2 e a
                sequência do jogador 1 e retorna a relação de
                acertos e erros do jogador 2;
--}
verify_sequence xs ys = do
    val <- getLine
    return val


{--
    parameter 1: número de tentativas;
    parameter 2: sequência do jogador 1;
    definition: Função que recebe a tentativa do jogador 2 e verifica
                a corretude da sequência;
--}
player_2 0 xs = putStrLn ("Número de tentativas esgotadas! Senha correta: "++ (show xs))
player_2 n xs = do
    putStrLn "Digite uma tentativa de 4 dígitos: "
    line <- getLine
    if line <= xs then (putStrLn "Parabéns! Você acertou!!") 
    else verify_sequence (xs) (line) player_2 (n-1) (xs) 


{--
    definition: Função principal para inicializar o jogo. O jogador 1
                digita sequência e o número máximo de tentativas e 
                chama a função para as operações do jogador 2.
--}
game = do
    putStrLn "---------------------------------------------"
    putStrLn "----------------- Jogador 1 -----------------"
    putStrLn "- Digite uma sequência de 4 dígitos: "
    line <- getLine
    putStrLn "- Digite o número de tentativas: "
    n <- getLine
    let num = read n :: Int
    putStrLn ""
    putStrLn "---------------------------------------------"
    putStrLn "----------------- Jogador 2 -----------------"
    player_2 num line
