{-
    Rodrigo Oliveira Ferrari - 201865567C
    Daniel Muller Rezende - 202065020C
-}

import System.Random
import Control.Monad.IO.Class

-- Gera uma lista de 4 números aleatórios entre 0 e 9.
player_1 :: Control.Monad.IO.Class.MonadIO a => a [Int]
player_1 = do
    seed <- newStdGen
    return (take 4 (randomRs (0 :: Int ,9) seed))

-- Gera um número aleatório entre 1 e 3, esta função é usada para randomizar a lista que dá a dica para o jogador 2.
setAction::Control.Monad.IO.Class.MonadIO a => a Int
setAction = do
    seed <- newStdGen
    return (fst (randomR (1,3) seed))

-- Retorna uma tupla que o segundo membro é uma lista contendo os símbolos 'O' e primeiro membro é a senha do jogador 2 menos os digitos que ele acertou o número e posição.
get_right_pos xs [] ans = ans 
get_right_pos (x:xs) (y:ys) ans = if x == y then get_right_pos xs ys (fst ans,('O':(snd ans))) else get_right_pos xs ys ((x:(fst ans)),snd ans)

-- Retorna uma tupla que o segundo membro é uma lista contendo os símbolos '-' e primeiro membro é a senha do jogador 2 menos os digitos que ele acertou o número, mas não a posição.
verify_existence [] ys ans = ans
verify_existence (x:xs) ys ans = if (elem x ys) then verify_existence xs ys (fst ans,('-':(snd ans))) else verify_existence xs ys ((x:(fst ans)),snd ans)

-- Retorna uma lista contendo símbolos 'X', a quantidade equivale ao tamanho da lista que é passada no parâmetro.
mark_error [] ans = ans
mark_error (x:xs) ans = mark_error xs ('X':ans)

-- Converte uma lista de valores inteiros em uma String, ex: [1,2,3,4] vira "1234"
convert_to_string [] ys = ys
convert_to_string (x:xs) ys = convert_to_string xs (ys ++ (show x))

-- Retorna True se todos os caracteres de uma lista são iguais a da segunda lista, retorna False, caso contrário.
is_everything_all_right [] [] = True
is_everything_all_right (x:xs) (y:ys) = if x == y then is_everything_all_right xs ys else False

{-
    Retorna a lista completa que representa a dica dada para o jogador 2 baseado na senha que ele chutou, ela pede como parâmetro as 3 lista criadas nas funções anteriores. Após isso
    um número aleatório é gerado e um elemento de uma lista associada ao número é colocada por vez. A função continua até todas as listas parciais serem inseridas na lista principal.
-}
createTipArr [] [] [] ans = do 
    return ans
createTipArr xs ys zs ans = do
    action <- setAction
    if action == 1
        then
            if (length xs) == 0
                then createTipArr xs ys zs ans
            else
                createTipArr (tail xs) ys zs ((head xs):ans)
    else if action == 2
        then
            if (length ys) == 0
                then createTipArr xs ys zs ans
            else
                createTipArr xs (tail ys) zs ((head ys):ans)
    else
        if (length zs) == 0
                then createTipArr xs ys zs ans
            else
                createTipArr xs ys (tail zs) ((head zs):ans)

-- Função que executa as rodadas do jogo mastermind, ela termina somente se o jogador acertar a senha ou esgotar suas tentativas.
execute_rounds n seq = do
    if n > 0 -- Verifica se o jogador não gastou todas as suas tentativas.
        then
            do --Jogador 2 digita sua senha.
                putStrLn "Digite sua senha: "
                player_2_password <- getLine
                if (length player_2_password) == 4 -- Verifica se a senha tem 4 dígitos.
                    then
                        if is_everything_all_right player_2_password seq -- Verifica se o jogador acertou a senha.
                            then putStrLn "Você acertou a senha!"
                        else -- Caso contrário, começa a criação da lista de dica.
                            do
                                let result1 = get_right_pos player_2_password seq ([],[]) -- Recebe a tupla com a senha parcial e os símbolos de acerto.
                                let rightpos = snd result1 -- Recebe a lista de símbolos de acerto.
                                let result2 = verify_existence (fst result1) seq ([],[]) -- Recebe a Recebe a tupla com a senha parcial símbolos de acerto parcial. A lista passada por parâmetro é a senha parcial da função anterior.
                                let exs = snd result2 -- Recebe a lista de símbolos de acerto parcial.
                                let err = mark_error (fst result2) [] -- Recebe a lista de erros usando a senha parcial que sobrou após ser filtrada pelas duas funções anteriores.
                                tip <- createTipArr rightpos exs err [] -- Cria a lista completa de dica.
                                putStrLn ("Senha incorreta, dica: " ++ tip)
                                execute_rounds (n-1) seq -- Executa mais uma iteração agora reduzindo as tentativas restantes em 1.
                else -- Caso contrário, invalida a tentativa e o jogador 2 pode tentar digitar novamente.
                    do
                        putStrLn "Senha não possui 4 dígitos!"
                        execute_rounds n seq
    else -- Caso contrário, revela a senha para o jogador 2 e encerra as rodadas.
        putStrLn ("Tentativas esgotadas, a senha era " ++ seq)

{-
    Função usada para chamar o jogo Mastermind, cria a senha através de uma lista de inteiros e converte essa lista para string (não conseguimos converter a lista do jogador 2 
    para inteiros então fizemos o processo inverso) e chama a função de execução das rodadas.
-}
mastermind = do
    putStrLn "Jogador 1 criou sua senha!"
    player_1_password <- player_1
    let strSeq = convert_to_string (player_1_password) []
    execute_rounds 8 strSeq   