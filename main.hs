import System.Random
import Control.Monad.IO.Class

player_1 :: Control.Monad.IO.Class.MonadIO a => a [Int]
player_1 = do
    seed <- newStdGen
    return (take 4 (randomRs (0 :: Int ,9) seed))

setAction::Control.Monad.IO.Class.MonadIO a => a Int
setAction = do
    seed <- newStdGen
    return (fst (randomR (1,3) seed))

get_right_pos xs [] ans = ans 
get_right_pos (x:xs) (y:ys) ans = if x == y then get_right_pos xs ys (fst ans,('O':(snd ans))) else get_right_pos xs ys ((x:(fst ans)),snd ans)

verify_existence [] ys ans = ans
verify_existence (x:xs) ys ans = if (elem x ys) then verify_existence xs ys (fst ans,('-':(snd ans))) else verify_existence xs ys ((x:(fst ans)),snd ans)

mark_error [] ans = ans
mark_error (x:xs) ans = mark_error xs ('X':ans)

convert_to_string [] ys = ys
convert_to_string (x:xs) ys = convert_to_string xs (ys ++ (show x))

is_everything_all_right [] [] = True
is_everything_all_right (x:xs) (y:ys) = if x == y then is_everything_all_right xs ys else False

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

execute_rounds n seq = do
    if n > 0
        then
            do
                putStrLn "Digite sua senha: "
                player_2_password <- getLine
                if is_everything_all_right player_2_password seq
                    then putStrLn "Você acertou a senha!"
                else
                    do
                        let result1 = get_right_pos player_2_password seq ([],[])
                        let rightpos = snd result1
                        let result2 = verify_existence (fst result1) seq ([],[])
                        let exs = snd result2
                        let err = mark_error (fst result2) []
                        tip <- createTipArr rightpos exs err []
                        putStrLn ("Senha incorreta, dica: " ++ tip)
                        execute_rounds (n-1) seq
    else
        putStrLn ("Tentativas esgotadas, a senha era " ++ seq)


mastermind = do
    putStrLn "Jogador 1 criou sua senha!"
    player_1_password <- player_1
    let strSeq = convert_to_string (player_1_password) []
    execute_rounds 8 strSeq   