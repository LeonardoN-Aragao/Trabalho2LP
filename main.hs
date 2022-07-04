import System.Random (randomRIO, random)
import Data.Char
oi = print("ola")



--values :: w -> [Char] 
values = do
    b <- randomRIO('1','4' :: Char)
    print b

mostraLista :: Show a => [a] -> IO ()
mostraLista [] = return ()
mostraLista (x:xs) = do 
    print x
    mostraLista xs


auxGenerateList (0, c) = return c
auxGenerateList (n, p) = do 
    b <-randomRIO('1','6' :: Char)
    auxGenerateList (n-1,b:p)

generateList n = do
    b <-randomRIO('1','6' :: Char)
    c <- auxGenerateList (n-1,b:[])
    return c

geraNumeros 0 = return ()
geraNumeros n = do 
    b <-randomRIO('1','6' :: Char)
    print b
    geraNumeros (n-1)

compareLists ([],[]) = return True
compareLists (x:xs,y:ys) = do 
    if x == y then
        compareLists (xs,ys)
    else
        return False

-----------------------------------------------------
-- data Info = Info Int Int

compareValue (_,[],index) = return (0,-1)
compareValue (key,x:xs, index) = do 
    if key == x then do
        return (1,index)
    else 
        compareValue (key,xs,index+1)




contaCorretos(x:xs, y:ys) = do
    let listaDeTuplas = zip (x:xs) (y:ys)
    let tuplasDiferentes = filter (\(x,y) -> x /= y) listaDeTuplas
    let tuplaDeListas = unzip tuplasDiferentes
    let lista1 = fst(tuplaDeListas)
    let lista2 = snd(tuplaDeListas)
    let qtdIncorretos = length(lista1)
    return (lista1, lista2, 4 - qtdIncorretos)

--compareAnswer :: ([Char], [Char]) -> (Int, Int)    
compareAnswer(x:xs, y:ys) = do
    (lista1, lista2, completos) <- contaCorretos(x:xs, y:ys)
    if (length(lista1) == 0) then 
        return (completos, 0)
    else do
        parciais <- contaParciais(lista1, lista2, 0)
        return (completos, parciais)

contaParciais([], _, cont) = return cont
contaParciais(x:xs, y:ys, cont) = do
    (existe, index) <- compareValue(x, y:ys, 0)
    if existe == 0 then
        contaParciais(xs, y:ys, cont)
    else do
        let novaLista = deleteElementByIndex (index+1) (y:ys)
        --return novaLista
        contaParciais(xs, novaLista, cont+1)

deleteElementByIndex _ [] = []
deleteElementByIndex x zs | x > 0 = take (x-1) zs ++ drop x zs
    |   otherwise = zs

-----------------------------------------------------

remove (key, list) =  filter (\e -> e/=key) list

--isValid :: [Char] -> Bool
isValid (x:xs) = do
    let aux2 = filter (<='6') (x:xs)
    let aux = filter (>'0') aux2
    length(aux) == 4

--getInput :: Show a => [a] -> IO ()
getInput :: ([Char], Int) -> IO () 
getInput (list, cont) = do
    putStr "? "
    input <- getLine
    let aux = remove (' ', input)
    let tam = length aux
    if ((tam /= 4) || not(isValid aux)) then do
        putStrLn "Quantidade ou número inválido, por favor digite novamente 4 números de 1-6"
        getInput (list, cont)
    else do
        (completo,parcial) <- compareAnswer(aux,list)
        putStr (show completo)
        putStr (" Completo, ")
        putStr (show parcial)
        putStrLn (" Parcial")
        if completo == 4 then do
            putStr ("Parabéns, você acertou após ")
            putStr (show cont)
            putStrLn (" tentativas!")
        else do
            getInput (list,(cont+1))


-----------------------------------------------------
main :: IO ()
main = do 

    c <- generateList 4
    getInput(c, 1)
