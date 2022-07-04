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

-- auxAddList (0, c) = return c
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

-- if está na posicao aumenta 1 no completo
-- else procura se tem em outra posicao e aumenta o parcial caso tiver

--auxCompareAnswer :: ([Char], [Char], Int, Int) -> (Int, Int)
-- auxCompareAnswer ([],[],_,completo,parcial) = return (completo, parcial)
-- auxCompareAnswer (x:xs, y:ys, z:zs, completo, parcial) = do 
--    if x == y then
--        auxCompareAnswer (xs,ys,zs,completo + 1,parcial)
--    else do
--        (aux, cont) <-compareValue (x,z:zs,0)
--        if aux == 1 then do
--             let p = parcial + aux
--             auxCompareAnswer (xs,ys,z:zs,completo,p)
--        else
--             auxCompareAnswer (xs,ys,z:zs,completo,parcial)    

--compareAnswer :: ([Char], [Char]) -> (Char, Char)
-- compareAnswer (x:xs, y:ys) = do
--     a <- compareLists (x:xs,y:ys)
--     if  a == True then
--         return (4, 0)
--     else do
--         (c, parcial) <- auxCompareAnswer(x:xs, y:ys, y:ys,0,0)
--         return (c, parcial)


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
    --getInput
    
    --map digitToInt ['2','2','4']

-----------------------------------------------------
main :: IO ()
main = do 
    -- mostraLista [0,2..10]
    -- a <- compareLists ([1,2,4],[1,2,4])
    -- b <- compareLists ([1,2,4],[1,2,5])
    -- print a
    -- print b
    c <- generateList 4
    getInput(c, 1)
    -- print c
    -- (a,b) <- compareAnswer(['4','4','6','6'],['1','4','6','6'])
    -- print ("Completos:")
    -- print a 
    -- print ("Parciais:")
    -- print b


--    map (print a) [1:4]
 
-- filter (== 3) [62,3,25,7,1,9] 

-- map (+1) [23,42,45,98]

-- let square x = x * x in map square [1..10]


-- let take5s = filter (==5) in map take5s [[1,5],[5],[1,1]]

-- map (toUpper) "Chris"

-- let (a,b) = (10,12) in a * 2

-- let (a:_:_:_) = "xyzert" in a



-- let (_:a:_) = "xyz" in a


-- let (_,(a:_)) = (10,"abc") in a


-- let [a,b,c] = "cat" in (a,b,c)
-- ('c','a','t'):: (Char, Char, Char)
