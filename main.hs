import System.Random (randomRIO, random)

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
auxAddList (0, c) = return [c]
auxAddList (n, p) = do 
    b <-randomRIO('1','4' :: Char)
    auxAddList (n-1,b:p)

addList n = do
    b <-randomRIO('1','4' :: Char)
    c <- auxAddList (n-1,b:[])
    return c

geraNumeros 0 = return ()
geraNumeros n = do 
    b <-randomRIO('1','4' :: Char)
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

compareValue (_,[],p) = return p
compareValue (key,x:xs, parcial) = do 
    if key == x then
        compareValue (key,xs,parcial+1)
    else
        print ""

-- if está na posicao aumenta 1 no completo
-- else procura se tem em outra posicao e aumenta o parcial caso tiver
--compareAnswer :: ([Char], [Char], Int, Int) -> Info
compareAnswer ([],[],completo,parcial) = return (completo, parcial)
compareAnswer (x:xs,y:ys, completo, parcial) = do 
    if x == y then
        compareAnswer (xs,ys,completo+1,parcial)
    else do
        aux <-compareValue (x,y:ys,0)
        if aux > 1 then
            compareAnswer (xs,ys,completo,parcial+aux-1)
        else
            compareAnswer (xs,ys,completo,parcial)    
-----------------------------------------------------
main :: IO ()
main = do 
    mostraLista [0,2..10]
    a <- compareLists ([1,2,4],[1,2,4])
    b <- compareLists ([1,2,4],[1,2,5])
    print a
    print b
    c <- addList 4
    print c








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
