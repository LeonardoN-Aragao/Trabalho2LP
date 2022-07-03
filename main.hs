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

compareValue (_,[],p) = return 0
compareValue (key,x:xs, parcial) = do 
    if key == x then do
        return 1
    else 
        compareValue (key,xs,parcial)

-- if está na posicao aumenta 1 no completo
-- else procura se tem em outra posicao e aumenta o parcial caso tiver

--auxCompareAnswer :: ([Char], [Char], Int, Int) -> (Int, Int)
auxCompareAnswer ([],[],completo,parcial) = return (completo, parcial)
auxCompareAnswer (x:xs, y:ys, completo, parcial) = do 
   if x == y then
       auxCompareAnswer (xs,ys,completo + 1,parcial)
   else do
       aux <-compareValue (x,ys,0)
       if aux > 1 then do
           let p = (parcial + aux) - 1
           auxCompareAnswer (xs,ys,completo,p)
       else
           auxCompareAnswer (xs,ys,completo,parcial)    

--compareAnswer :: ([Char], [Char]) -> (Char, Char)
compareAnswer (x:xs, y:ys) = do
    a <- compareLists (x:xs,y:ys)
    if  a == True then
        return (4, 0)
    else do
        (c, parcial) <- auxCompareAnswer(x:xs, y:ys,0,0)
        return (c, parcial)


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
    (a,b) <- compareAnswer(['2','3','1','4'],['2','3','4','1'])
    print a 
    print b


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
