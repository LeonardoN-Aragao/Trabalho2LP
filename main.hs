-- Gabriel Santos Fortunato: 201665074AC
-- Leonardo Nunes Aragão: 201665565C

import System.Random (randomRIO, random)
import Data.Char

main :: IO ()
main = do 
    answerList <- generateList 4
    getAttempt(answerList, 1)

-------------------------------------Joguinho-------------------------------------------------------------------

getAttempt :: ([Char], Int) -> IO () 
getAttempt (answer, attemptCounter) = do
    putStr "? "
    input <- getLine
    let attempt = removeOccurrencesInList(' ', input)
    if (not(isValid attempt)) then do
        putStrLn "Quantidade ou número inválido, por favor digite novamente 4 números de 1-6"
        getAttempt (answer, attemptCounter)
    else do
        (correctAmount,partialAmount) <- checkAttempt(attempt,answer)
        putStr (show correctAmount)
        putStr (" Completo, ")
        putStr (show partialAmount)
        putStrLn (" Parcial")
        if correctAmount == 4 then do
            putStr ("Parabéns, você acertou após ")
            putStr (show attemptCounter)
            putStrLn (" tentativas!")
        else do
            getAttempt (answer,(attemptCounter+1))


------------------------------------------------------------------------------------------------------------------

-------------------------------------------Validação das tentativas----------------------------------------------

--Retorna o número de corretos dada a tentativa, além de retornar 2 sublistas remanescentes que correspondem 
--à lista da tentativa e da resposta, após remover os corretos Ex: "1234" e "1341" retorna "234","341" e 1
getCorrectAmount(headAttempt:tailAttempt, headAnswer:tailAnswer) = do
    let tupleList = zip (headAttempt:tailAttempt) (headAnswer:tailAnswer)
    let differentOnly = filter (\(x,y) -> x /= y) tupleList
    let listTuple = unzip differentOnly
    let remainingListFromAttempt = fst(listTuple)
    let remainingListFromAnswer = snd(listTuple)
    let incorrectAmount = length(remainingListFromAttempt)
    return (remainingListFromAttempt, remainingListFromAnswer, 4 - incorrectAmount)

--Retorna o número de parciais, dada a lista remanescente retornada pelo método acima
getPartialAmount([], _, currentPartialAmount) = return currentPartialAmount
getPartialAmount(headCandidates:tailCandidates, headRemaining:tailRemaining, currentPartialAmount) = do
    (found, index) <- findInList(headCandidates, headRemaining:tailRemaining, 0)
    if found == 0 then
        getPartialAmount(tailCandidates, headRemaining:tailRemaining, currentPartialAmount)
    else do
        let remainingList = deleteElementByIndex (index) (headRemaining:tailRemaining)
        getPartialAmount(tailCandidates, remainingList, currentPartialAmount+1)

--Dada uma lista com a tentativa e uma lista correspondente à resposta correta, retorna a quantidade de corretos e quantidade de acertos parciais 
checkAttempt(headAttempt:tailAttempt, headAnswer:tailAnswer) = do
    (remainingListFromAttempt, remainingListFromAnswer, correctAmount) <- getCorrectAmount(headAttempt:tailAttempt, headAnswer:tailAnswer)
    if (length(remainingListFromAttempt) == 0) then 
        return (correctAmount, 0)
    else do
        partialAmount <- getPartialAmount(remainingListFromAttempt, remainingListFromAnswer, 0)
        return (correctAmount, partialAmount)

----------------------------------------------------------------------------------------------------------------

--------------------------------------------Funções Auxiliares-----------------------------------------------------


--Retorna uma lista de 4 dígitos sendo cada um de valor entre 1 e 6
generateList size = do
    item <-randomRIO('1','6' :: Char)
    list <- auxGenerateList (size-1,item:[])
    return list
auxGenerateList (0, list) = return list
auxGenerateList (size, list) = do 
    item <-randomRIO('1','6' :: Char)
    auxGenerateList (size-1,item:list)

--Tentamos fazer funcionar sem precisar passar o index e sem retornar "sucesso", como na função abaixo(findInList)
indexOf :: (Char, [Char]) -> Int
indexOf (key,head:tail) = do
    auxIndexOf(key, head:tail, 0)
auxIndexOf :: (Char, [Char], Int) -> Int
auxIndexOf (_,[], index) = -1
auxIndexOf (key,head:tail, index) = do 
    if key == head then do
        index
    else 
        auxIndexOf (key,tail,index+1)

--Busca elemento numa lista e retorna 2 inteiros, sendo o primeiro 1 caso encontrado 
--e 0 caso não encontrado, o segundo inteiro representa o índice, caso encontrado, e -1, caso não
findInList (_,[],index) = return (0,-1)
findInList (key,head:tail, index) = do 
    if key == head then do
        return (1,index)
    else 
        findInList (key,tail,index+1)

deleteElementByIndex _ [] = []
deleteElementByIndex x zs | x >= 0 = take (x) zs ++ drop x zs
    |   otherwise = zs

removeOccurrencesInList (key, list) = filter (\e -> e/=key) list

--Valida entrada se está de acordo com as regras do jogo (4 números entre 1 e 6)
isValid (list) = do
    inValidRange(list) && inValidSize(list)
inValidSize(list) = do
    length list == 4
inValidRange(list) = do
    let aux = filter (<='6') (list)
    let aux2 = filter (>'0') aux
    length(aux2) == 4

    ----------------------------------------------------------------------------------------------------------------