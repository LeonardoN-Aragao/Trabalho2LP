-- Gabriel Santos Fortunato: 201665074AC
-- Leonardo Nunes Aragão: 201665565C


import System.Random (randomRIO, random)

-- funcionandoo porem parece errada

compareValue (_,[],p) = return p
compareValue (key,x:xs, parcial) = do 
    if key == x then do
        let z = parcial + 1
        compareValue (key, xs, z)
    else 
        compareValue (key,xs,parcial)

-------------------------------------------------------------

printRandomDigit :: IO ()
printRandomDigit = do
    digit <- randomRIO('0','9' :: Char)
    print digit

printListItems :: Show a => [a] -> IO ()
printListItems [] = return ()
printListItems (head:tail) = do 
    print head
    printListItems tail

generateAndPrintDigits 0 = return ()
generateAndPrintDigits amount = do 
    digit <-randomRIO('0','9' :: Char)
    print digit
    generateAndPrintDigits (amount-1)

checkListsEquality ([],[]) = return True
checkListsEquality (head1:tail1,head2:tail2) = do 
    if head1 == head2 then
        checkListsEquality (tail1,tail2)
    else
        return False

--------------------------------------------------------------