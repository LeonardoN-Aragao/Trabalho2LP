-- funcionandoo porem parece errada
compareValue (_,[],p) = return p
compareValue (key,x:xs, parcial) = do 
    if key == x then do
        let z = parcial + 1
        compareValue (key, xs, z)
    else 
        compareValue (key,xs,parcial)



getInput = do
    putStr "teste "
    input <- getLine
    let aux = remove ' ' input
    let tam = length tent

    if (tam < 0 && tam > 5) && verifyNumber aux then
        putStrLn "Quantidade ou número inválido, por favor digite novamente 4 números de 1-6"
        getInput
    else


        -- ord "awdawd2" == 2

        map digitToInt ['2','2','4']