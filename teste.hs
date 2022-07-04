-- funcionandoo porem parece errada
compareValue (_,[],p) = return p
compareValue (key,x:xs, parcial) = do 
    if key == x then do
        let z = parcial + 1
        compareValue (key, xs, z)
    else 
        compareValue (key,xs,parcial)



