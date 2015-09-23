
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' = foldr1 (const id)

myLast'' = foldr1 (flip const)
 
myLast''' = head . reverse
 
myLast'''' = foldl1 (curry snd)
 
myLast''''' [] = error "No end for empty lists!"  
myLast''''' x = x !! (length x -1)


myButLast :: [a] -> a
myButLast = last . init
 
myButLast' x = reverse x !! 1
 
myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs
 
myButLast''' (x:(_:[])) = x
myButLast''' (_:xs) = myButLast''' xs
 
myButLast'''' = head . tail . reverse