sum :: Num a => [a] -> a
sum l
	| length l == 1 = head l
	| otherwise = sum (drop 1 l) + head l
  
finalGrade :: Num a => [a] -> [a] -> a
finalGrade l1 l2 = 
    let weightedSum = [x*y | x <- l1, y <- l2]
    in  (sum weighted_sum) / (sum l2)
