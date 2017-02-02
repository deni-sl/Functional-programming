import Data.List

keepsInside :: [[Int]] -> (Int -> Int) -> [[Int]]
keepsInside matrix f  = filter isMember (transpose matrix) 
    where
	isMember x = helper x x
	    where
		helper (x:xs) y = 
		    if (elem (f x) y)
			then helper xs y
			else False
		helper [] y = True
		
main :: IO()
main = do
    print (keepsInside [[1,0,5],[-1,0,2]] (\x -> x^2))