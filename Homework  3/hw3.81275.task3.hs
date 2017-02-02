type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = [Edge]

neighbours :: Graph -> Vertex -> [Vertex]
neighbours [] _ = []
neighbours (x:g) v =
    if (fst x == v)
	then snd x : neighbours g v
	else neighbours g v

isSumReachable :: Graph -> Vertex -> Int -> Bool
isSumReachable g v n 
    | v >= n = False
    | otherwise = helper (neighbours g v) v
	where
	helper :: [Vertex] -> Int -> Bool
	helper [] _  = False
	helper (currVer:c) sum
	    |sum == n = True
	    |(null (neighbours g currVer)) && sum == n = True
	    |(null (neighbours g currVer)) && sum /= n = False
	    |otherwise = not(null (filter (\x -> x == True) (map (\x -> (helper c (sum + currVer))) (neighbours g v))))

main :: IO ()
main = do
    --print (neighbours g 1)
    print (isSumReachable g 2 7)
    where
	    g = [(1,2), (1,3), (1,4), (2,5), (3,6), (4,5), (5,6)]
    
     
  