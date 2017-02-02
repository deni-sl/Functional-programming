import Data.List

matrixInfList :: [[a]] -> [a]
matrixList matrix = foldl1 (++) (transpose matrix)
matrixInfList matrix = cycle (matrixList matrix)

main :: IO()
main = do
    print (matrixInfList [[1,2],[3,4],[5,6]])