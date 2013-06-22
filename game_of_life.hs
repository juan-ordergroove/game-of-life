import Data.Map as Map
start_board = [((0,0), True), ((0,1), True), ((1,1), True)]
board = Map.fromList start_board
board_size = 20

has_life :: Int -> Int -> Int
has_life x y
    | Map.member (x,y) board = 1
    | otherwise              = 0

neighbor_count :: Int -> Int -> [Int]
neighbor_count x y = 
    [ has_life a b | a <- [x-1, x, x+1], b <- [y-1, y, y+1] ]

will_live :: Int -> Int -> Int -> Bool
will_live x y nc
    | alive == 1 && nc < 2  = False
    | alive == 1 && nc > 3  = False
    | alive == 0 && nc == 3 = True
    | otherwise             = True
    where alive = has_life x y

main :: IO ()
main = do
    print $ sum nc - has_life 1 1
    print $ will_live 1 1 $ sum nc
    where nc = neighbor_count 1 1
