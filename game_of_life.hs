import Data.Map as Map
start_board = [((0,0), True), ((0,1), True), ((1,1), True)]
board = Map.fromList start_board

has_life x y
    | Map.member (x,y) board = 1
    | otherwise              = 0

neighbor_count x y = 
    [ has_life a b | a <- [x-1, x, x+1], b <- [y-1, y, y+1] ]

will_live x y nc
    | has_life x y == 1 && nc < 2   = False
    | has_life x y == 1 && nc > 3   = False
    | has_life x y == 0 && nc == 3  = True
    | otherwise                     = True
        
main :: IO ()
main = do
    let nc = neighbor_count 1 1
    print $ sum nc - has_life 1 1
    print $ will_live 1 1 $ sum nc
    putStrLn "byebye"
