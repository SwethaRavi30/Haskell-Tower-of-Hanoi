creatHanoi :: Int ->Int ->[[Int]]
creatHanoi 1 _ = [[]]
creatHanoi x y = [[1..y] ]++creatHanoi (x-1) 0

movedisc :: ([[Int]],Int,Int)->[[Int]]

-- take (y-1) xs)  -> take all towers before source tower
-- ([tail (xs!!(y-1))])  -> all disk insource tower except the top disk
-- (drop y(take (z-1) xs)) -> take all towers after source till destination
-- ([(head (xs!!(y-1)) ):(xs!!(z-1))]) -> add the top disk on source tower to the top of destination tower
-- (drop z xs) -> to get the last tower, it will be empty if destination is the last tower
movedisc (xs,y,z) =if (y<z) then 
    take (y-1) xs ++ [tail (xs!!(y-1))] ++drop y(take (z-1) xs)++ [(head (xs!!(y-1)) ):(xs!!(z-1))] ++drop z xs
        else
            take (z-1)xs++[(head (xs!!(y-1)) ):(xs!!(z-1))]++drop z(take (y-1) xs)++[tail (xs!!(y-1))]++drop y xs

moveValid::([[Int]],Int,Int)->Bool
moveValid (xs,y,z) = if (y>length xs ||z>length xs) then False
                        else 
                        if (xs!!(y-1))==[] then False
                         else 
                         if (xs!!(z-1))==[] then True
                         else
                         if head(xs!!(y-1))< head (xs!!(z-1)) then True
                         else False
 
--io function that creats the hanoi tower list
startHanoi:: IO [[Int]]
startHanoi= do 
            putStrLn("Play a game of Hanoi tower.Type the number of tower ")
            x<-getLine 
            putStrLn("Type the number of disc")
            y<-getLine
            putStrLn("You selected "++ x ++" towers and "++ y ++" discs")
            putStrLn("game started")
            return (creatHanoi (read x::Int) (read y::Int))
--self recursive game loop that takes IO and plays the game 
playHanoi::[[Int]]->IO [[Int]]
playHanoi xss= if (all null (init xss)) then 
                     do
                     showHanoi xss
                     putStrLn("You win")
                     return xss
                else
                    do
                     showHanoi xss
                     putStrLn("enter the peg to make move from")
                     x<-getLine 
                     putStrLn("enter the peg move to") 
                     y<-getLine
                     if moveValid(xss,(read x::Int),(read y::Int)) then
 
                        playHanoi (movedisc (xss,(read x::Int),(read y::Int)))
                     else
                        do
                         putStrLn("invalid move") 
                         playHanoi xss


             
--A helper function that prints out the tower on the screen. 
showHanoi::[[Int]]->IO ()
showHanoi xss=putStrLn (show xss)

main:: IO ()
main = do 
             x<-startHanoi 
             playHanoi x
             return ()
