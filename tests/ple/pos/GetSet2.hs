{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple"        @-}

module ReflString3 where 

type State = String -> Int 

{-@ reflect set @-}
set :: State -> String -> Int -> State 
set s x v y = if x == y then v else s y

{-@ reflect get @-}
get :: State -> String -> Int 
get s x = s x 

{-@ ok :: s:State -> { get (set s "x" 10) "x" == 10 } @-}
ok :: State -> () 
ok _ = ()

{-@ fails :: s:State -> { s1:State | get s1 "x" == 10 } @-}
fails :: State -> State  
fails s = set s "x" 10
