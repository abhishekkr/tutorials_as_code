import Data.Char

echo :: IO ()                                                                   
echo =  getLine >>= \line ->                                                    
        if line == "" then                                                      
            return ()                                                             
        else                                                                    
            putStrLn (map toUpper line) >>                                        
            echo                                                                  

main :: IO ()                                                                   
main = echo
