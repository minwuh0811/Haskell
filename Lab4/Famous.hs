--Lab4B
--Group 8 { Noor alweli, Min Wu, Andi}

module Famous where
import  System.IO.Error

--Question and Answer
data QA = Question String QA QA |Answer String 
    deriving (Show, Read)

--Play is a recursive function
--returns a new Dtree which will not change if the computer guess right
--and which can be stored back into the tree file.
play :: QA -> IO QA
play (Question ques qaT qaF) = do 
    putStrLn ques
    x <- getLine
    case x of
        'y':_ -> do 
           qaTrue <-play qaT
           return (Question ques qaTrue qaF)
        'n':_ -> do 
            qaFalse<-play qaF
            return (Question ques qaT qaFalse)
        _     -> play (Question ques qaT qaF)
play (Answer q)= do
    results <- yesNoQuestion ("My guess: Is it " ++ q ++"?")
    if results
     then do
        putStrLn "Ok this time you lost"
        return (Answer q)
     else questionToFamous q

--Function that make a string of ques 
--returns a answer typed by the user/player
questionToFamous:: String -> IO QA
questionToFamous q = do
    putStrLn "Just curious: Who was your famous person?"
    name <- getLine
    putStrLn ("Give me a question for which the answer for" ++ name ++ "is \"yes\" and the answer for " ++ q ++ " is \"no\".")
    y <- getLine
    return (Question y (Answer name) (Answer q))

--Function that creates a yes/no ques (repeatedlly if needed) until it gets
--a string of yes/no and gives a boolean,
--returns a True if the answer is right
--and returns a False if the answer is wrong
yesNoQuestion :: String -> IO Bool
yesNoQuestion q = do 
    putStrLn q
    x <-getLine
    case x of
        'y':_ -> return True
        'n':_ -> return False
        _ -> yesNoQuestion q

--test 
qaTest= Question "Is she from Europe?" (Answer "Marie Curie" ) (Answer "Marilyn Monroe")

--function for running the game and define the whole game
--as main function
main:: IO()
main = do
   s <- tryIOError(readFile "famous.qa") 
   case s of
    Left ex -> writeFile "famous.qa" (show qaTest)
    Right val -> do 
        s<-readFile "famous.qa"
        out <- play (read s)
        writeFile "famous.qa" (show out)
    




