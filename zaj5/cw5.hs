import Control.Monad

-- Napisać program, który będzie pytał o dwie liczby, a następnie wypisze ich sumę, iloczyn
-- i różnicę
zad1 = do
    putStrLn "Podaj pierwszą liczbe: "
    x <- getLine
    putStrLn "Podaj drugą liczbe: " 
    y <- getLine
    let xInt = (read x :: Int)
    let yInt = (read y :: Int)
    print (xInt + yInt)
    print (xInt * yInt)
    print (xInt - yInt)

-- Napisać program, który będzie pytał o dwie liczby, a następnie wypisze ich największy
-- wspólny dzielnik i najmniejszą wspólną wielokrotność.
zad2 = do
    putStrLn "Podaj pierwszą liczbe: "
    x <- getLine
    putStrLn "Podaj drugą liczbe: " 
    y <- getLine
    let xInt = (read x :: Int)
    let yInt = (read y :: Int)
    print (gcd xInt yInt)
    
-- Napisać program, który będzie pytał o imię i nazwisko, a następnie wypisze inicjały imienia
-- i nazwiska.    

zad3 = do
    putStrLn "Podaj imię: "
    name <- getLine
    putStrLn "Podaj nazwisko: "
    lastname <- getLine
    print (head name)
    print (head lastname)

-- Napisać program „grę”, który prosi użytkownika o podanie liczby z zakresu 0-99
-- i podpowiada, czy wprowadzona liczba jest większa, czy mniejsza od ustalonej liczby.
-- Program kończy działanie w przypadku odgadnięcia liczby lub po 10 próbach. 

zad4 = do
    putStrLn "Podaj liczbe z zakresu [0..99] "
    number <- getLine
    let intNumber = (read number :: Int)
    zad4Helper (0, intNumber)

zad4Helper (n ,num) = do
    putStrLn "Podaj liczbe druga .."
    secondNum <- getLine
    let intSecondNum = (read secondNum :: Int)
    when (num /= intSecondNum && n < 10) $ do
        if num > intSecondNum           
            then putStr ("Liczba " ++  secondNum ++ " mniejsza od " ++ show num ++ "\n")
            else putStr ("Liczba " ++  secondNum ++ " wieksza od " ++ show num ++ "\n")
        zad4Helper (n+1, num)