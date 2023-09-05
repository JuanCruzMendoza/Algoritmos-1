-- Ejercicio 1
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Ejercicio 2
parteEntera :: Float -> Int
parteEntera n | n >= 0 && n < 1 = 0
              | n < 0 && n > -1 = 0
              | n >= 1 = 1 + parteEntera (n-1)
              | n <= -1 = -1 + parteEntera (n+1)

-- Ejercicio 3
esDivisible :: Int -> Int -> Bool
esDivisible x y | x > 0 = esDivisible (x-y) y
                | otherwise = x == 0

-- Ejercicio 4
sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = 2*n-1 + sumaImpares (n-1)

-- Ejercicio 5
medioFact :: Int -> Int
medioFact n | n == 0  || n == 1 = 1
            | otherwise = n * medioFact (n-2)

-- Ejercicio 6
sumaDigitos :: Int ->Int
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

-- Ejercicio 7
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = mod n 10 == mod (div n 10) 10 && todosDigitosIguales (div n 10)

-- Ejercicio 8
cantDigitos :: Int -> Int
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)

iesimoDigito :: Int -> Int -> Int
iesimoDigito n i = mod (div n (10^(cantDigitos n-i))) 10

-- Ejercicio 9
ultimoNum :: Int -> Int
ultimoNum n | n < 10 = n
            | otherwise = ultimoNum (div n 10)

esCapicua :: Int -> Bool
esCapicua n = mod n 10 == ultimoNum n

-- Ejercicio 10
f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1 (n-1)

f2 :: Int -> Int -> Int
f2 1 q = q
f2 n q = q^n + f2 (n-1) q

f3 :: Int -> Int -> Int
f3 n q = f2 (2*n) q

f4 :: Int -> Int -> Int
f4 n q = f3 n q - f2 n q