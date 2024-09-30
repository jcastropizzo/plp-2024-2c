module Main (main) where

-- ej 2
valorAbsoluto :: Float -> Float
valorAbsoluto x = fromIntegral (floor x :: Int)

bisiesto :: Int -> Bool
bisiesto x = mod x 4 == 0

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int  -> Bool
isPrime k = (k > 1) && null [ x | x <- [2..isqrt k], k `mod` x == 0]

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n
  | n >= 2    = length [x | x <- [2..n], n `mod` x == 0 && isPrime x]
  | otherwise = 0

-- ej 3
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero x =
  case x of
    Left w -> w
    Right w -> if w then 1 else 0

-- ej 4
limpiar :: String -> String -> String
-- que elimina todas las apariciones de cualquier carácter de la primera
-- cadena en la segunda. Por ejemplo, limpiar ``susto'' ``puerta'' evalúa a ``pera''. Nota: String es un
-- renombre de [Char]. La notación ``hola'' es equivalente a [`h',`o',`l',`a'] y a `h':`o':`l':`a':[]
limpiar _ [] = ""
limpiar charsALimpiar (x:xs) = if x `elem` charsALimpiar then rec else x:rec
  where rec = limpiar charsALimpiar xs

difPromedio :: [Float] -> [Float]
-- que dada una lista de números devuelve la diferencia de cada uno con el
-- promedio general. Por ejemplo, difPromedio [2, 3, 4] evalúa a [-1, 0, 1].
-- difPromedio l = foldr (\x result -> (x-avg) : result ) [] l
--   where avg = (sum l) / fromIntegral(length l)
difPromedio l = map (\ x -> x - avg) l
  where avg = sum l / fromIntegral (length l)

todosIguales :: [Int] -> Bool
-- que indica si una lista de enteros tiene todos sus elementos iguales.
todosIguales l = (length l < 2 ) || foldr (\c r -> (c == v) && r) True l
  where v = head l

-- ej 5
data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
-- que indica si un árbol es vacío (i.e. no tiene nodos)
vacioAB a = case a of
  Nil -> True
  Bin {} -> False

negacionAB :: AB Bool -> AB Bool
-- que dado un árbol de booleanos construye otro formado por la negación
-- de cada uno de los nodos
negacionAB a = case a of
  Nil -> Nil
  Bin l v r -> Bin r v l

productoAB :: AB Int -> Int
productoAB a = case a of
  Nil -> 1
  Bin l v r -> v * productoAB l * productoAB r

main :: IO ()
main = do
  -- print (aEntero (Left 42))    -- Output: 42
  -- print (aEntero (Right True)) -- Output: 1
  -- print (aEntero (Right False)) -- Output: 0
  print (limpiar "susto" "puerta") -- Output: 0
  print (difPromedio [2,3,4]) -- Output: 0
  print (todosIguales [1]) -- Output: 0
  print (todosIguales [1,1]) -- Output: 0
  print (todosIguales [1,2]) -- Output: 0
  print (todosIguales [1,1,1]) -- Output: 0
  print (todosIguales [1,1,2]) -- Output: 0
