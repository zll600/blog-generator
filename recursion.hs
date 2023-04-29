replicate :: Int -> a -> [a]
replicate n x =
  if n <= 0
    then []
    else x : replicate (n - 1) x

even :: Int -> Bool
even n =
  if n == 0
    then True
    else odd (n - 1)

odd :: Int -> Bool
odd n =
  if n == 0
    then False
    else even (n - 1)
