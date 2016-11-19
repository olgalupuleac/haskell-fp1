-- 1. head' ���������� ������ ������� ��������� ������ (0,5 �����)
head' :: [a] -> a
head' (x:_) = x

-- 2. tail' ���������� ������ ��� ������� ��������, ��� ������� - ������ (0,5)
tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' []     = []

-- 3. take' ���������� ������ n >= 0 ��������� ��������� ������ (0,5)
take' :: Int -> [a] -> [a]
take' n (x:xs) |n > 0    = x:take' (n - 1) xs
take' _ _                = []

-- 4. drop' ���������� ������ ��� ������ n >= 0 ���������; ���� n ������ ����� -- ������, �� ������ ������. (0,5)
drop' :: Int -> [a] -> [a]
drop' 0 l       = l
drop' n (_:xs)  = drop' (n - 1) xs
drop' _ _       = []

-- 5. filter' ���������� ������ �� ���������, ��� ������� f ���������� True (0,5)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f (x:xs) |f x       = x:filter' f xs
                 |otherwise = filter' f xs
filter' _ _                 = []

-- 6. foldl' ��������������� ��������� ������� f � �������� ������ l � ��������, ����������� �� ���������� ����, ��������� �������� z (0,5)
-- foldl' (+) 0 [1, 2, 3] == (((0 + 1) + 2) + 3)   
-- foldl' (*) 4 [] == 4
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z (x:xs) = foldl' f (f z x) xs
foldl' _ z _      = z

-- 7. concat' ��������� �� ���� ��� ������ � ���������� �� ������������ (0,5)
-- concat' [1,2] [3] == [1,2,3]
concat' :: [a] -> [a] -> [a]
concat' (x:xs) l = x:concat' xs l
concat' _ l      = l

-- 8. quickSort' ���������� ��� ��������������� ������ (0,5)
quickSort' :: Ord a => [a] -> [a]
quickSort' (x : xs) = concat' (concat' (quickSort' (filter' (< x) (x : xs) ) ) (
                                        filter' (== x) (x : xs) ) )  (
                                        quickSort' (filter' (> x) (x : xs) ) )
quickSort' _        = []