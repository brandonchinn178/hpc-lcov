module Example where

foo :: Int -> Int
foo = id

bar :: Int -> Int
bar x = if x == 0 then 100 else x

baz :: Int -> Int
baz 0 = 100
baz x = x
