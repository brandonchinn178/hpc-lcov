import Example

-- Skip foo, do one branch of bar, do multiple baz
main :: IO ()
main = do
  -- print $ foo 0
  -- print $ foo 10
  -- print $ foo 20
  -- print $ bar 0
  print $ bar 10
  print $ bar 20
  print $ baz 0
  print $ baz 10
  print $ baz 20
