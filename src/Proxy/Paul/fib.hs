import Debug.Trace

fib = ((map fib' [0 ..]) !!)
    where
      fib' 0 = 0
      fib' 1 = 1
      fib' n = traceShow n $ (fib (n - 1) + fib (n - 2))
      
fib2 x =  (((map fib' [0 ..]) !!) x)
    where
      fib' 0 = 0
      fib' 1 = traceShow (take 7 $ map fib' [0 ..]) 1
      fib' n = fib (n - 1) + fib (n - 2)
      
f :: [a] -> a
f (x:xs) = x