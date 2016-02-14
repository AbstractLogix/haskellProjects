sumWith v [] = v
sumWith v (x:xs) = (sumWith $! (v + x)) xs

-- $! forces evaluation 'Strict Application Operator'

{- in this case using the $! operator forces the evaluation of v + x before the
recursive call -}


