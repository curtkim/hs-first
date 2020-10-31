module Simple
where

-- calculates the arithmetic mean of two numbers
--
arithmetic_mean :: Fractional a => a -> a -> a
arithmetic_mean x y  = (x + y)  / 2