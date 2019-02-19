## Values That Can Fail That Can Fail
Exploring Functor and Monad in a DIY way.


### 1) applying functions to values

*I can enter a price under 1000, and the program displays the total prices as calculated with Utah tax rate (6.85). The total price is rounded to the nearest cent.*

```haskell
    rounded 99.994 ⏎
    99.99
    rounded 99.995 ⏎
    100.00
    
    totalPrice 100 ⏎
    106.85
    totalPrice 42 ⏎
    44.88
    
    rounded = (/ 100) . fromInteger . round . (* 100)
    
    totalPrice p = rounded (p * 1.0685)
    
    process f = interact (unlines . map f . lines)
    
    main = process (show . totalPrice . read)
```

### 2) Price values in computations that can fail

*When I forget to enter a price, then the program prints "missing price"*    

We need a new type for values in computations that can fail:

```haskell
    data FPrice = Value Double
                | Fail
        deriving Show
```

Then we can scan the line of input, looking for the first argument:

```haskell
    readValue "42" ⏎
    Value 42.0
    readValue "" ⏎
    Fail
    
    readValue s = case words s of
        []    -> Fail
        (x:_) -> Value (read s) 
```

Once we have a value in a context of a computation that can fail, we need to calculate the total price with it:

```haskell
    totalPrice' (Value 42) ⏎
    Value 44.88
    totalPrice' Fail ⏎
    Fail
    
    totalPrice' (Value v) = Value (totalPrice v)
    totalPrice' Fail      = Fail
```

Then we need to show the result, extracting the value from the context, or displaying a message.

```haskell
    showValue Fail ⏎
    missing price
    showValue (Value 44.88) ⏎
    44.88
    
    showValue (Value v) = show v
    showValue Fail      = "missing price"
```

And now we can use FPrice in our main program:

```haskell
    main = process (showValue . totalPrice' . readValue)
```

### 3) generalizing values in context of computations that can fail

*When I enter foo, the program prints: not a number:foo*

We can store information about the failure in our data type:

```haskell
    data FPrice = Value Double
                | Fail String
        deriving Show
```

That way, we can have our value respond to different cases:

```haskell
    readValue "" ⏎
    Fail "missing price"
    readValue "foo" ⏎
    Fail "not a number:foo"
    readValue "42" ⏎
    Value 42.0
    
    readValue s = case words s of
        (x:_) -> case (reads x) of
                    [(v,_)] -> Value v
                    []      -> Fail ("not a number:" ++ s) 
        []    -> Fail "missing price"
```

We need to adapt the functions that calculate the total price and show the result:

```haskell
    totalPrice' (Fail s)  = Fail s
    totalPrice' (Value v) = Value (totalPrice v)
    
    showValue (Fail s)  = s
    showValue (Value v) = show v
```

Running the program:

```haskell
    42 ⏎
    44.88
     ⏎
    missing price
    foo ⏎
    not a number:foo
```

### 4) mapping functions over values in a context

*Total Price is printed on ten positions, with two digits after decimal point.*

What we want is to convert the value in the FPrice context from Double to String, but we then we need a more general type:

```haskell
    data FValue a = Value a | Fail String deriving Show
```

and now we can have:

```haskell
    printPrice (Value 42) ⏎
    Value "     42.00"
    printPrice (Fail "oops") ⏎
    Fail "oops"
    
    printPrice :: FValue Double -> FValue String
    printPrice (Value v) = Value (printf "%10.2f" v)
    printPrice (Fail  s) = Fail s
```

and we adapt the functions that shows the result:

```haskell
    showValue :: FValue String -> String
    showValue (Fail s)  = s
    showValue (Value v) = v
```

Running the program:

```haskell
    42 ⏎
         44.88
    100 ⏎
        106.85
      ⏎
    missing price
    foo ⏎
    not a number:foo
```

Compare totalPrice and printPrice

```haskell
    totalPrice' :: FValue Double -> FValue Double
    totalPrice' (Value v) = Value (totalPrice v)
    totalPrice' (Fail s)  = Fail s
    
    printPrice :: FValue Double -> FValue String
    printPrice (Value v) = Value (printf "%10.2f" v)
    printPrice (Fail  s) = Fail s
```

If we can create a function that maps any simple function on our value in context, we can get rid of those repetitious functions:

```haskell
    fmap :: (a -> b) -> (fvalue a) -> (fvalue b)
    fMap f (Value v) = Value (f v)
    fMap _ (Fail s)  = Fail s 
    
    main = process (showValue . fMap (printf "%10.2f") . fMap totalPrice . readValue)
```

### 5) binding functions producing values in a context

*When I enter a price lower or equal to zero, or a price with more than 3 digits after decimal point, the programs displays an error*

This function checks if a Price value is positive or transforms it into a Fail:

```haskell
    checkPositive (Value 42) ⏎
    Value 42.0
    checkPositive (Value 0) ⏎
    Fail "not a checkPositive price"
    checkPositive (Value (-42)) ⏎
    Fail "not a checkPositive price"
    checkPositive (Fail "oops") ⏎
    Fail "oops"
    
    checkPositive :: FValue Double -> FValue Double
    checkPositive (Fail s) = Fail s
    checkPositive (Value p) | p > 0     = Value p
                       | otherwise = Fail "not a positive price"
```

And this function checks if a Price value has no more than 2 digits after decimal point:

```haskell
    checkCents (Value 42.01) ⏎
    Value 42.01
    checkCents (Value 42.005) ⏎
    Fail "not a correct price"
    checkCents (Fail "oops") ⏎
    Fail "oops"
    
    checkCents :: FValue Double -> FValue Double
    checkCents (Fail s) = Fail s
    checkCents (Value p) | rounded (p * 100) == (p * 100) = Value p
                         | otherwise                      = Fail "not a price"
```

We can generalize this approach of checking FValues:

```haskell
    checkArg :: FValue String -> FValue String
    checkArg (Fail s) = Fail s
    checkArg (Value s) = case words s of
        (x:_) -> Value s
        []    -> Fail "missing price"
    
    checkValue :: FValue String -> FValue Double
    checkValue (Fail s) = Fail s
    checkValue (Value s) = case (reads s) of
        [(v,_)] -> Value v
        []      -> Fail ("not a number:" ++ s) 
    
    checkPositive :: FValue Double -> FValue Double
    checkPositive (Fail s) = Fail s
    checkPositive (Value p) | p > 0     = Value p
                            | otherwise = Fail "not a positive price"
     
    checkCents :: FValue Double -> FValue Double
    checkCents (Fail s) = Fail s
    checkCents (Value p) | rounded (p * 100) == (p * 100) = Value p
                         | otherwise                      = Fail "not a price"
    
    main = process (showValue . 
                    fMap (printf "%10.2f") . 
                    fMap totalPrice . 
                    checkCents .
                    checkPositive .
                    checkValue .
                    checkArg .
                    Value)
```

Running the program:

```haskell
    42 ⏎
         44.88
    -42 ⏎
    not a positive price
    42.001 ⏎
    not a price
    foo ⏎
    not a number:foo
    ⏎
    missing price
```

The repetition of the Fail pattern in all our functions suggest we use fmap, mapping a function from a to FValue b on a FValue a. For example:


```haskell
    checkValue :: String -> FValue Double
    checkValue s = case (reads s) of
        [(v,_)] -> Value v
        []      -> Fail ("not a number:" ++ s) 
    
    checkValue "42" ⏎
    Value 42.0
    checkValue "foo" ⏎
    Fail "not a number:foo"
```

But when we map this function over a FValue, we get a FValue (FValue a) result.

```haskell
    fMap checkValue (Value "42") ⏎
    Value (Value 42.0)
    fMap checkValue (Value "foo") ⏎
    Value (Fail "not a number:foo")
    fMap checkValue (Fail "oops") ⏎
    Fail "oops"
```

We need a function to transform a FValue (FValue a) back into a FValue a:

```haskell
    fJoin :: FValue (FValue a) -> FValue a
    fJoin (Value fv) = fv
    fJoin (Fail s)   = Fail s
    
    fJoin (fMap checkValue (Value "42")) ⏎
    Value 42.0
    fJoin (fMap checkValue (Value "foo")) ⏎
    Fail "not a number:foo"
    fJoin (fMap checkValue (Fail "oops")) ⏎
    Fail "oops"
```

Thus binding a function from a to FValue b to a FValue a is done by mapping then joining:

```haskell
    fBind :: (a -> FValue b) -> FValue a -> FValue b
    fBind g = fJoin . fMap g
```

Wich simplifies particular functions:

```askell
fMap :: (a -> b) -> (FValue a) -> (FValue b)
fMap f (Value v) = Value (f v)
fMap _ (Fail s)  = Fail s 

fJoin :: FValue (FValue a) -> FValue a
fJoin (Value fv) = fv
fJoin (Fail s)   = Fail s

fBind :: (a -> FValue b) -> FValue a -> FValue b
fBind g = fJoin . fMap g

checkArg :: String -> FValue String
checkArg s = case words s of
    (x:_) -> Value s
    []    -> Fail "missing price"

checkValue :: String -> FValue Double
checkValue s = case (reads s) of
    [(v,_)] -> Value v
    []      -> Fail ("not a number:" ++ s) 

checkPositive :: Double -> FValue Double
checkPositive p | p > 0     = Value p
                | otherwise = Fail "not a positive price"
 
checkCents :: Double -> FValue Double
checkCents p | rounded (p * 100) == (p * 100) = Value p
             | otherwise                      = Fail "not a price"

main = process (showValue . 
                fMap (printf "%10.2f") . 
                fMap totalPrice . 
                fBind checkCents .
                fBind checkPositive .
                fBind checkValue .
                fBind checkArg .
                Value)
```

