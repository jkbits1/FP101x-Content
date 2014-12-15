


-- looks interesting
-- https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
-- http://www.haskellforall.com/2012/12/the-continuation-monad.html
-- http://marc.info/?l=haskell-cafe&m=140646064120025&w=2
-- https://www.fpcomplete.com/school/advanced-haskell/the-mother-of-all-monads    

-- so can use forever fn
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Identity

-- for callcc
import Control.Monad.Cont

add :: Int -> Int -> Int
add x y = x +y

square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth x y = add (square x) (square y)


-- add :: Int -> Int -> Int
add_cps :: Int ->Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

pyth_cps :: Int -> Int -> ((Int -> r) -> r)
pyth_cps x y = \k ->
  square_cps x $ \x_squ ->
  square_cps y $ \y_squ ->
  add_cps x_squ y_squ $ k
  
  
-- converting pyth_cps to a fn, test_cps, that expects one int only

-- addOne :: Int -> Int
-- add x = x + 1
addOne_cps :: Int -> ((Int -> r) -> r)
addOne_cps x = \k -> k (x + 1)

test_cps :: Int -> ((Int -> r) -> r)
test_cps x = \k ->
  square_cps x $ \x_squ ->
  addOne_cps x_squ $ k
  
-- same fn, but on one line
test_cps1 x = \k -> square_cps x $ \x_squ -> addOne_cps x_squ $ k

-- moving k to left side
test_cps1a x k = square_cps x $ \x_squ -> addOne_cps x_squ $ k

test_cps1b x k = square_cps x $ \x_squ -> ad $ k

test_cps1c x k = square_cps x $ (\x_squ -> ad $ k)
test_cps1d x k = square_cps x $ (\x_squ -> (ad $ k))
test_cps1e x k = square_cps x $ (\x_squ -> (ad $ print))

test_cps2 x k = square_cps x $ (fn_x2 )
test_cps2a x k = square_cps x $ (fn_x2a)

-- moving k back to right side
test_cps2b x = \k -> square_cps x $ (fn_x2a)


  
-- test_cps2 :: Int -> ((Int -> r) -> r)
-- test_cps2 x = \k -> square_cps x $          (fn_x 1 print)
-- test_cps2a x = \k -> square_cps x $          (fn_x 1 print)
-- test_cps2a x = \k -> square_cps x $          k

-- test_cps2 x = \k -> square_cps x $          (fn_x 1 print)
-- test_cps2 x = \k -> square_cps x $          (fn_x 1) $ print
-- test_cps2 x = \k -> square_cps x $          (fn_x1a) $ print

  
-- $ \x_squ ->  addOne_cps x_squ $ k
-- fn_x = \x_squ ->  addOne_cps x_squ $ k
fn_x x_squ k =                             (addOne_cps x_squ) $ k
-- fn_x 2 print

fn_x1 x_squ =                               (addOne_cps x_squ) 

fn_x1a x_squ =                               (ad) 

-- ignore param, but provide it anyway  
fn_x2 :: Int -> IO ()
fn_x2 x = ad $ print

fn_x2a :: Int -> IO ()
fn_x2a x_squ = addOne_cps x_squ $ print
  
-- test bits

ad = addOne_cps 1 

ac = add_cps 1 2

sqc = square_cps 3

sqc2 = square_cps 4

-- ac2 = add_cps sqc sqc2

-- can't get this to work in ghci
-- add_cps sqc sqc2 $ print

-- :t ($ 2) :: (Int -> IO()) -> IO()

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

-- call as - thrice_cps addOne_cps 1 print (first param is a cps fn)
thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k -> f_cps x $ \fx -> f_cps fx $ \ffx -> f_cps ffx $ k

chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r) 
-- s is equiv of addOne, square etc
-- f is something like the rightmost lambda expression (not quite sure)
-- addOne_cps x = \k -> k (x + 1)
chainCPS s f = \k -> s $ \x -> f x $ k

-- convert to monad >>= (not sure)
-- (>>=)  :: m a -> (a -> m b) -> m b (from hutton monads doc)
-- Monad m => m a, Monad m => m b
-- chainCPS :: m a -> (a -> m b) -> m b

-- this a wrapper
cont :: ((a -> r) -> r) -> Cont r a

-- this is an unwrapper and runner
runCont :: Cont r a -> ((a -> r) -> r)

-- from Cont.hs (Control.Monad.Cont)
-- | Construct a continuation-passing computation from a function.
-- (The inverse of 'runCont'.)
-- cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT (\ k -> Identity (f (runIdentity . k)))

-- | Runs a CPS computation, returns its result after applying the final
-- continuation to it.
-- (The inverse of 'cont'.)
-- runCont :: Cont r a	-- ^ continuation computation (@Cont@).
    -- -> (a -> r)		-- ^ the final continuation, which produces
			-- -- the final result (often 'id').
    -- -> r
runCont m k = runIdentity (runContT m (Identity . k))

-- from Cont.hs (Control.Monad.Cont)
-- instance Monad (ContT r m) where
    -- return a = ContT ($ a)
    -- m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)
    
-- from  https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style#cite_ref-1
-- instance Monad (Cont r) where
  -- return x = Main.cont ($ x)
  -- s >>= f = Main.cont $ \c -> Main.runCont s $ \x -> Main.runCont (f x) c
  
  




-- add :: Int ->Int -> Int
-- add_cps :: Int ->Int -> ((Int -> r) -> r)

-- add_cps :: Int ->Int -> ((Int -> r) -> r)
-- add_cps x y = \k -> k (add x y)

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (add x y)

-- square :: Int ->Int 
-- square_cps :: Int -> ((Int -> r) -> r)
square_cont :: Int -> Cont r Int
square_cont x = return (square x)

-- pyth :: Int -> Int -> Int
-- pyth_cps :: Int -> Int -> ((Int -> r) -> r)
pyth_cont :: Int -> Int -> Cont r Int
pyth_cont x y = do
    x_squ <- square_cont x
    y_squ <- square_cont y
    add_cont x_squ y_squ
    
-- from     http://www.stephendiehl.com/what/
add_what :: Int -> Int -> Cont r Int
add_what x y = return $ x + y

mult_what :: Int -> Int -> Cont r Int
mult_what x y = return $ x * y


-- ex1 :: IO ()
-- ex1 = print $ Main.runCont (f >>= g) id
ex1what x y z = print $ Control.Monad.Cont.runCont (f >>= g) id
  where
    f = add_cont x y -- both *_cont or *_what fns work
    g = mult_what z -- takes its second param from f

-- same as ex1what (used for call below)    
ex2what x y z = print $ Control.Monad.Cont.runCont (f >>= g) id
  where
    f = add_cont x y -- both *_cont or *_what fns work
    g = mult_what z -- takes its second param from f

bothWhats = do
    ex1what 2 3 4
    ex2what 1 2 3
    
    
-- ex2 :: IO ()
-- ex2 = print $ Control.Monad.Trans.Cont.runCont Control.Monad.Cont.callCC show
-- -- "3"



-- from https://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-Cont.html
calcLen :: [a] -> Cont r Int
calcLen xs = return (length xs)

double :: Int -> Cont r Int
double n = return (n*2)

getLen = do
    Control.Monad.Cont.runCont (calcLen "123") print

getDblLen = do
    Control.Monad.Cont.runCont (calcLen "123" >>= double) print

-- using this form with add_cont & add_what from other examples
getAdd1a x y = do
    Control.Monad.Cont.runCont (add_cont x y) print
    
getAdd2 = do
    Control.Monad.Cont.runCont (add_what 2 3) print

getPyth2 x y = do
    Control.Monad.Cont.runCont (pyth_cont x y) print

-- getPyth2a x y = do
    -- Control.Monad.Cont.runCont (pyth_cont 2 3) id
    -- Control.Monad.Cont.runCont (pyth_cont 2 3) id
    
    -- x_squ <- square_cont x
    -- y_squ <- square_cont y
    -- add_cont x_squ y_squ

    
-- this works, but don't understand it yet
-- uses ContT ??    
-- from https://www.fpcomplete.com/school/advanced-haskell/the-mother-of-all-monads    
ex1a = do
    a <- return 1
    b <- return 10
    return $ a+b

test1 = Control.Monad.Cont.runCont ex1a show    

-- type is from ghci, don't understand it
-- getPyth3 :: ContT r Identity Int
getPyth3 x y = do      
    -- fns below don't need returns as each is already a Cont (I think)
    x_squ <- square_cont x 
    y_squ <- square_cont y 
    add_cont x_squ y_squ

-- getPyth3 x y = do
-- getPyth3 = do
    -- a <- return 1
    -- b <- return 10
    -- return $ a+b
    -- x_squ <- return 2 -- (square_cont 2)
    -- return (square_cont x)
    -- return $ (square_cont 1)
    -- return $ x_squ
    -- x_squ <- return (square_cont x)
    -- y_squ <- return square_cont y
    -- return $ add_cont x_squ y_squ

testPyth3 x y = Control.Monad.Cont.runCont (getPyth3 x y) show    
-- call as - testPyth3a 3 9 print (or show, etc)
testPyth3a x y k = Control.Monad.Cont.runCont (getPyth3 x y) k

-- equiv of above calls, in a single item (don't know if better or not,
--                              but is the same, following notes below)
-- call as - testPyth4 3 9 print (or show, etc)
testPyth4 x y k = Control.Monad.Cont.runCont 
    (do      
        x_squ <- square_cont x 
        y_squ <- square_cont y 
        add_cont x_squ y_squ) 
    k




-- ex1 :: IO ()
-- ex1 = print $ Main.runCont (f >>= g) id
-- ex1 = do print $ Control.Monad.Cont.runCont (f >>= g) id
-- ex1 = Control.Monad.Cont.runCont (f >>= g) print --id
-- ex1 = do Control.Monad.Cont.runCont (f >>= g) print id
          -- where
            -- -- f = add_cont 1 2
            -- f = add_what 1 2
            -- -- g = add_cont 1 2
            -- g = add_what 1 2
    
-- from https://sites.google.com/site/haskell/notes/alittletrick    
-- (flip runCont) id $ do
  -- ...

-- Here we see another arguably better solution:
-- (`runCont` id) $ do 
   -- ...

-- which translates to
-- (do ...) `runCont` id

-- and then
-- runCont (do ...) id    
    
    

onInput :: (String -> IO()) -> IO()

onInput f = forever $ do
    str <- getLine
    f str
    
-- fns to pass to onInput, e.g. onInput testStrLn
appendStrLn s a = putStrLn (s ++ a)    

testStrLn s = appendStrLn s " - test"

-- from  https://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-Cont.html#g:6

-- import Control.Monad.Cont
-- import System.IO

-- main = do
  -- hSetBuffering stdout NoBuffering
  -- runContT (callCC askString) reportResult

-- askString :: (String -> ContT () IO String) -> ContT () IO String
-- askString next = do
  -- liftIO $ putStrLn "Please enter a string"
  -- s <- liftIO $ getLine
  -- next s

-- reportResult :: String -> IO ()
-- reportResult s = do
  -- putStrLn ("You entered: " ++ s)
