Homework 6.0: Monads
Due 2017-10-08

> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

This homework (and all the ones after it), you are free to work on
your own or in groups of up to three. Please make sure that your whole
group---and only your group---are listed as collaborators on any
submission.

In this homework, you'll be working with monads in a variety of ways:
to speed up a shuffling algorithm; to create a generic framework for
MapReduce; and to generate random values for testing.

I've imported the libraries you'll need. *Look at them before you
start!* You'll want to use both the [Haskell language
documentation](https://www.haskell.org/documentation) as well as
[Hackage](https://hackage.haskell.org/) and
[Hoogle](https://www.haskell.org/hoogle/).

If you're running Haskell on your own computer and you installed the
[Haskell platform](https://www.haskell.org/platform/), you should be
able to install the two libraries we need by running at the command
line `cabal install random quickcheck` (Haskell's package manager is
named `cabal`). These libraries *should* already be installed on the
lab machines, but they may not be; the same `cabal install random
quickcheck` command should work.

You are *of course* allowed to import other libraries. It may even make your solutions easier!

> module Hw05 where
>
> import Control.Monad
>
> import Data.Array.IO
>
> import qualified Data.Map as Map
> import Data.Map (Map(..),(!))
>
> import System.Environment
> import System.Exit
> import System.IO
> import System.Random
> import Data.List

> import Test.QuickCheck
> import Test.QuickCheck.Monadic


**Problem (1): shuffling**

In this problem, we're going to "shuffle" a list, reordering it randomly.

To start, let's get familiar with
[`System.Random`](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html#v:getStdRandom).

Write a function that takes two numbers, `low` and `high`, and returns
a random number `n` such that `low <= n <= high`, i.e., inclusively
within the range.

getStdRandom 

> rand :: Int -> Int -> IO Int
> rand low high = getStdRandom (randomR (low,high))

Now write a function that takes a list and shuffles it. The
straightforward algorithm is O(n^2):

  - Given a non-empty list `xs`,
  - randomly split the list into an element `y` and the rest of the list `ys`,
  - cons `y` onto the shuffling of `ys`.

Don't worry, we'll speed it up in a minute.

IO Int
(Int -> IO [Int])
a -> [b] -> b = (\num lst -> lst !! num)

<$> :: (a->b) -> a -> b
<*> :: f (a->b) -> f a -> f b
(>>=) :: f a -> (a -> f b) -> f b


> shuffleList :: [a] -> IO [a]
> shuffleList [] = pure []
> shuffleList xs = rand 0 ((length xs)-1) >>= (\num -> let (pre, (y:ys)) = splitAt num xs 
>                                                        in  (:) <$> (pure y) <*> (shuffleList (pre ++ ys)))
>                       
>                  


Don't forget that you can run `:set +s` to get timing information in
GHCi. My implementation on my computer runs `sum <$> shuffleList
[0..10000]` 3.26 seconds.

It turns out that there's a much faster, O(n) algorithm for shuffling:
the [Fisher--Yates
shuffle](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle). It
works on arrays, not linked lists, so we'll have to use Haskell's
arrays.

Haskell's arrays are a little funky: arrays are parameterized by two
things: the type of their index and the monad in which they'll be
used. We'll work with
[`IOArray`s](http://hackage.haskell.org/package/array-0.5.1.0/docs/Data-Array-IO.html). The
`IOArray` type represents arrays that can be used in the `IO`
monad. We'll interact with these arrays using the [`MArray`
interface](http://hackage.haskell.org/package/array-0.5.1.0/docs/Data-Array-MArray.html).

Let's take a brief look at `IOArray`. It has kind `* -> * -> *`. The
first type it needs is the type of its indices... we can just use
`Int` for that, but it's interesting that we can use any type in the
`Ix` class. The second type it needs is the type of its
contents. Shuffling won't care about that, so we'll end up working
with `IOArray Int a`.

As a warmup, write a function that takes a list and generates a
corresponding array. It's worth noting that the bounds that Haskell
uses in, e.g.,
[`newListArray`](http://hackage.haskell.org/package/array-0.5.1.0/docs/Data-Array-MArray.html#v:newListArray)
are *inclusive*, per
[`Data.Ix`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Ix.html).

> listToArray :: [a] -> IO (IOArray Int a)
> listToArray x = newListArray (0, (length x) -1) x

Okay: let's do it. Implement the Fisher--Yates shuffling algorithm that takes a given
array and shuffles it.

readArray :: IOArray Int a -> Int -> IOArray a

writeArray arr ((length arr) - 1) (readArray arr randomIndex)


> swap :: IOArray Int a -> Int -> Int -> IO ()
> swap arr ind1 ind2     = 
>   readArray arr ind1 >>= \val1 -> -- val1 = arr[ind1]
>   readArray arr ind2 >>= \val2 -> -- val2 = arr[ind2]
>   writeArray arr ind1 val2 >>     -- arr[ind1] = val2
>   writeArray arr ind2 val1        -- arr[ind2] = val1


> shuffle :: IOArray Int a -> IO ()
> shuffle arr = getBounds arr >>= 
>                \bs -> if snd bs == -1 then return ()
>                else
>                fisherYates (snd bs)
>                where  fisherYates 0 = pure ()
>                       fisherYates n = do
>                           rand 0 (n-1) >>= \x -> swap arr n x
>                           fisherYates (n-1)

Now use your array-based function `shuffle` to work on lists. Be sure
to test your code on a wide variety of inputs!

> fastShuffle :: [a] -> IO [a]
> fastShuffle l = listToArray l >>= \a -> Hw05.shuffle a >>
>                   getElems a 


My version of `fastShuffle` runs much more quickly than the naive one:
`sum <$> fastShuffle [0..10000]` runs in 0.04 seconds!

Finally, write a function that reads in a file and shuffles its
lines. You should ignore lines with no characters on them and your
final output should not end in a newline.

> shuffleFile :: FilePath -> IO String
> shuffleFile f = do
>   str <- readFile f
>   shuffleStr str

> shuffleStr str = do 
>   let lines = filter (\x -> x /= "") (splitOnNl str)
>   linesArray <- listToArray lines
>   Hw05.shuffle linesArray
>   l <- getElems linesArray
>   if l == [] then return ""
>   else return (foldl (\acc line -> acc ++ "\n" ++ line) (head l) (tail l))

> splitOnNl :: [Char] -> [[Char]]
> splitOnNl [] = []
> splitOnNl str = 
>   case elemIndex '\n' str of
>        Just i -> let (line, (nline:rest)) = Data.List.splitAt i str
>                  in  line : splitOnNl rest
>        Nothing -> [str]

Finally, this shuffling program is useful enough that we should make
it a command. To do so, you'll need to write a `main` function, which
should have type `IO ()`.

First and foremost, to compile your program, you'll run a command
like: `ghc Hw05.lhs -main-is Hw05.main -o shuffle`. This will compile
your program to an executable called `shuffle`. (The `-main-is` flag
is necessary because the default in Haskell is to have a file named
Main.hs.)

How should your program behave? It should take a single, optional
argument indicating a filename to shuffle. Without an argument (or
with an argument of `-`), it should read the content to be sorted from
standard input. If more than one argument is given, you should print a
"usage" message on standard error and exit with a non-zero exit code.

Either way, your program should read the lines of the input (file or
standard input), shuffle them, and then print out the shuffled input.

Look at the various `System` modules to find out how to parse
command-line arguments.

> main :: IO ()
> main = do
>   args <- getArgs
>   if length args > 1 then error "Usage error: too many args" else return () 
>   output <- if length args == 0 || head args == "-" 
>             then getContents >>= \str -> shuffleStr str 
>             else shuffleFile (head args)
>   putStrLn output

**Problem (2): monadic MapReduce**

[MapReduce](https://en.wikipedia.org/wiki/MapReduce) is a model for
data parallel computation. We'll look at data parallel programming
after Thanksgiving, but for now let's try to understand MapReduce as
is.

Our mappers will take an input of type `a` and produce a list of
key-value pairs, where keys have type `k` and values have type `v`.

> type Mapper a k v = a -> [(k,v)]

Our reducers take a key and list of values and produces a new (ideally
shorter!) list of values.

> type Reducer k v = k -> [v] -> [v]

The actual MapReduce implementaiton has three real phases: mapping,
shuffling, and reducing. Here's a simple implementation:

> mapReduce :: Ord k => Mapper a k v -> Reducer k v -> [a] -> [(k,[v])]
> mapReduce m r = reduce r . shuffleKeys . concatMap ((map listifyVal) . m)
>   where listifyVal (k,v) = (k,[v])
>         shuffleKeys = Map.fromListWith (++)
>         reduce r = Map.toList . Map.mapWithKey r

The canonical MapReduce example is word count. (Riveting, isn't it?)
Here's how to implement word count in MapReduce: given a list of
documents, a mapper breaks a given document into its consituent words,
where appearance of a word maps to 1. After shuffling, identical words
will be grouped together; the reducer sums up each token.

> wordCount = mapReduce countWords sumCounts
>  where countWords = map (\w -> (w,1)) . words
>        sumCounts _ cs = [sum cs]

I'm not sure why Google is so proud of what amounts to eight lines of
code and a slow way to count words.

Let's modify the MapReduce paradigm to allow for mappers and reducers
that generate monadic computations, like so:

> type MapperM m a k v = a -> m [(k,v)]
> type ReducerM m k v = k -> [v] -> m [v]

Note that a `MapperM` returns its list of key-value pairs inside of
some monad `m`; `ReducerM` is similar.

Adapt `mapReduce` above to define `mapReduceM`:

> listifyVal :: (k,v) -> (k, [v])
> listifyVal (k,v) = (k, [v])

> deMonadVals :: (Ord k, Monad m) => [(k, m [v])] -> [m (k,[v])] 
> deMonadVals lst = map (\(k,mv) -> mv >>= (\v -> return (k,v))) lst

> mapReduceM :: (Ord k, Monad m) => MapperM m a k v -> ReducerM m k v -> [a] -> m [(k,[v])]
> mapReduceM m r input =     do                            
>        listOfLists <- sequenceA (map m input)                          
>        let listifiedPairs = map listifyVal (foldr (++) [] listOfLists)
>        let reducedList = Map.fromListWith (++) listifiedPairs
>        let reducedMap = Map.mapWithKey r reducedList
>        let monadicValList = Map.toList reducedMap
>        sequenceA (deMonadVals monadicValList)

To test, here's an adaptation of the `wordCount` example above.

> wordCountM = mapReduceM countWords sumCounts
>  where countWords = return . map (\w -> (w,1)) . words
>        sumCounts w cs = do
>          when (length cs > 1) $ putStrLn $ "Lots of " ++ w ++ "!"
>          return [sum cs]

If you're having trouble, I recommend piecing apart the function and
relying on types to help you get through. If you're truly stuck, a
good place to start is refactoring `mapReduce` so it doesn't use
composition (`(.)`).

**Problem (3): QuickCheck**

We'll be using
[QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck.html)
to write some tests.

Write a QuickCheck property to check that `reverse` is *involutive*,
i.e., that reversing a reversed list yields the original list.

> prop_rev_involutive l = reverse (reverse l) == l

Write a QuickCheck property that checks the [Collatz
conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) for a
given number greater than 0.

> collatz :: (Num t, Integral a) => a -> t
> collatz 1 = 1
> collatz n = if n `mod` 2 == 0 then collatz (n `quot` 2) else collatz (3*n+1)

> prop_Collatz :: Integral a => a -> Bool 
> prop_Collatz n = if n <= 0 then True else collatz n == 1

Write a QuickCheck property that expresses the correctness of your
`fastShuffle` function. No need to go for *full* correctness of every
potential property of your shuffle, e.g., that it's pseudorandom. You
might need to write a type signature. Check out
`Test.QuickCheck.Monadic`.

> prop_fastShuffle_correct :: [Int] -> Property
> prop_fastShuffle_correct s = monadicIO $ do
>   shuffled <- run (fastShuffle s)
>   let lengthEq = length shuffled == length s
>   let subsetL = foldr (\x b -> (elem x shuffled) && b) True s
>   let subsetR = foldr (\x b -> (elem x s) && b) True shuffled
>   assert (lengthEq && subsetL && subsetR)

> data ArithExp =
>     Num Int
>   | Plus ArithExp ArithExp
>   | Times ArithExp ArithExp
>   | Neg ArithExp
>   deriving Show
>
> eval :: ArithExp -> Int
> eval (Num i) = i
> eval (Plus e1 e2) = eval e1 + eval e2
> eval (Times e1 e2) = eval e1 * eval e2
> eval (Neg e) = 0 - eval e

Write a generator that generates arbitrary `ArithExp`s. Use it to
define an `Arbitrary` instance for `ArithExp`... keep in mind that we
don't want to generate *giant* data structures, so you may need to keep track of sizes.

ae :: Arbitrary a => Gen (ArithExp)

> ae size = if size > 5 then Num <$> arbitrary
>           else oneof 
>           [Num <$> arbitrary, 
>           Plus <$> (ae (size+1)) <*> (ae (size+1)),
>           Times <$> (ae (size+1)) <*> (ae (size+1)),
>           Neg <$> (ae (size+1))]

> instance Arbitrary ArithExp where
>   arbitrary = ae 0

Write a test to ensure that `Plus e e` behaves the same as `Times 2 e`
for all expressions `e`.

> prop_double = forAll arbitrary (\rand_ae -> eval (Plus rand_ae rand_ae) == eval (Times (Num 2) rand_ae))
