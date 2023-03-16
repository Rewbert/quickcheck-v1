module QuickCheck
  -- testing functions
  ( quickCheck    -- :: prop -> IO ()
  , verboseCheck  -- :: prop -> IO ()
  , test          -- :: prop -> IO ()  -- = quickCheck
  
  , State(..)    -- :: *
  , check         -- :: Config -> prop -> IO ()
 
  -- property combinators
  , forAll        -- :: Gen a -> (a -> prop) -> prop
  , (==>)         -- :: Bool -> prop -> prop
  
  -- gathering test-case information
  , label         -- :: String         -> prop -> prop
  , collect       -- :: Show a => a    -> prop -> prop
  , classify      -- :: Bool -> String -> prop -> prop
  , trivial       -- :: Bool           -> prop -> prop
  
  -- generator combinators
  , Gen           -- :: * -> * ; Functor, Monad
  
  , elements      -- :: [a] -> Gen a
  , two           -- :: Gen a -> Gen (a,a)
  , three         -- :: Gen a -> Gen (a,a,a)
  , four          -- :: Gen a -> Gen (a,a,a,a)
  
  , sized         -- :: (Int -> Gen a) -> Gen a
  , resize        -- :: Int -> Gen a -> Gen a
  , choose        -- :: Random a => (a, a) -> Gen a
  , oneof         -- :: [Gen a] -> Gen a
  , frequency     -- :: [(Int, Gen a)] -> Gen a
  
  , vector        -- :: Arbitrary a => Int -> Gen [a]

  -- default generators
  , Arbitrary(..) -- :: class
  , rand          -- :: Gen StdGen
  , promote       -- :: (a -> Gen b) -> Gen (a -> b)
  , variant       -- :: Int -> Gen a -> Gen a

  -- testable
  , Testable(..)  -- :: class
  , Property      -- :: *
  )
 where

-- QuickCheck v.0.2
-- DRAFT implementation; last update 000104.
-- Koen Claessen, John Hughes.

import System.Random
import Data.Char
import Data.List( group, sort, intersperse )
import Control.Monad( liftM2, liftM3, liftM4 )

infixr 0 ==>
infix  1 `classify`

--------------------------------------------------------------------
-- Generator

newtype Gen a
  = Gen (Int -> StdGen -> a)

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

resize :: Int -> Gen a -> Gen a
resize n (Gen m) = Gen (\_ r -> m n r)

rand :: Gen StdGen
rand = Gen (\n r -> r)

promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> \a -> let Gen m = f a in m n r)

variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v+1)))
 where
  rands r0 = r1 : rands r2 where (r1, r2) = split r0

generate :: Int -> StdGen -> Gen a -> a
generate n rnd (Gen m) = m size rnd'
 where
  (size, rnd') = randomR (0, n) rnd

instance Functor Gen where
  fmap f m = m >>= return . f

instance Applicative Gen where
    pure x = Gen $ \n r -> x
    Gen f' <*> Gen x' = Gen $ \n r0 ->
        let (r1,r2) = split r0
            f = f' n r1
            x = x' n r2
        in f x

instance Monad Gen where
  return a    = Gen (\n r -> a)
  Gen m >>= k =
    Gen (\n r0 -> let (r1,r2) = split r0
                      Gen m'  = k (m n r1)
                   in m' n r2)

-- derived

choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | i <- [1..n] ]

oneof :: [Gen a] -> Gen a
oneof gens = elements gens >>= id

frequency :: [(Int, Gen a)] -> Gen a
frequency xs = choose (1, tot) >>= (`pick` xs)
 where
  tot = sum (map fst xs)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs

-- general monadic

two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

four :: Monad m => m a -> m (a, a, a, a)
four m = liftM4 (,,,) m m m m

--------------------------------------------------------------------
-- Arbitrary

class Arbitrary a where
  arbitrary   :: Gen a
  coarbitrary :: a -> Gen b -> Gen b

instance Arbitrary () where
  arbitrary     = return ()
  coarbitrary _ = variant 0

instance Arbitrary Bool where
  arbitrary     = elements [True, False]
  coarbitrary b = if b then variant 0 else variant 1

instance Arbitrary Char where
  arbitrary     = choose (32,255) >>= \n -> return (chr n)
  coarbitrary n = variant (ord n)

instance Arbitrary Int where
  arbitrary     = sized $ \n -> choose (-n,n)
  coarbitrary n = variant (if n >= 0 then 2*n else 2*(-n) + 1)

instance Arbitrary Integer where
  arbitrary     = sized $ \n -> choose (-toInteger n, toInteger n)
  coarbitrary n = variant (fromInteger (if n >= 0 then 2*n else 2*(-n) + 1))

instance Arbitrary Float where
  arbitrary     = liftM3 fraction arbitrary arbitrary arbitrary 
  coarbitrary x = coarbitrary (decodeFloat x)

instance Arbitrary Double where
  arbitrary     = liftM3 fraction arbitrary arbitrary arbitrary 
  coarbitrary x = coarbitrary (decodeFloat x)

fraction a b c = fromInteger a + (fromInteger b / (abs (fromInteger c) + 1))

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary          = liftM2 (,) arbitrary arbitrary
  coarbitrary (a, b) = coarbitrary a . coarbitrary b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c) where
  arbitrary             = liftM3 (,,) arbitrary arbitrary arbitrary
  coarbitrary (a, b, c) = coarbitrary a . coarbitrary b . coarbitrary c

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (a, b, c, d)
 where
  arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary
  coarbitrary (a, b, c, d) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d

instance Arbitrary a => Arbitrary [a] where
  arbitrary          = sized (\n -> choose (0,n) >>= vector)
  coarbitrary []     = variant 0
  coarbitrary (a:as) = coarbitrary a . variant 1 . coarbitrary as

instance (Arbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary         = promote (`coarbitrary` arbitrary)
  coarbitrary f gen = arbitrary >>= ((`coarbitrary` gen) . f)

--------------------------------------------------------------------
-- Testable

data Result
  = Result { ok :: Maybe Bool, stamp :: [String], arguments :: [String] }

nothing :: Result
nothing = Result{ ok = Nothing, stamp = [], arguments = [] }

newtype Property = Prop { unGen :: Gen Result }

result :: Result -> Property
result res = Prop (return res)

evaluate :: Testable a => a -> Gen Result
evaluate = unGen . property--gen where Prop gen = property a

class Testable a where
  property :: a -> Property

instance Testable () where
  property _ = result nothing

instance Testable Bool where
  property b = result (nothing{ ok = Just b })

instance Testable Result where
  property res = result res

instance Testable Property where
  property prop = prop

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
  do a   <- gen
     res <- evaluate (body a)
     return (argument a res)
 where
  argument a res = res{ arguments = show a : arguments res }

(==>) :: Testable a => Bool -> a -> Property
True  ==> a = property a
False ==> a = property ()

label :: Testable a => String -> a -> Property
label s a = Prop (add `fmap` evaluate a)
 where
  add res = res{ stamp = s : stamp res }

classify :: Testable a => Bool -> String -> a -> Property
classify True  name = label name
classify False _    = property

trivial :: Testable a => Bool -> a -> Property
trivial = (`classify` "trivial")

collect :: (Show a, Testable b) => a -> b -> Property
collect v = label (show v)

--------------------------------------------------------------------
-- Testing

data State = State
  { maxTest              :: Int
  , maxFail              :: Int
  , numSuccess           :: Int
  , numDiscarded         :: Int
  , numRecentlyDiscarded :: Int
  , seed                 :: StdGen
  , labels               :: [[String]]
  , size                 :: Int -> Int -> Int
  , every                :: Int -> [String] -> String
  , terminal             :: String -> IO ()
  }

data Args = Args
  { maxSuccess      :: Int
  , maxDiscardRatio :: Int
  , maxSize         :: Int
  , chatty          :: Bool
  }

stdArgs :: Args
stdArgs = Args
  { maxSuccess      = 100
  , maxDiscardRatio = 10
  , maxSize         = 100
  , chatty          = True
  }

quick :: Args -> IO State
quick a = do
  g <- newStdGen
  return $ State { maxTest              = maxSuccess a
                 , maxFail              = maxSuccess a * maxDiscardRatio a
                 , numSuccess           = 0
                 , numDiscarded         = 0
                 , numRecentlyDiscarded = 0
                 , seed    = g
                 , labels  = []
                 , size    = computeSize a
                 , every   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
                 , terminal = if chatty a then putStr else noOp
                 }
  where
    noOp _ = return ()

computeSize :: Args -> Int -> Int -> Int
computeSize a n d
  |    n `roundTo` (maxSize a) + (maxSize a) <= (maxSuccess a)
    || n >= (maxSuccess a)
    || (maxSuccess a) `mod` (maxSize a) == 0
    = (n `mod` (maxSize a) + d `div` 10) `min` (maxSize a)
  | otherwise
    = ((n `mod` (maxSize a)) * (maxSize a) `div` ((maxSuccess a) `mod` (maxSize a)) + d `div` 10) `min` (maxSize a)
  where
    roundTo :: Integral a => a -> a -> a
    roundTo n m = (n `div` m) * m

verbose :: IO State
verbose = do
  q <- quick stdArgs
  return $ q { every = \n args -> show n ++ ":\n" ++ unlines args }

test :: Testable a => a -> IO ()
test gen = do
  q <- quick stdArgs
  check q gen

quickCheck :: Testable a => a -> IO ()
quickCheck = quickCheckWith stdArgs

quickCheckWith :: Testable a => Args -> a -> IO ()
quickCheckWith a gen = do
  q <- quick a
  check q gen

verboseCheck :: Testable a => a -> IO ()
verboseCheck gen = do
  q <- quick stdArgs
  check q gen

check :: Testable a => State -> a -> IO ()
check st a = tests st (property a)

tests :: State -> Property -> IO () 
tests st f
  | numSuccess st   == maxTest st =
      done "OK, passed" (numSuccess st) (labels st)
  | numDiscarded st == maxFail st =
      done "Arguments exhausted after" (numSuccess st) (labels st)
  | otherwise               =
      do terminal st (every st (numSuccess st) (arguments result))
         case ok result of
           Nothing    -> tests (updateAfterFail st)    f
           Just True  -> tests (updateAfterSuccess st result) f
           Just False ->
             putStr ( "Falsifiable, after "
                   ++ show (numSuccess st)
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (size st (numSuccess st) (numRecentlyDiscarded st)) rnd2 (unGen f)
      (rnd1,rnd2) = split (seed st)
      updateAfterFail :: State -> State
      updateAfterFail st = st { numDiscarded         = numDiscarded st + 1
                              , numRecentlyDiscarded = numRecentlyDiscarded st + 1
                              , seed                 = rnd1
                              }
      updateAfterSuccess :: State -> Result -> State
      updateAfterSuccess st res = st { numSuccess           = numSuccess st + 1
                                     , numRecentlyDiscarded = 0
                                     , seed                 = rnd1
                                     , labels               = stamp res : labels st
                                     }

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps =
  do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"

--------------------------------------------------------------------
-- the end.
