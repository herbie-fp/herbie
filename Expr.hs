{-# LANGUAGE GADTs, Rank2Types, NoMonomorphismRestriction #-}
import Data.List (maximumBy)
import Data.Function (on)
import GHC.Real  (Ratio((:%)))
import GHC.Float (float2Double)
import System.Random
    

-- Boolean expressions. Currently quite poor. 
data Bxpr a where
    Eq :: Expr a -> Expr a -> Bxpr a

-- Numerical expressions over constants of type a.  Everything is
-- totally standard except for Fun, which lets you lift a Haskell
-- function into the language. The string argument is used for pretty
-- printing. GADTs are used to make sure the types work out. The weird
-- typeclass precondition is to make sure that recursing on the Expr r
-- is possible for evalu and show. Quite hacky.
data Expr a where
    Const :: a -> Expr a
    Add   :: Expr a -> Expr a -> Expr a
    Sub   :: Expr a -> Expr a -> Expr a
    Mul   :: Expr a -> Expr a -> Expr a
    Div   :: Expr a -> Expr a -> Expr a
    Fun   :: (Eq r, Fractional r, Show r) => String -> (r -> a) -> Expr r -> Expr a
    If    :: Bxpr a -> Expr a -> Expr a -> Expr a

instance Show a => Show (Bxpr a) where
    show (Eq x y) = show x ++ " == " ++ show y

instance Show a => Show (Expr a) where
    show (Const c) = show c
    show (Add l r) = "(" ++ show l ++ ") + (" ++ show r ++ ")"
    show (Sub l r) = "(" ++ show l ++ ") - (" ++ show r ++ ")"
    show (Mul l r) = "(" ++ show l ++ ") * (" ++ show r ++ ")" 
    show (Div l r) = "(" ++ show l ++ ") / (" ++ show r ++ ")"
    show (Fun n _ e) = n ++ "(" ++ show e ++ ")"
    show (If b l r) = "if " ++ show b ++ " then " ++ show l ++ " else " ++ show r

--- Evaluators ---
bval :: (Fractional a, Eq a) => Bxpr a -> Bool
bval (Eq x y) = eval x == eval y

eval :: (Fractional a, Eq a) => Expr a -> a
eval (Const c)  = c
eval (Add l r)  = eval l + eval r
eval (Sub l r)  = eval l - eval r
eval (Mul l r)  = eval l * eval r
eval (Div l r)  = eval l / eval r

-- this case typechecks only because of the typeclass constraint on Fun
eval (Fun _ f x)  = f (eval x)

eval (If b l r) = if bval b then eval l else eval r

-- constrain the above generic evaluator to floats or doubles:
feval :: Expr Float -> Float
feval = eval
 
deval :: Expr Double -> Double
deval = eval

-- hacky embedding of the above language into Haskell
instance Num a => Num (Expr a) where
    (+) = Add
    (*) = Mul
    (-) = Sub
    abs = undefined
    signum = undefined
    fromInteger = Const . fromInteger

instance Fractional a => Fractional (Expr a) where
    (/) = Div
    fromRational (a GHC.Real.:% b) = fromInteger a / fromInteger a

(===) = Eq                                     

-- baby's first program
prog1 :: (Eq a, Show a, Floating a) => a -> Expr a
prog1 x = let x' = Const x  -- shorthand.
          in If (x' === 0) 
                1
                (((Fun "exp" exp x') - 1) / x')
          
-- baby's second program, now with better numerical properties!
prog2 x = let y = Fun "exp" exp (Const x)
          in If (y === 1)
                1
                ((y - 1) / (Fun "log" log y))

-- compare a program's value in float to that in double.  the strange
-- higher-rank type for the program allows it to be instantiated on
-- both floats and doubles. (note that the above evaluator will then
-- automatically use the requested precision.)
test :: (forall a. (Eq a, Floating a, Show a) => a -> Expr a) -> Float -> Double
test p1 x = (float2Double (feval (p1 x)) - deval (p1 (float2Double x))) / deval (p1 (float2Double x))
        
-- do n random tests on p using random source g
-- the distribution is a total hack, but it doesn't matter for now
tests :: RandomGen g => (forall a. (Eq a, Floating a, Show a) => a -> Expr a) ->
         Int -> g -> [(Float, Double)]
tests p n = filter (\(_,a) -> not (isNaN a || isInfinite a)) .
            map (\x -> (x, abs $ test p x)) . take n .
            map (subtract 1 . (1/)) . randoms 


maxNonInfiniteFloat :: RealFloat a => a -> a
maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

-- simple driver
main = do g <- newStdGen
          print . maximumBy (compare `on` snd) $ tests prog1 1000 g
          print . maximumBy (compare `on` snd) $ tests prog2 1000 g
