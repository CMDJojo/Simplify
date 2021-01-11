module Simplify
    where

import Test.QuickCheck
import Poly

data BinOp = AddOp | MulOp
--                              in form: ax^b (X a b)
data Expr = E Expr BinOp Expr | X Int Int

prop_Expr :: Expr -> Bool
prop_Expr (X _ b) = b >= 0
prop_Expr (E x _ y) = prop_Expr x && prop_Expr y

instance Show Expr where
    show (E x o y) = "(" ++ show x ++ getOp o ++ show y ++ ")"
        where
            getOp :: BinOp -> String
            getOp AddOp = " + "
            getOp MulOp = " * "
    show (X 0 _) = "0"
    show (X a b) = show a ++ termStr b
        where
            termStr :: Int -> String
            termStr 0 = ""
            termStr 1 = "x"
            termStr b = "x^" ++ show b

instance Arbitrary Expr where
    arbitrary = do
        exprGen 0 4
        where
            exprGen :: Int -> Int -> Gen Expr
            exprGen n g = do
                num <- elements [1..3]
                if n < g && num < 3 then do
                    op <- elements [AddOp, MulOp]
                    x <- exprGen (n+1) g
                    y <- exprGen (n+1) g
                    return (E x op y)
                else do
                    coefficient <- elements [-9..9]
                    exponent <- elements [0..9]
                    return (X coefficient exponent)

eval :: Int -> Expr -> Int
eval x (X a b) = a * (x^b)
eval x (E a MulOp b) = eval x a * eval x b
eval x (E a AddOp b) = eval x a + eval x b

exprToPoly :: Expr -> Poly
exprToPoly (E e1 AddOp e2) = exprToPoly e1 + exprToPoly e2
exprToPoly (E e1 MulOp e2) = exprToPoly e1 * exprToPoly e2
exprToPoly (X a b) = fromList (a : replicate b 0)

prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly e x = eval x e == evalPoly x (exprToPoly e)

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

polyToExpr :: Poly -> Expr
polyToExpr = addAll . filter junkFilter . listToExpr . toList
    where
        addAll :: [Expr] -> Expr
        addAll [] = X 0 1
        addAll list = foldr1 (\x y -> E x AddOp y) list

        junkFilter :: Expr -> Bool
        junkFilter (X 0 _) = False
        junkFilter _ = True

        listToExpr :: [Int] -> [Expr]
        listToExpr list = zipWith X list (reverse [0..length list - 1])

prop_polyToExpr :: Poly -> Int -> Bool
prop_polyToExpr p x = evalPoly x p == eval x (polyToExpr p)

-- If a simplified expression is evaluated to "0", this will count as junk
-- according to the junk filter, but isn't actually junk, so we added a
-- special case for that
prop_noJunk :: Expr -> Bool
prop_noJunk = not . isJunk . simplify

isJunk :: Expr -> Bool
isJunk (X _ _) = False
isJunk (E (X 0 _) _ _) = True -- we cannot check for (X 0 _) by itself since
-- an expression with only (X 0 _ is not junk)
isJunk (E (X 1 0) MulOp _) = True
isJunk (E _ _ (X 0 _)) = True -- can comment out
isJunk (E _ MulOp (X 1 0)) = True
isJunk (E a o b) = isJunk a || isJunk b