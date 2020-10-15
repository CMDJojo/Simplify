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
exprToPoly expression =
    fromList (map (collect simplified) (reverse [0..maxExponent]))
    where
        simplified = polyize expression
        maxExponent = maxExp simplified

        collect :: Expr -> Int -> Int
        collect (X a b) b'
            | b == b' = a
            | otherwise = 0
        collect (E a _ b) b' = collect a b' + collect b b'

        maxExp :: Expr -> Int
        maxExp (X _ b) = b
        maxExp (E exp1 _ exp2) = max (maxExp exp1) (maxExp exp2)

        polyize :: Expr -> Expr
        polyize x@(X a b) = x
        polyize (E a AddOp b) = E (polyize a) AddOp (polyize b)
        polyize (E a MulOp b) = multiply (polyize a) (polyize b)

        multiply :: Expr -> Expr -> Expr
        multiply (X a1 b1) (X a2 b2) = X (a1*a2) (b1+b2)
        multiply x@(X a1 b1) (E exp1 AddOp exp2) =
                E (multiply x exp1) AddOp (multiply x exp2)
        multiply e@(E _ AddOp _) x@(X _ _) = multiply x e
        multiply (E exp1 AddOp exp2) e@(E _ AddOp _) =
            E (multiply exp1 e) AddOp (multiply exp2 e)

prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly e x = eval x e == evalPoly x (exprToPoly e)

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

polyToExpr :: Poly -> Expr
polyToExpr = addAll . filterJunk . listToExpr . toList
    where
        addAll :: [Expr] -> Expr
        addAll [] = X 0 1
        addAll list = foldr1 folder list

        filterJunk :: [Expr] -> [Expr]
        filterJunk = filter (not . junkFilter)

        junkFilter :: Expr -> Bool
        junkFilter (X 0 _) = True
        junkFilter _ = False

        folder :: Expr -> Expr -> Expr
        folder a b = E a AddOp b

        listToExpr :: [Int] -> [Expr]
        listToExpr list = zipWith X (reverse list) [0..length list]


prop_polyToExpr :: Poly -> Int -> Bool
prop_polyToExpr p x = evalPoly x p == eval x (polyToExpr p)

prop_noJunk :: Expr -> Bool
prop_noJunk = not . isJunk . simplify

isJunk :: Expr -> Bool
isJunk (X _ _) = False
isJunk (E (X 0 _) MulOp _) = True
isJunk (E (X 1 0) MulOp _) = True
isJunk (E (X 0 _) AddOp _) = True
isJunk (E _ MulOp (X 0 _)) = True
isJunk (E _ MulOp (X 1 0)) = True
isJunk (E _ AddOp (X 0 _)) = True
isJunk (E a o b) = isJunk a || isJunk b