--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                                                                            --
--  ?????? IKS - ???????? ??????? ??? ????????????? ??????-????????? ? ?????  --
--  IKS.                                                                      --
--                                                                            --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
module Basis_IKS
  (Lambda(..), i, k, s, b, c, w, y, free, transform)
where

--------------------------------------------------------------------------------
-- ??? ??? ????????????? ??????-?????. ??????-???? ???????????? ????? ??????????
-- (??????????? Var), ? ??????? ?????? ?? ??? ? ????????? ????; ?????????? ????
-- ??????-?????? ???? ? ????? (??????????? App); ? ????? ??????-??????????
-- (??????????? Lam), ? ??????? ?????????? ??????? ? ??????-??????. ?????????? ?
-- ?????????? ????? ???????? ?? ?????????? ?????.

data Lambda = Var String        -- ??????????
            | App Lambda Lambda -- ?????????? (??????????)
            | Lam String Lambda -- ??????????
  deriving Eq

--------------------------------------------------------------------------------
-- ?????????? ???? Lambda ??? ?????? Show ??? ????? ??????????? ?????????????
-- ??????-?????? ? ???? ?????. ? ???????? ??????? "??????" ???????????? ????
-- "\".
--
-- * ?????????? ?????????????? ?? ??????.
-- * ?????????? ?????????????? ? ???? ???? ??????-??????, ??????????? ? ??????
-- (??? ??? ??????, ???? ??????-????? ???????). ??? ???? ??????????, ???????????
-- ? ??????????, ????? ??????????? ? ??????.
-- * ?????????? ?????????????? ? ???? "\x.TERM", ??? x - ??? ??????????.

instance Show Lambda where
  show (Var x)   = x
  show (App x y) = case y of App _ _ -> showLam x ++ "(" ++ show y ++ ")"
                             _       -> showLam x ++ showLam y
    where showLam l@(Lam _ _) = "(" ++  show l  ++ ")"
          showLam x           = show x
  show (Lam x e)  = "\\" ++ x ++ "." ++ show e

--------------------------------------------------------------------------------
-- ??????????? ??????? ??? ????????????? ??????? ???????????? I, K, S.

i = Var "I"
k = Var "K"
s = Var "S"

--------------------------------------------------------------------------------
-- ???????? ??? ???????? ????, ???????? ?? ???????? ?????????? ????????? ?
-- ????????? ??????-?????.
--
-- ??????? ?????????:
--   x - ??????????, ??? ??????????? ?????????? ?????????.
--   l - ??????-????, ? ??????? ??????????? ??????????? ?????????? x.
--
-- ???????????? ????????:
--   True, ???? ?????????? x ???????? ? ??????-????? l.
--   False ? ????????? ??????.

free :: [Char] -> Lambda -> Bool
free x (Var y)     = x == y
free x (App e1 e2) = free x e1 || free x e2
free x (Lam y e)   = free x e

--------------------------------------------------------------------------------

transform :: Lambda -> Lambda
transform (Var x)                                = Var x
transform (App x y)                              = App (transform x)
                                                       (transform y)
transform (Lam x (Var y))     | x == y           = i
transform (Lam x e)           | (not . free x) e = App k (transform e)
transform (Lam x l@(Lam y e)) | free x e         = transform (Lam x (transform l))
transform (Lam x (App e1 e2))                    = App (App s (transform (Lam x e1)))
                                                       (transform (Lam x e2))

b = Lam "x" (Lam "y" (Lam "z" (App (Var "x") (App (Var "y") (Var "z")))))
c = Lam "x" (Lam "y" (Lam "z" (App (App (Var "x") (Var "z")) (Var "y"))))
w = Lam "x" (Lam "y" (App (App (Var "x") (Var "y")) (Var "y")))

y a = App a (y a)

--[ ????? ?????? ]--------------------------------------------------------------
-- transform b 
-- S(S(KS)(S(KK)(S(KS)(S(KK)I))))(K(S(S(KS)(S(KK)I))(KI)))
fac = (\f -> \n -> if n == 0 then 1 else (*) n (f (pred n)) ) fac
fac' n = if n == 0 then 1 else n * fac' (n - 1 )  
