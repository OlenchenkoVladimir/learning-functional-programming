-- (\xy-> y x)((\x -> x x) w )((\z -> z(z))u) == (\x -> \y-> y x)((\x -> x x) w )((\z -> z(z))u)
-- (\xy-> y x)    =====>>>> ((\z -> z(z))u) ((\x -> x x) w )
-- ((\z -> z(z))u)=====>>>> u(u) (\x -> x x) w
-- u(u)ww 
-- Аппликативная ___ ___ не всегда ... нормальную форму выражения.
-- M = (\y.x)(EE), где E = \x.xx
let fac = (\f -> \n -> (if iszro n) 1 (mult n (f (pred n))))fac
