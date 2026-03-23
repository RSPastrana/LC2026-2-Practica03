module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

noRep :: Eq a => [a] -> [a]
noRep [] = []
noRep (x:xs)
    | x `elem` xs = noRep xs
    | otherwise = x : noRep xs

negar :: Prop -> Prop
negar (Var p) = Not (Var p)
negar (Cons True) = Cons False
negar (Cons False) = Cons True
negar (Not p) = p
negar (And p q) = Or (negar p) (negar q)
negar (Or p q) = And (negar p) (negar q)
negar (Impl p q) = And p (negar q)
negar (Syss p q) = negar (And (Impl p q)(Impl q p))

distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And q r) p) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir  p = p

rs :: [Clausula] -> [Clausula]
rs[] = []
rs [x] = [x]
rs (x:(y:xs))
    | hayResolvente x y = noRep((x:(y:xs)) ++ [resolucion x y] ++ rs (x:xs) ++ rs (y:xs))
    | otherwise = noRep((x:(y:xs)) ++ rs (x:xs) ++ rs (y:xs))

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn = undefined


--Ejercicio 2
fnc :: Prop -> Prop
fnc = undefined

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas = undefined

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion = undefined

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente = undefined

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion = undefined