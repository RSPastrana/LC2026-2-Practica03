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
negar (Syss p q) = negar (And (Impl p q) (Impl q p))

--Funcion auxiliar para fnn--
elimEquiv :: Prop -> Prop
elimEquiv (Var p)= Var p
elimEquiv (Cons True) = Cons True
elimEquiv (Cons False) = Cons False
elimEquiv (Not p) = Not (elimEquiv p)
elimEquiv (And p q) = And (elimEquiv p) (elimEquiv q)
elimEquiv (Or p q) = Or (elimEquiv p) (elimEquiv q)
elimEquiv (Impl p q) = Or (Not (elimEquiv p)) (elimEquiv q)
elimEquiv (Syss p q) = And (elimEquiv (Impl p q)) (elimEquiv (Impl q p))

distribuir :: Prop -> Prop
distribuir (Or p (And q r)) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or (And q r) p) = And (distribuir (Or p q)) (distribuir (Or p r))
distribuir (Or p q) = Or (distribuir p) (distribuir q)
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir  p = p

-- Funcion auxiliar para distribuir -- 
distribuirAux :: Prop -> Prop
distribuirAux p =
  let p' = distribuir p
  in if p' == p
  then p
  else distribuir p'

rs :: [Clausula] -> [Clausula]
rs[] = []
rs [x] = [x]
rs (x:(y:xs))
    | hayResolvente x y = noRep ((x:(y:xs)) ++ [resolucion x y] ++ rs (x:xs) ++ rs (y:xs))
    | otherwise = noRep ((x:(y:xs)) ++ rs (x:xs) ++ rs (y:xs))

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons b) = Cons b
fnn (Not (Var p)) = Not (Var p)
fnn (Not (Cons b)) = Cons (not b)
fnn (Not (Not p)) = fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not (Impl p q)) = fnn (Not (Or (Not p) q))
fnn (Not (Syss p q)) = fnn (Not (And (Impl p q) (Impl q p)))
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = fnn (Or (Not p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))

--Ejercicio 2
fnc :: Prop -> Prop
fnc p = distribuirAux (fnn p)

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (Var p) = [ [Var p] ]
clausulas (Not (Var p)) = [ [Not (Var p)] ] --porque en FNC las negaciones aparecen en frente de literales
clausulas (And p q) = (clausulas p ++ clausulas q)
clausulas (Or p q) = [litInOr (Or p q)]

--Funcion auxiliar para clausulas--
litInOr :: Prop -> [Literal]
litInOr (Not p) = [ Not p ]
litInOr (Var p) = [Var p]
litInOr (Cons True) = [Cons True]
litInOr (Cons False) = [Cons False]
litInOr (Or p q) = noRep((litInOr p) ++ (litInOr q))

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion [] [] = []
resolucion c1 [] = c1
resolucion [] c2 = c2
resolucion [Var p] [Var q]
    | (Var p) == negar (Var q) = []
    | otherwise = [Var p] ++ [Var q]

resolucion (l1:c1) (l2:c2)
    | negar l1 `elem` (l2:c2) = noRep (c1 ++ removerLit (negar l1) (l2:c2))
    | otherwise = noRep ([l1] ++ (noRep (resolucion (c1) (l2:c2))))

--Funcion auxiliar para resolucion--
removerLit :: Literal -> Clausula -> Clausula
removerLit l [] = []
removerLit l (x:xs)
    | x == l = xs
    | otherwise = x : removerLit l xs

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente [] [] = False
hayResolvente c1 [] = False
hayResolvente [] c2 = False
hayResolvente (l1:c1) (l2:c2)
    | negar l1 `elem` (l2:c2) = True
    | otherwise = hayResolvente (c1) (l2:c2) || hayResolvente (l1:c1) (c2)

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion p = satAux (clausulas (simplificarFNC (fnc p)))

-- verifica si existe la clausula vacia
hayVacia :: [Clausula] -> Bool
hayVacia [] = False
hayVacia (c:cs)
    | c == [] = True
    | otherwise = hayVacia cs

-- pertenece (sin usar elem)
pertenece :: Clausula -> [Clausula] -> Bool
pertenece _ [] = False
pertenece c (x:xs)
    | c == x = True
    | otherwise = pertenece c xs

-- union sin repetidos
union :: [Clausula] -> [Clausula] -> [Clausula]
union [] ys = ys
union (x:xs) ys
    | pertenece x ys = union xs ys
    | otherwise = x : union xs ys

-- igualdad de conjuntos
contenido :: [Clausula] -> [Clausula] -> Bool
contenido [] _ = True
contenido (x:xs) ys
    | pertenece x ys = contenido xs ys
    | otherwise = False

iguales :: [Clausula] -> [Clausula] -> Bool
iguales xs ys = contenido xs ys && contenido ys xs

-- generar resolventes validos (usa hayResolvente)
resolventes :: [Clausula] -> [Clausula]
resolventes [] = []
resolventes (c:cs) = noRep(resAux c cs ++ resolventes cs)

resAux :: Clausula -> [Clausula] -> [Clausula]
resAux _ [] = []
resAux c (x:xs)
    | hayResolvente c x =
        let r = resolucion c x
        in if r == [] then [[]] else r : resAux c xs
    | otherwise = resAux c xs

-- R(S)
rS :: [Clausula] -> [Clausula]
rS s = union s (resolventes s)

satAux :: [Clausula] -> Bool
satAux s
    | hayVacia s = False
    | hayVacia (resolventes s) = False   
    | iguales (s) (rS s) = True
    | otherwise = satAux (rS s)

simplificarFNC :: Prop -> Prop
simplificarFNC (And p q)
    | p == q = simplificarFNC p
    | otherwise = And (simplificarFNC p) (simplificarFNC q)
simplificarFNC (Or p q) = Or (simplificarFNC p) (simplificarFNC q)
simplificarFNC p = p
