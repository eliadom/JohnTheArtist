module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace

-- Problema 1

-- Entrem una sola comanda gran, i retornem una llista de comandes.
-- L'entrada està separada per :#: i la sortida per , i està en forma de []
-- per default, els elements de la llista estan separats per , per tant
--  només caldrar separar les comandes i col.locar-les en una llista.
separa :: Comanda -> [Comanda]
separa (x :#: xs) = (separa x) ++ (separa xs) -- l'unic element que hi passem es la llista de comandes
separa (Para) = [] -- en cas que sigui un Para, no fem res
separa (comanda) = [comanda] -- altrament retornem la mateixa comanda


-- Problema 2

-- Entrem una llista de comandes, i retornem una sola comanda gran.
-- L'entrada està separada per , en forma de [], i la sortida és una comanda
-- separada per :#:
ajunta :: [Comanda] -> Comanda
ajunta [] = (Para) -- Si ja no queden elements, afegim Para al final
ajunta (x:xs) = x :#: ajunta xs -- altrament, coloquem la nostra comanda a l'inici i seguim.


-- Problema 3

--isEqual :: Eq a => [a] -> [a] -> Bool
--isEqual [] [] = True
--isEqual [] _  = False
--isEqual _  [] = False
--isEqual (x:xs) (y:ys) = x == y && isEqual xs ys

prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent comanda1 comanda2 = (separa(comanda1) == separa(comanda2))

prop_split_join :: Comanda -> Bool
prop_split_join comanda = (ajunta(separa(comanda)) == comanda)

--prop_split :: Comanda -> Bool
--prop_split comanda = separa(comanda) 


-- Problema 4

copia :: Int -> Comanda -> Comanda
copia 1 comanda = comanda
copia n comanda = comanda :#: (copia (n-1) comanda)

-- Problema 5

pentagon :: Distancia -> Comanda
pentagon x = copia 5 ((Avança x) :#: (Gira 72.0))

-- Problema 6

poligon :: Distancia -> Int -> Angle -> Comanda
poligon dist n angle = copia n ((Avança dist) :#: (Gira angle))

prop_poligon_pentagon :: Comanda -> Bool
prop_poligon_pentagon comanda = (comanda == pentagon 50)

-- Problema 7

espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral dist 1 sumdist angle = Avança dist :#: Gira angle
espiral dist n sumdist angle = (Avança dist :#: Gira angle:#:espiral (dist+sumdist) (n-1) sumdist angle)

-- Problema 9

optimitza :: Comanda -> Comanda

optimitza comanda = optimitzaAUX(separa(comanda))

optimitzaAUX :: [Comanda] -> Comanda

--Avança 10 :#: Avança 15 :#: Gira -15 :#: Gira 15 :#: Gira 10

optimitzaAUX [] = Para
optimitzaAUX (x:xs) = com1 x (optimitzaAUX xs)
  where
    com1 com2 Para = com2
    com1 (Avança 0) (Avança 0) = Para
    com1 com2 (Avança 0) = com2
    com1 com2 (Avança 0 :#: com3) = com2 :#: com3
    com1 (Avança 0) com2 = com2
    com1 (Gira 0) (Gira 0) = Para
    com1 com2 (Gira 0) = com2
    com1 com2 (Gira 0 :#: com3) = com2 :#: com3
    com1 (Gira 0) com2 = com2
    com1 (Avança m) (Avança n) = if n + m /= 0 then Avança (n + m) else Para
    com1 (Avança m) (Avança n :#: com2) = if n + m /= 0 then Avança (n + m) :#: com2 else com2
    com1 (Gira m) (Gira n) = if n + m /= 0 then Gira (n + m) else Para
    com1 (Gira m) (Gira n :#: com2) = if n + m /= 0 then Gira (n + m) :#: com2 else com2
    com1 Para (Avança n) = Avança n
    com1 Para (Gira n) = Gira n    
    com1 (Avança m) com2 = Avança m :#: com2
    com1 (Gira m) com2 = Gira m :#: com2

--ajuntaAUX :: [Comanda] -> Comanda
--ajuntaAUX [x] = x -- Si ja no queden elements, afegim Para al final
--ajuntaAUX (x:xs) = x :#: ajuntaAUX xs -- altrament, coloquem la nostra comanda a l'inici i seguim.
   
-- Problema 10

triangle :: Int -> Comanda

mes = (Gira 90)
menys = (Gira (-90))
f 0 = (Avança 10)
f n = (f(n-1) :#: mes :#: f(n-1) :#: menys :#: f(n-1) :#: menys :#: f(n-1) :#: mes :#: f(n-1))

--triangle 1 = mes:#: f 1
triangle n = mes:#: f n

-- Problema 11

fulla :: Int -> Comanda

mesF = (Gira (-45))
menysF = (Gira 45)

gi 0 = (Avança 10)
gi n = (gi(n-1) :#: gi(n-1))


fi 0 = (Avança 10)
fi n = (gi (n-1) :#: (Branca (menysF :#: fi (n-1))) :#: (Branca (mesF :#: fi (n-1))) :#:(Branca (gi (n-1) :#: fi (n-1))))


fulla n = fi n

-- Problema 12

hilbert :: Int -> Comanda

fh n = Avança 10

l 0 = Para
l n = (mes :#: (r (n-1)) :#: (fh (n-1)) :#: menys :#: (l (n-1)) :#: (fh (n-1)) :#: (l (n-1)) :#: menys :#: (fh (n-1)) :#: (r (n-1)) :#: mes)

r 0 = Para
r n = (menys :#: (l (n-1)) :#: (fh (n-1)) :#: mes :#: (r (n-1)) :#: (fh (n-1)) :#: (r (n-1)) :#: mes :#: (fh (n-1)) :#: (l (n-1)) :#: menys)

--hilbert 1 = l 1
hilbert n = l n

-- Problema 13

fletxa :: Int -> Comanda

mesFletxa = (Gira (-60))
menysFletxa = (Gira 60)

gF 0 = (Avança 10)
gF n = (fF (n-1) :#: menysFletxa :#: gF (n-1) :#: menysFletxa :#: fF (n-1))


fF 0 = (Avança 10)
fF n = (gF (n-1) :#: mesFletxa :#: fF (n-1) :#: mesFletxa :#: gF (n-1))

fletxa n = fF n

-- Problema 14

branca :: Int -> Comanda

mesB = (Gira (-22.5))
menysB = (Gira 22.5)

gB 0 = (Avança 10)
gB n = (fB (n-1) :#: menysB :#: (Branca ((Branca (gB (n-1))) :#: mesB :#: gB (n-1))) :#: mesB :#: fB (n-1) :#: (Branca (mesB :#: fB (n-1) :#: gB (n-1))) :#: menysB :#: gB (n-1))

fB 0 = (Avança 10)
fB n = (fB (n-1) :#: fB(n-1))

branca n = gB n
