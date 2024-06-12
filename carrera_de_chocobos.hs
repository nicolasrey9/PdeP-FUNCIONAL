bosqueTenebroso :: Pista
bosqueTenebroso = [(100, f1), (50, f2), (120, f2), (200, f1), (80, f3)]
pantanoDelDestino :: Pista
pantanoDelDestino = [(40, f2), (90, (\(f,p,v)-> f + p + v)), (120, fuerza), (20, fuerza)]

f1 :: CorreccionDeVelocidad
f1 chocobo = velocidad chocobo * 2
f2 :: CorreccionDeVelocidad
f2 chocobo = velocidad chocobo + fuerza chocobo
f3 :: CorreccionDeVelocidad
f3 chocobo = velocidad chocobo / peso chocobo

type Pista = [Tramo]
type Tramo = (Distancia, CorreccionDeVelocidad)

type Distancia = Float
type CorreccionDeVelocidad = Chocobo -> Float

type Chocobo = (Float, Float, Float)
amarillo :: Chocobo
amarillo = (5, 3, 3)
negro :: Chocobo
negro = (4, 4, 4)
blanco :: Chocobo
blanco = (2, 3, 6)
rojo :: Chocobo
rojo = (3, 3, 4)

fuerza (f,_,_) = f
peso (_,p,_) = p
velocidad (_,_,v) = v

type Jinete = (String, Chocobo)
apocalipsis :: [Jinete]
apocalipsis = [("Leo", amarillo), ("Gise", blanco), ("Mati", negro), ("Alf",rojo)]

quickSort _ [] = []
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs
    ++ [x] ++ (quickSort criterio . filter (criterio x)) xs


--------------
-- Punto 01 --
--------------
mayorSegun :: Ord b => (a -> b) -> a -> a -> Bool
mayorSegun f a b = f a > f b

menorSegun :: Ord b => (a -> b) -> a -> a -> Bool
menorSegun f a b = f a < f b

--------------
-- Punto 02 --
--------------
--a--
tiempo :: Chocobo -> Tramo -> Float
tiempo unChocobo unTramo = (distancia unTramo /) . correccionDeVelocidad unTramo $ unChocobo
-- tiempo unChocobo unTramo = (distancia unTramo ) / (correccionDeVelocidad unTramo $ unChocobo)


correccionDeVelocidad :: Tramo -> CorreccionDeVelocidad
correccionDeVelocidad = snd
distancia :: Tramo -> Distancia
distancia = fst

--b--
tiempoTotal :: Chocobo -> Pista -> Float
tiempoTotal unChoco = sum . map (tiempo unChoco)

--------------
-- Punto 03 --
--------------
podioDeCarrera :: Pista -> [Jinete] -> [Jinete]
podioDeCarrera unaPista = quickSort (menorTiempoEn unaPista)

menorTiempoEn :: Pista -> Jinete -> Jinete -> Bool
menorTiempoEn unaPista = menorSegun (\(_,c) -> tiempoTotal c unaPista) 

--------------
-- Punto 04 --
--------------
--a--
elMejorDelTramo' :: Tramo -> [Jinete] -> String
elMejorDelTramo' tramo = fst . head . podioDeCarrera [tramo]

--b--
-- elMasWinner :: Pista -> [Jinete] -> String
-- elMasWinner pista jinetes = nombre . ganoMasTramos pista $ jinetes

-- ganodorDeTramo :: [Tramos] -> [Jinete] -> [Jinete]
-- ganoMasTramos (tramo : tramos) jinetes = elMejorDelTramo' tramo jinetes : ganoMasTramos tramos jinetes


elMasWinner :: Pista -> [Jinete] -> String
elMasWinner pista jinetes = fst . head . quickSort (mayorSegun (cantidadDeTramosGanadosPor pista jinetes)) $ jinetes
cantidadDeTramosGanadosPor :: Pista -> [Jinete] -> Jinete -> Int
cantidadDeTramosGanadosPor [] _ _ = 0
cantidadDeTramosGanadosPor (tramo : tramos) jinetes unJinete
    | elMejorDelTramo' tramo jinetes == fst unJinete = 1 + cantidadDeTramosGanadosPor tramos jinetes unJinete
    | otherwise                           = cantidadDeTramosGanadosPor tramos jinetes unJinete

-- quickSort _ [] = []
-- quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs
--     ++ [x] ++ (quickSort criterio . filter (criterio x)) xs


--------------
-- Punto 05 --
--------------
quienesPueden :: Tramo -> Float -> [Jinete] -> [Jinete]
quienesPueden unTramo tiempoLimite = filter ((<tiempoLimite) . flip tiempo unTramo . snd)


--------------
-- Punto 06 --
--------------
type Estadistica = (String, Int, Float)
estadisticas :: Pista -> [Jinete] -> [Estadistica]
estadisticas unaPista unosJinetes = map (generarStat unaPista unosJinetes) unosJinetes

generarStat :: Pista -> [Jinete] -> Jinete -> Estadistica
generarStat unaPista unosJinetes unJinete = 
    (fst unJinete, cantidadDeTramosGanadosPor unaPista unosJinetes unJinete, tiempoTotal (snd unJinete) unaPista)


--------------
-- Punto 07 --
--------------
fuePareja :: Pista -> [Jinete] -> Bool
fuePareja unaPista = fst . foldl (dosParejos unaPista) (True, 0) . podioDeCarrera unaPista


dosParejos :: Pista -> (Bool, Float) -> Jinete -> (Bool, Float)
dosParejos unaPista (unBul, valor) unJinete =
    (unBul && valor * 0.1 >  valor - taimJinete, taimJinete)
    where
        taimJinete = tiempoTotal (snd unJinete) unaPista


--------------
-- Punto 08 --
--------------













