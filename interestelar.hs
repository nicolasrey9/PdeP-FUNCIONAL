data Planeta = UnPlaneta String Posicion (Int -> Int)

posicion :: Planeta -> Posicion
posicion (UnPlaneta _ p _) = p
tiempo :: Planeta -> Int -> Int
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX :: (a, b, c) -> a
coordX (x,_,_) = x
coordY :: (a, b, c) -> b
coordY (_,y,_) = y
coordZ :: (a, b, c) -> c
coordZ (_,_,z) = z

data Astronauta = UnAstronauta String Int Planeta

nombre :: Astronauta -> String
nombre (UnAstronauta n _ _) = n
edad :: Astronauta -> Int
edad (UnAstronauta _ e _) = e
planeta :: Astronauta -> Planeta
planeta (UnAstronauta _ _ p) = p

--------------
-- Punto 01 --
--------------
-- a --
distanciaEntrePlanetas :: Planeta -> Planeta -> Float
distanciaEntrePlanetas planeta1 planeta2 = 
    sqrt $ restaDeCoordenadas coordX ^ 2 + restaDeCoordenadas coordY ^2 + restaDeCoordenadas coordZ ^2
    where
        restaDeCoordenadas coordenadas = coordenadas (posicion planeta1) - coordenadas (posicion planeta2)


-- b --
tiempoDeViaje :: Float -> Planeta -> Planeta -> Int
tiempoDeViaje veocidadDeViaje unPlaneta = fromEnum.(/ veocidadDeViaje) . distanciaEntrePlanetas unPlaneta

--------------
-- Punto 02 --
--------------
pasarTiempo :: Int -> Astronauta -> Astronauta
pasarTiempo anios unAstronauta = aumentarEdadEn tiempoEnPlaneta unAstronauta
    where
        tiempoEnPlaneta = tiempo (planeta unAstronauta) anios

--------------
-- Punto 03 --
--------------
type Nave =  Planeta -> Planeta -> Int

naveVieja :: Int -> Nave
naveVieja tanquesDeOxigeno planeta1 planeta2
    | tanquesDeOxigeno < 6 = tiempoDeViaje 10 planeta1 planeta2
    | otherwise            = tiempoDeViaje 7 planeta1 planeta2

naveFuturista :: Nave
naveFuturista _ _ = 0

realizarUnViaje :: Nave -> Planeta -> Astronauta -> Astronauta
realizarUnViaje nave destino unAstronauta = cambiarPlanetaA destino . aumentarEdadEn (nave (planeta unAstronauta) destino) $ unAstronauta

cambiarPlanetaA :: Planeta -> Astronauta -> Astronauta
cambiarPlanetaA nuevoHogar unAstronauta = UnAstronauta (nombre unAstronauta) (edad unAstronauta) nuevoHogar

aumentarEdadEn :: Int -> Astronauta -> Astronauta
aumentarEdadEn anios unAstronauta = UnAstronauta (nombre unAstronauta) (edad unAstronauta + anios) (planeta unAstronauta)
--------------
-- Punto 04 --
--------------
-- a --
rescatar :: [Astronauta] -> Nave -> Astronauta -> [Astronauta]
rescatar rescatistas nave varado = 
    queViajenTodos nave planetaOrigen . (trasPasarTiempo varado :) . queViajenTodos nave (planeta varado) $ rescatistas
    where
        planetaOrigen = planeta $ head rescatistas
        trasPasarTiempo = pasarTiempo (nave planetaOrigen (planeta varado))

queViajenTodos ::  Nave -> Planeta -> [Astronauta] -> [Astronauta]
queViajenTodos nave destino = map (realizarUnViaje nave destino)


-- b --
puedenSerRescatados :: [Astronauta] ->Nave -> [Astronauta] -> [String]
puedenSerRescatados rescatistas nave = 
    map nombre . filter (puedeSerRescatado rescatistas nave)

puedeSerRescatado :: [Astronauta] ->Nave -> Astronauta -> Bool
puedeSerRescatado rescatistas nave = all ((<= 90). edad) . rescatar rescatistas nave

--------------
-- Punto 05 --
--------------

--------------
-- Punto 01 --
--------------

--------------
-- Punto 01 --
--------------