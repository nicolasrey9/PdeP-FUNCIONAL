-- https://docs.google.com/document/d/1g2Gc81R62_xAIiGF0H663ypAz1vxJybr5LDo1sj9tAU/edit#heading=h.ielqgky5ojzp
--------------------------------------------------------------
------------------------ PUNTO 1 -----------------------------
--------------------------------------------------------------
import Data.List
data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving(Show, Eq)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = coloresDistintos auto1 auto2 && distanciaMenorA10 auto1 auto2

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = vaPrimero auto carrera && noTieneANadieCerca auto carrera

puesto :: Auto -> Carrera -> Int
puesto auto = (+1) . length . filter (flip leGana auto)

--
coloresDistintos :: Auto -> Auto -> Bool
coloresDistintos auto1 = (/= color auto1) . color

distanciaMenorA10 :: Auto -> Auto -> Bool
distanciaMenorA10 auto1 = (10>) . abs . (distancia auto1 -) . distancia

noTieneANadieCerca :: Auto -> Carrera -> Bool
noTieneANadieCerca auto = not . any (estaCerca auto)

vaPrimero :: Auto -> Carrera -> Bool
vaPrimero auto = all (leGana auto)

leGana :: Auto -> Auto -> Bool
leGana ganador perdedor = distancia ganador > distancia perdedor
--------------------------------------------------------------
------------------------ PUNTO 2 -----------------------------
--------------------------------------------------------------
autoCorraDurante :: Int -> Auto -> Auto
autoCorraDurante tiempo auto = auto{distancia = distancia auto + tiempo * velocidad auto}

type Modificador = Int -> Int

alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad modificador auto = auto{velocidad = modificador (velocidad auto)}

bajarVelocidad :: Int -> Modificador
bajarVelocidad velocidadABajar velocidadActual
    | velocidadABajar >= velocidadActual = 0
    | otherwise                          = velocidadActual - velocidadABajar
--------------------------------------------------------------
------------------------ PUNTO 3 -----------------------------
--------------------------------------------------------------
afectarALosQueCumplen :: (Auto -> Bool) -> (Auto -> Auto) -> Carrera -> Carrera
afectarALosQueCumplen criterio efecto carrera = (map efecto . filter criterio) carrera ++ filter (not.criterio) carrera

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto auto = afectarALosQueCumplen (estaCerca auto) (alterarVelocidad (bajarVelocidad 50))

miguelitos :: Int -> PowerUp
miguelitos velABajar auto = afectarALosQueCumplen (leGana auto) (alterarVelocidad (bajarVelocidad velABajar))

jetPack :: Int -> PowerUp
jetPack tiempoDuracion auto = afectarALosQueCumplen (== auto) (usarJetPack tiempoDuracion)


--
usarJetPack :: Int -> Auto -> Auto
usarJetPack duracion = alterarVelocidad reducirAMitadVel . autoCorraDurante duracion . alterarVelocidad duplicarVel

duplicarVel :: Modificador
duplicarVel = (*2)

reducirAMitadVel :: Modificador
reducirAMitadVel = (`div`2)
--------------------------------------------------------------
------------------------ PUNTO 4 -----------------------------
--------------------------------------------------------------
type Evento = Carrera -> Carrera
type TablaDePosiciones = [(Int, String)]

simularCarrera :: Carrera -> [Evento] -> TablaDePosiciones
simularCarrera carrera = armarTabla . llegarAlEstadoFinalDeCarrera carrera

llegarAlEstadoFinalDeCarrera :: Carrera -> [Evento] -> Carrera
llegarAlEstadoFinalDeCarrera = foldl (flip ($))

armarTabla :: Carrera -> TablaDePosiciones
armarTabla = transformarATabla . ordenarCompetidores

-- resolucion dudosa
ordenarCompetidores :: Carrera -> [Auto]
ordenarCompetidores carrera = emparejarListas (reverse . sort . map distancia $ carrera) carrera

emparejarListas :: [Int] -> Carrera -> [Auto]
emparejarListas _  []  = []
emparejarListas _ [x]  = [x]
emparejarListas (x:xs) autosDesorden = emparejarElem x autosDesorden ++ emparejarListas xs autosDesorden

emparejarElem :: Int -> Carrera -> [Auto]
emparejarElem distanciaAEmparejar = take 1 . filter ((==distanciaAEmparejar) . distancia)

transformarATabla :: [Auto] -> [(Int, String)]
transformarATabla autosOrdenados = zip [1 .. length autosOrdenados] (map color autosOrdenados)
--



correnTodos :: Int -> Evento
correnTodos tiempo = map (autoCorraDurante tiempo)

-- type PowerUp = Auto -> Carrera -> Carrera
-- type Evento = Carrera -> Carrera
-- afectarALosQueCumplen :: (Auto -> Bool) -> (Auto -> Auto) -> Carrera -> Carrera
-- afectarALosQueCumplen criterio efecto carrera = (map efecto . filter criterio) carrera ++ filter (not.criterio) carrera

-- Desarrollar las siguientes funciones de modo que puedan usarse para generar los eventos que se dan en una carrera:
-- usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, 
-- encuentre el auto correspondiente dentro del estado actual de la carrera para usarlo y 
-- produzca los efectos esperados para ese power up.
usarPowerUp :: PowerUp -> String -> Evento
usarPowerUp powerUp colorX carrera = powerUp (encontrarAutoColor colorX carrera) carrera
encontrarAutoColor :: String -> Carrera -> Auto
encontrarAutoColor colorx = head . filter ((==colorx) . color)

--------------------------------------------------------------
------------------------ PUNTO 1 -----------------------------
--------------------------------------------------------------

--------------------------------------------------------------
------------------------ PUNTO 1 -----------------------------
--------------------------------------------------------------

--------------------------------------------------------------
------------------------ PUNTO 1 -----------------------------
--------------------------------------------------------------

--------------------------------------------------------------
------------------------ PUNTO 1 -----------------------------
--------------------------------------------------------------















