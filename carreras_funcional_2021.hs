-- https://docs.google.com/document/d/1g2Gc81R62_xAIiGF0H663ypAz1vxJybr5LDo1sj9tAU/edit#heading=h.ielqgky5ojzp
--------------------------------------------------------------
------------------------ PUNTO 1 -----------------------------
--------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
import Data.List
data Auto = Auto{
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving(Show, Eq)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaMenorA10 auto1 auto2

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto = all (leGanaYNoLoTieneCerca auto)

leGanaYNoLoTieneCerca :: Auto -> Auto -> Bool
leGanaYNoLoTieneCerca auto1 auto2 = leGana auto1 auto2 && not (estaCerca auto1 auto2)

puesto :: Auto -> Carrera -> Int
puesto auto = (+1) . length . filter (flip leGana auto)

--
distanciaMenorA10 :: Auto -> Auto -> Bool
distanciaMenorA10 auto1 = (10>) . abs . (distancia auto1 -) . distancia

leGana :: Auto -> Auto -> Bool
leGana ganador perdedor = distancia ganador > distancia perdedor
--------------------------------------------------------------
------------------------ PUNTO 2 -----------------------------
--------------------------------------------------------------
autoCorraDurante :: Int -> Auto -> Auto
autoCorraDurante tiempo auto = auto{distancia = distancia auto + tiempo * velocidad auto}

type Modificador = Int -> Int

alterarVelocidad :: Modificador -> Auto -> Auto
alterarVelocidad modificador auto = auto{velocidad = max 0 (modificador . velocidad $ auto)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidadABajar = alterarVelocidad (cantidadABajar - )

--------------------------------------------------------------
------------------------ PUNTO 3 -----------------------------
--------------------------------------------------------------
afectarALosQueCumplen :: (Auto -> Bool) -> (Auto -> Auto) -> Carrera -> Carrera
afectarALosQueCumplen criterio efecto carrera = (map efecto . filter criterio) carrera ++ filter (not.criterio) carrera

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto auto = alterarVelocidadDeAutosSegun (estaCerca auto) 50

miguelitos :: Int -> PowerUp
miguelitos velABajar auto = alterarVelocidadDeAutosSegun (leGana auto) velABajar

jetPack :: Int -> PowerUp
jetPack tiempoDuracion auto = afectarALosQueCumplen (== auto) (usarJetPack tiempoDuracion)

--
alterarVelocidadDeAutosSegun :: (Auto -> Bool) -> Int -> Carrera -> Carrera
alterarVelocidadDeAutosSegun criterio velocidad = afectarALosQueCumplen criterio (bajarVelocidad velocidad)

usarJetPack :: Int -> Auto -> Auto
usarJetPack duracion = alterarVelocidad (`div`2) . autoCorraDurante duracion . alterarVelocidad (*2)


--------------------------------------------------------------
------------------------ PUNTO 4 -----------------------------
--------------------------------------------------------------
type Evento = Carrera -> Carrera
type TablaDePosiciones = [(Int, String)]
{-

simularCarrera :: Carrera -> [Evento] -> TablaDePosiciones
simularCarrera carrera = armarTabla . llegarAlEstadoFinalDeCarrera carrera

llegarAlEstadoFinalDeCarrera :: Carrera -> [Evento] -> Carrera
llegarAlEstadoFinalDeCarrera = foldl (flip ($))

armarTabla :: Carrera -> TablaDePosiciones -}
-----------------------------------------------------------



correnTodos :: Int -> Evento
correnTodos tiempo = map (autoCorraDurante tiempo)


usarPowerUp :: PowerUp -> String -> Evento
usarPowerUp powerUp colorX carrera = powerUp (encontrarAutoColor colorX carrera) carrera
encontrarAutoColor :: String -> Carrera -> Auto
encontrarAutoColor colorx = head . filter ((==colorx) . color)

--------------------------------------------------------------
------------------------ PUNTO 5 -----------------------------
--------------------------------------------------------------





