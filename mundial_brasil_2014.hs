martin :: Jugador
martin = CJugador "Martin" 26 0.0 50 35.0
juan :: Jugador
juan = CJugador "Juancho" 30 0.2 50 40.0
maxi :: Jugador
maxi = CJugador "Maxi Lopez" 27 0.4 68 30.0

jonathan :: Jugador
jonathan = CJugador "Chueco" 20 1.5 80 99.0
lean :: Jugador
lean = CJugador "Hacha" 23 0.01 50 35.0
brian :: Jugador
brian = CJugador "Panadero" 21 5 80 15.0

garcia :: Jugador
garcia = CJugador "Sargento" 30 1 80 13.0
messi :: Jugador
messi = CJugador "Pulga" 26 10 99 43.0
aguero :: Jugador
aguero = CJugador "Aguero" 24 5 90 5.0

equipo1 :: Equipo
equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre :: Equipo
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo :: Equipo
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])

data Jugador = CJugador{
    nombre :: String,
    edad :: Int,
    promedioGol :: Float,
    habilidad :: Int,
    cansancio :: Float
}deriving (Show, Eq)

type Equipo = (String, Char, [Jugador])

nombreEquipo :: Equipo -> String
nombreEquipo (nombre,_,_) = nombre

grupoEquipo :: Equipo -> Char
grupoEquipo (_,grupo,_) = grupo

jugadoresEquipo :: Equipo -> [Jugador]
jugadoresEquipo (_,_,jugadores) = jugadores

quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs
    ++ [x] ++  (quickSort criterio . filter (criterio x)) xs

--------------
-- Punto 01 --
--------------
quienesSonFigurita :: Equipo ->  [Jugador]
quienesSonFigurita = filter esFigura . jugadoresEquipo

esFigura :: Jugador -> Bool
esFigura unJugador = habilidad unJugador > 75 && promedioGol unJugador > 0


--------------
-- Punto 02 --
--------------
jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero :: Equipo -> Bool
tieneFarandulero = any esFarandulero . jugadoresEquipo

esFarandulero :: Jugador -> Bool
esFarandulero  = (`elem` jugadoresFaranduleros) . nombre

--------------
-- Punto 03 --
--------------
figuritasDificiles :: Char -> [Equipo] -> [String]
figuritasDificiles grupo = 
    map nombre . filter figuritaDificil . jugadoresDeEquipos . filter ((==grupo) . grupoEquipo)

jugadoresDeEquipos :: [Equipo] -> [Jugador]
jugadoresDeEquipos = concatMap jugadoresEquipo

figuritaDificil :: Jugador -> Bool
figuritaDificil unJugador = esJoven unJugador && esFigura unJugador && (not.esFarandulero) unJugador

esJoven :: Jugador -> Bool
esJoven unJugador = edad unJugador < 27

--------------
-- Punto 04 --
--------------
jugarPartido :: Equipo -> Equipo
jugarPartido (nombre, grupo, jugadores) = (nombre, grupo, cansarJugadoresPorPartido jugadores)

cansarJugadoresPorPartido :: [Jugador] -> [Jugador]
cansarJugadoresPorPartido = map cansarJugador

cansarJugador :: Jugador -> Jugador
cansarJugador unJugador
    | (not.esFarandulero) unJugador && esJoven unJugador && esFigura unJugador = modificarCansancio (const 50) unJugador
    | esJoven unJugador = modificarCansancio (+ cansancio unJugador * 0.1) unJugador
    | (not.esJoven) unJugador && esFigura unJugador = modificarCansancio (+20) unJugador
    | otherwise = modificarCansancio (*2) unJugador

modificarCansancio :: (Float -> Float) -> Jugador -> Jugador
modificarCansancio f unJugador = unJugador{cansancio=f.cansancio $ unJugador}


--------------
-- Punto 05 --
--------------
ganaPartido ::  Equipo -> Equipo -> Equipo
ganaPartido unEquipo otroEquipo
    | unEquipo `gana` otroEquipo = jugarPartido unEquipo
    | otherwise                  = jugarPartido otroEquipo

gana :: Equipo -> Equipo ->Bool
gana ganador perdedor = (sumarPromediosGol . plantelMenosCansado) ganador > (sumarPromediosGol . plantelMenosCansado) perdedor

menorCansancio :: Jugador -> Jugador -> Bool
menorCansancio menosCansado masCansado = cansancio menosCansado < cansancio masCansado

plantelMenosCansado :: Equipo -> [Jugador]
plantelMenosCansado = take 11 . quickSort menorCansancio.jugadoresEquipo

sumarPromediosGol :: [Jugador] -> Float
sumarPromediosGol = sum . map promedioGol

--------------
-- Punto 06 --
--------------
campeonDeTorneo :: [Equipo] -> Equipo
campeonDeTorneo = foldl1 ganaPartido

campeonDeTorneo' :: [Equipo] -> Equipo
campeonDeTorneo' = foldr1 ganaPartido

campeonDeTorneo'' :: [Equipo] -> Equipo
campeonDeTorneo'' [equipo] = equipo
campeonDeTorneo'' (equipo1 : equipo2 : equipos) = campeonDeTorneo'' (ganaPartido equipo1 equipo2: equipos)

Los días pasaron, las vuvuzelas se escucharon, una nueva Larissa Riquelme se hizo conocida, 
y el pulpo Paul volvió a acertar en los resultados. Después de un gran mundial se quiere saber 
quién va a ser elegido como el mejor de todos para entregarle el premio y ser reconocido en todo 
el mundo como “EL GROSO”. Para ello se ingresa una lista de equipos, y del equipo elegido ganador 
(el campeón), se quiere saber el nombre del primer jugador que cumpla la condición de ser figura 
(en todo equipo hay 1 por lo menos).

--------------
-- Punto 07 --
--------------
