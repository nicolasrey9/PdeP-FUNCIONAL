-- https://docs.google.com/document/d/1LeWBI6pg_7uNFN_yzS2DVuVHvD0M6PTlG1yK0lCvQVE/edit
-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

---------------------------------------------------------------------------------------------- PUNTO 1 -------------------------------
-------------------------------------------------------------------
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 (precisionJugador habilidad * 2) 0

madera :: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad `div` 2) 5

hierros :: Int -> Palo
hierros n habilidad = UnTiro (fuerzaJugador habilidad * n) (precisionJugador habilidad `div` n) (max (n-3) 0)

todosLosPalos :: [Palo]
todosLosPalos = [putter, madera] ++ map hierros [1..10]

-------------------------------------------------------------------
--------------------------- PUNTO 2 -------------------------------
-------------------------------------------------------------------
golpe :: Jugador -> Palo -> Tiro
golpe unaPersona unPalo = unPalo . habilidad $ unaPersona

-------------------------------------------------------------------
--------------------------- PUNTO 3 -------------------------------
-------------------------------------------------------------------
data Obstaculo  = Obstaculo{
  puedeSuperarlo :: Tiro -> Bool,
  efectoTrasSuperarlo :: Tiro -> Tiro
}
tunel :: Obstaculo
tunel = Obstaculo puedeSuperarTunel superoTunel
puedeSuperarTunel :: Tiro -> Bool
puedeSuperarTunel = superaRestricciones [rasDelSuelo, (>90).precision]
superoTunel :: Tiro -> Tiro
superoTunel = precisionPasaA100 . modificarVelocidad (*2)

laguna :: Int -> Obstaculo
laguna largoLaguna = Obstaculo puedeSuperarLaguna (superoLaguna largoLaguna)
puedeSuperarLaguna :: Tiro -> Bool
puedeSuperarLaguna = superaRestricciones [between 1 5 . altura, (>80). velocidad]
superoLaguna :: Int -> Tiro -> Tiro
superoLaguna largoLaguna = modificarAltura (`div` largoLaguna)

hoyo :: Obstaculo
hoyo = Obstaculo puedeSuperarHoyo superoHoyo
puedeSuperarHoyo :: Tiro -> Bool
puedeSuperarHoyo = superaRestricciones [between 5 20 . velocidad, rasDelSuelo, (>95). precision]
superoHoyo :: Tiro -> Tiro
superoHoyo _ = tiroNulo

tiroNulo :: Tiro
tiroNulo = UnTiro 0 0 0

modificarVelocidad :: (Int -> Int) -> Tiro -> Tiro
modificarVelocidad modificador unTiro = unTiro{velocidad = modificador . velocidad $ unTiro}
modificarAltura :: (Int -> Int) -> Tiro -> Tiro
modificarAltura modificador unTiro = unTiro{altura = modificador . altura $ unTiro}

superaRestricciones :: [Tiro -> Bool] -> Tiro ->  Bool
superaRestricciones restricciones tiro = all ($ tiro) restricciones

rasDelSuelo :: Tiro -> Bool
rasDelSuelo = (==0) . altura

precisionPasaA100 :: Tiro -> Tiro
precisionPasaA100 unTiro = unTiro{precision = 100}
-------------------------------------------------------------------
--------------------------- PUNTO 4 -------------------------------
-------------------------------------------------------------------
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (puedeSuperarlo unObstaculo . golpe unJugador) todosLosPalos

obstaculosConsecutivosR :: [Obstaculo] -> Tiro -> Int
obstaculosConsecutivosR [] unTiro = 0
obstaculosConsecutivosR (obstaculo : obstaculos) unTiro
  | puedeSuperarlo obstaculo unTiro = 1 + obstaculosConsecutivosR obstaculos (efectoTrasSuperarlo obstaculo unTiro)
  | otherwise = 0

obstaculosConsecutivosNoR :: [Obstaculo] -> Tiro -> Int
obstaculosConsecutivosNoR obstaculos unTiro =
  length . takeWhile (\(obstaculo,tiroMod) -> puedeSuperarlo obstaculo tiroMod) . zip obstaculos
    . tirosSucesivos unTiro $ obstaculos

tirosSucesivos :: Tiro -> [Obstaculo] -> [Tiro]
tirosSucesivos unTiro obstaculos = foldl tiroSucesivo [unTiro] obstaculos

tiroSucesivo :: [Tiro] -> Obstaculo -> [Tiro]
tiroSucesivo unosTiros obstaculo = unosTiros ++ [(efectoTrasSuperarlo obstaculo . last) unosTiros]

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil player obstaculos =
  maximoSegun (obstaculosConsecutivosNoR obstaculos . golpe player) todosLosPalos
-------------------------------------------------------------------
--------------------------- PUNTO 5 -------------------------------
-------------------------------------------------------------------
type Padre = String

padresPerdedores :: [(Jugador, Puntos)] -> [Padre]
padresPerdedores = padresDe . ninosPerdedores

padresDe :: [(Jugador, Puntos)] -> [Padre]
padresDe = map (padre . fst)

ninosPerdedores :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
ninosPerdedores puntosPorJugador = filter (/= ninoGanador puntosPorJugador) puntosPorJugador
ninoGanador :: [(Jugador, Puntos)] -> (Jugador, Puntos)
ninoGanador = maximoSegun snd