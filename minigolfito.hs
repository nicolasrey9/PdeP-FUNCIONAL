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

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 (precisionJugador habilidad * 2) 0

madera :: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad `div` 2) 5

hierros :: Int -> Palo
hierros n habilidad = UnTiro (fuerzaJugador habilidad * n) (precisionJugador habilidad `div` n) (max (n-3) 0)

palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

golpe :: Jugador -> Palo -> Tiro
golpe unaPersona unPalo = unPalo . habilidad $ unaPersona

type Obstaculo = Tiro -> Tiro

tunelConRampita :: Obstaculo
tunelConRampita unTiro
    | altura unTiro == 0 && precision unTiro > 90 = unTiro{velocidad= velocidad unTiro * 2, precision = 100}
    | otherwise = unTiro{velocidad=0, precision=0, altura=0}

laguna :: Int -> Obstaculo
laguna largoLaguna unTiro
    | velocidad unTiro > 80 && altura unTiro > 1 && altura unTiro < 5 = unTiro{altura = altura unTiro `div` largoLaguna}
    | otherwise = unTiro{velocidad=0, precision=0, altura=0}

