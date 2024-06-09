import Data.Char (toUpper)
import Data.ByteString (sort)
--------------
-- Punto 01 --
--------------
--a--
data Persona = Persona{
    habilidadesPersona :: [String],
    esBuena :: Bool
} deriving (Show, Eq)
nico :: Persona
nico = Persona [] True

--b--
data PowerRanger = PowerRanger{
    color :: String,
    habilidadesRanger :: [String],
    nivel :: Int
}deriving (Show, Eq)
powerRangerNegro :: PowerRanger
powerRangerNegro = PowerRanger "Negro" [] 100

--------------
-- Punto 02 --
--------------
convertirEnPowerRanger :: String -> Persona -> PowerRanger
convertirEnPowerRanger color unaPersona = 
    PowerRanger color (potenciarHailidades.habilidadesPersona $ unaPersona) (nivelPelea.habilidadesPersona $ unaPersona)

potenciarHailidades :: [String] -> [String]
potenciarHailidades = map ((++ "super"). primeraLetraEnMayusqula)

primeraLetraEnMayusqula :: String -> String
primeraLetraEnMayusqula (letra:letras) = toUpper letra : letras

nivelPelea :: [String] -> Int
nivelPelea = sum . map length

--------------
-- Punto 03 --
--------------
type Equipo = [PowerRanger]
formarEquipoRanger :: [String] -> [Persona] -> Equipo
formarEquipoRanger colores = zipWith convertirEnPowerRanger colores . filter esBuena

--------------
-- Punto 04 --
--------------
--a--
findOrElse :: (a-> Bool) -> a -> [a] -> a
findOrElse condicion valor =  head.(++ [valor]). filter condicion

--b--
rangerLider :: Equipo -> PowerRanger
rangerLider = buscarLider esRojo

esRojo :: PowerRanger -> Bool
esRojo = (=="Rojo").color

--------------
-- Punto 05 --
--------------
--a--
maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy funcion lista = head $ filter ( (== (maximum . map funcion) lista) . funcion) lista

--b--
rangerMasPoderoso :: Equipo -> PowerRanger
rangerMasPoderoso = maximumBy nivel
--------------
-- Punto 06 --
--------------
rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso = (>5) . length . habilidadesRanger

--------------
-- Punto 07 --
--------------
decirAyInfinitamente :: String
decirAyInfinitamente = cycle "ay "
alfa5 :: PowerRanger
alfa5 = PowerRanger "Metalico" ["Reparar", decirAyInfinitamente] 0

--------------
-- Punto 08 --
--------------
data ChicaSuperpoderosa = ChicaSuperpoderosa{
    colorChica :: String,
    cantidadPelo :: Int
} deriving (Show, Eq)

chicaLider :: [ChicaSuperpoderosa] -> ChicaSuperpoderosa
chicaLider = buscarLider esLaRoja

esLaRoja :: ChicaSuperpoderosa -> Bool
esLaRoja = (=="Roja").colorChica

buscarLider :: (a->Bool) -> [a] -> a
buscarLider criterio equipo = findOrElse criterio (head equipo) equipo
