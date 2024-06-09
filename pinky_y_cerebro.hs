--------------
-- Punto 01 --
--------------
data Animal = Animal{
    especie :: String,
    coeficiente :: Int,
    capacidades :: [String]
} deriving (Show, Eq)

--------------
-- Punto 02 --
--------------
inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior unidades unAnimal = unAnimal{coeficiente = (+unidades).coeficiente $ unAnimal}

pinkificar :: Transformacion
pinkificar = modificarCapacidades (const [])

superpoderes :: Transformacion
superpoderes unAnimal
    | esDeEspecie "Elefante" unAnimal = darHabilidad "no tenerle miedo a los ratones" unAnimal
    | esDeEspecie "Raton" unAnimal && coeficiente unAnimal > 100 = darHabilidad "hablar" unAnimal
    | otherwise           = unAnimal

esDeEspecie :: String -> Animal -> Bool
esDeEspecie unaEspecie = (==unaEspecie).especie

darHabilidad :: String -> Animal -> Animal
darHabilidad habilidad = modificarCapacidades (habilidad :)

modificarCapacidades :: ([String] -> [String]) -> Animal -> Animal
modificarCapacidades f unAnimal = unAnimal{capacidades= f.capacidades $ unAnimal}

--------------
-- Punto 03 --
--------------
type Criterio = Animal -> Bool
antropomorfico :: Criterio
antropomorfico unAnimal =
    tieneLaHabilidad "hablar" unAnimal && coeficiente unAnimal > 60

tieneLaHabilidad :: String -> Animal -> Bool
tieneLaHabilidad habilidad = elem habilidad . capacidades

noTanCuerdo :: Criterio
noTanCuerdo = (>2) . length . filter pinkiesco . capacidades

pinkiesco :: String -> Bool
pinkiesco palabra = 
    ((== "hacer ") . take 6) palabra && esPalabraPinkinesca (drop 6 palabra)

esPalabraPinkinesca :: String -> Bool
esPalabraPinkinesca palabra = length palabra <= 4 && alMenos1Vocal palabra

alMenos1Vocal :: String -> Bool
alMenos1Vocal = any (`elem` "aeiouAEIOU")
--------------
-- Punto 04 --
--------------
type Transformacion = Animal -> Animal
data Experimento = Experimento{
    transformaciones :: [Transformacion],
    criterioDeExito :: Criterio
}

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso unExperimento = 
    criterioDeExito unExperimento. aplicarExperimento unExperimento

aplicarExperimento :: Experimento -> Animal -> Animal
aplicarExperimento unExperimento unAnimal = foldl (flip ($)) unAnimal (transformaciones unExperimento)

--------------
-- Punto 05 --
--------------
type Informe a = [String] -> Experimento -> [Animal] -> [a]
reporte1 :: Informe Int
reporte1 unasCapacidades = desarrollarLista coeficiente (any (`elem` unasCapacidades))

reporte2 :: Informe String
reporte2 unasCapacidades = desarrollarLista especie (all (`elem` unasCapacidades))

reporte3' :: Informe Int
reporte3' unasCapacidades = desarrollarLista (length.capacidades) (not . any (`elem` unasCapacidades))

desarrollarLista :: (Animal -> a) -> ([String] -> Bool) -> Experimento -> [Animal] -> [a]
desarrollarLista mapeo filtrado unExperimento =
        map mapeo .filter (filtrado . capacidades) . map (aplicarExperimento unExperimento)
    

