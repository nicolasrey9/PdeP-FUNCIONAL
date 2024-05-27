-- https://docs.google.com/document/d/1l9UjDqVhLdeiON6rtXf7EwGU5JZvN2TWu5AJQzVmSwE/edit
-------------------------------------------------------
-------------------PUNTO 1-----------------------------
-------------------------------------------------------
data Pais = Pais{
    ingresoPerCapita    :: Int,
    activosSectorPublico :: Int,
    activosSectorPrivado :: Int,
    recursos             :: [String],
    deuda               :: Float
} deriving (Show, Eq)

namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

-------------------------------------------------------
-------------------PUNTO 2-----------------------------
-------------------------------------------------------

type Estrategia = Pais -> Pais
type Receta = [Estrategia]

prestarMillones :: Float -> Estrategia
prestarMillones cantidadMillones = prestarleAPais (cantidadMillones * 1.5)

reducirPuestosDelSectorPublico :: Int -> Estrategia
reducirPuestosDelSectorPublico puestosDeTrabajo
    | puestosDeTrabajo > 100   = disminuirIngresosPerCapitaYReducirActivos 20 puestosDeTrabajo
    | otherwise   = disminuirIngresosPerCapitaYReducirActivos 15 puestosDeTrabajo

darLaExplotacion :: String -> Estrategia
darLaExplotacion unRecurso = perderRecurso unRecurso . reducirDeudaEn 2

establecerBlindaje :: Estrategia
establecerBlindaje unPais =
    reducirPuestosDelSectorPublico 500  . prestarleAPais (pbi unPais / 2) $ unPais

----------------
disminuirIngresosPerCapitaYReducirActivos :: Int -> Int -> Pais -> Pais
disminuirIngresosPerCapitaYReducirActivos porcentaje puestosDeTrabajo =
    disminuirIngresoPerCapita porcentaje . reducirActivosSectorPublico puestosDeTrabajo

disminuirIngresoPerCapita :: Int -> Pais -> Pais
disminuirIngresoPerCapita porcentaje unPais =
    unPais{ingresoPerCapita = ingresoPerCapita unPais - ingresoPerCapita unPais * porcentaje `div` 100}

reducirActivosSectorPublico :: Int -> Pais -> Pais
reducirActivosSectorPublico aReducir unPais =
    unPais{activosSectorPublico = activosSectorPublico unPais - aReducir}

perderRecurso :: String -> Pais -> Pais
perderRecurso unRecurso unPais =
    unPais{recursos = filter (/= unRecurso) (recursos unPais)}

reducirDeudaEn :: Float -> Pais -> Pais
reducirDeudaEn deudaAReducir = modificarDeuda (`subtract` deudaAReducir)

prestarleAPais :: Float -> Pais -> Pais
prestarleAPais cantidadMillones = modificarDeuda (+cantidadMillones)

modificarDeuda :: (Float -> Float) -> Pais -> Pais
modificarDeuda operacionConValor unPais =
    unPais{deuda = operacionConValor . deuda $ unPais}

pbi :: Pais -> Float
pbi unPais = fromIntegral (ingresoPerCapita unPais) * fromIntegral (poblacionActiva unPais)

poblacionActiva :: Pais -> Int
poblacionActiva (Pais _ activosPublicos activosPrivados _ _) = activosPublicos + activosPrivados

-------------------------------------------------------
-------------------PUNTO 3-----------------------------
-------------------------------------------------------
prestar200YdarMineria :: Receta
prestar200YdarMineria= [prestarMillones 200, darLaExplotacion "Mineria"]

aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta = foldl (flip ($))

aplicarLaRecetaDelPunto3AlPaisNamibia = aplicarReceta namibia prestar200YdarMineria

-------------------------------------------------------
-------------------PUNTO 4-----------------------------
-------------------------------------------------------
quienesPuedenZafar :: [Pais] -> [Pais]
quienesPuedenZafar = filter (elem "Petroleo" . recursos)

totalDeudaAFavor :: [Pais] -> Float
totalDeudaAFavor = sum . map deuda

-------------------------------------------------------
-------------------PUNTO 5-----------------------------
-------------------------------------------------------
ordenadaDePeorAMejor :: Pais -> [Receta] -> Bool
ordenadaDePeorAMejor unPais = estaOrdenadaDeMenorAMayor . map (pbi . aplicarReceta unPais)

estaOrdenadaDeMenorAMayor :: [Float] -> Bool
estaOrdenadaDeMenorAMayor [] = True
estaOrdenadaDeMenorAMayor [x] = True
estaOrdenadaDeMenorAMayor (x:y:xs) =
    x < y && estaOrdenadaDeMenorAMayor (y:xs)

-------------------------------------------------------
-------------------PUNTO 6-----------------------------
-------------------------------------------------------
china :: Pais
china = Pais 600 200 100 recursosNaturalesInfinitos 0
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos
-- No termina de evaluar, porque .....

-- La evalua ya que no toca la lista




















