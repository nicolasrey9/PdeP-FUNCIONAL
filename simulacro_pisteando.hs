data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgasteRuedas :: Float,
    desgasteChasis :: Float,
    velocidadMax :: Float,
    tiempoDeCarrera :: Float  
} deriving(Show, Eq)

-------PUNTO 1
autoFerrari :: Auto
autoFerrari = Auto "Ferrari" "F50" 0 0 65 0
autoLamborghini :: Auto
autoLamborghini = Auto "Lamborghini" "Diablo" 4 7 73 0
autoFiat :: Auto
autoFiat = Auto "Fiat" "600" 27 33 44 0

-----PUNTO 2
estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado unAuto = desgasteChasis unAuto < 40 && desgasteRuedas unAuto < 60
noDaMas :: Auto -> Bool
noDaMas unAuto = desgasteChasis unAuto > 80 || desgasteRuedas unAuto > 80

-----PUNTO 3
repararAuto :: Auto -> Auto
repararAuto = modificarDesgasteDeRuedas (\desgasteActual -> 0) . modificarDesgasteChasis (* (3/20))

-----PUNTO 4
--
modificarDesgasteDeRuedas :: (Float -> Float) -> Auto -> Auto
modificarDesgasteDeRuedas modificador unAuto = 
    unAuto{desgasteRuedas = modificador.desgasteRuedas $ unAuto}

modificarDesgasteChasis :: (Float -> Float) -> Auto -> Auto
modificarDesgasteChasis modificador unAuto = 
    unAuto{desgasteChasis = modificador.desgasteChasis $ unAuto}

modificarTiempoDeCarrera :: (Float -> Float) -> Auto -> Auto
modificarTiempoDeCarrera modificador unAuto = 
    unAuto{tiempoDeCarrera = modificador.tiempoDeCarrera $ unAuto}
--

type Tramo = Auto -> Auto

curva :: Float -> Float -> Tramo
curva angulo longitud unAuto = 
    modificarTiempoDeCarrera (+ longitud / ( velocidadMax unAuto / 2 )) . modificarDesgasteDeRuedas (+3*longitud/angulo) $ unAuto

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300
curvaTranca :: Tramo
curvaTranca = curva 110 550

recta :: Float -> Tramo
recta longitud unAuto =  
    modificarTiempoDeCarrera (+longitud / velocidadMax unAuto) . modificarDesgasteChasis (+ 0.01*longitud) $ unAuto

tramoRectoClassic :: Tramo
tramoRectoClassic = recta 750
tramito :: Tramo
tramito = recta 280

boxes :: Tramo
boxes unAuto
    | estaEnBuenEstado unAuto = unAuto
    | otherwise               = modificarTiempoDeCarrera (+10).repararAuto $ unAuto

--
sumarLaMitadDeTiempoAgregadoPorTramo :: Auto -> Auto -> Auto
sumarLaMitadDeTiempoAgregadoPorTramo autoAntes autoDespues =
    modificarTiempoDeCarrera(+ (tiempoDeCarrera autoDespues - tiempoDeCarrera autoDespues) *0.5) autoDespues
--

tramoMojado :: Tramo -> Tramo
tramoMojado unTramo unAuto = sumarLaMitadDeTiempoAgregadoPorTramo unAuto . unTramo $ unAuto

tramoConRipio :: Tramo -> Tramo
tramoConRipio unTramo = unTramo . unTramo

tramoConObstruccion :: Float -> Tramo -> Tramo
tramoConObstruccion metrosOcupados unTramo = 
    modificarDesgasteDeRuedas (+2*metrosOcupados) . unTramo

-----PUNTO 5

pasarPorTramo :: Tramo -> Auto -> Auto
pasarPorTramo unTramo unAuto
    | not.noDaMas $ unAuto = unTramo unAuto

-----PUNTO 6
type Pista = [Tramo]

superPista :: [Tramo]
superPista = [tramoRectoClassic, curvaTranca, tramoMojado tramito, 
    tramito, tramoConObstruccion 2 (curva 80 400), curva 115 650,
    recta 970, curvaPeligrosa, tramoConRipio tramito, recta 800 . boxes]


peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista = map (corraMientrasSeLaBanque unaPista)

corraMientrasSeLaBanque :: Pista -> Auto -> Auto
corraMientrasSeLaBanque pista auto = foldl aplicarSiSeLaBanca auto pista

aplicarSiSeLaBanca :: Auto -> Tramo -> Auto
aplicarSiSeLaBanca auto tramo
    | noDaMas auto = auto
    | otherwise = tramo auto


-----PUNTO 7
type Carrera = [Pista]
crearCarrera :: Pista -> Int -> Carrera
crearCarrera unaPista numeroDeVueltas = replicate numeroDeVueltas $ unaPista

tourBuenosAires :: Carrera
tourBuenosAires = crearCarrera superPista 20
{-
Hacer que una lista de autos juegue una carrera, teniendo los resultados 
parciales de cada vuelta, y la eliminación de los autos que no dan más en cada vuelta.
-}

jugarCarrera :: [Auto] -> Carrera -> [[Auto]]
jugarCarrera autos = map (flip peganLaVuelta autos)

jugarCarrera' :: [Auto] -> Carrera -> [[String]]
jugarCarrera' autos = map (funcionLoca autos)

funcionLoca :: [Auto] -> Pista -> [String]
funcionLoca






