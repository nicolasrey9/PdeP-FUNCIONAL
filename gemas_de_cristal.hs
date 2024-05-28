import Text.Show.Functions

data Aspecto = UnAspecto{
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)

type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> [Aspecto] -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> [Aspecto] -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> [Aspecto] -> [Aspecto]
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : filter (not.mismoAspecto aspectoBuscado) situacion

--------------------------------------------------------------------------------------------
--1)

modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto cambioDeGrado unAspecto = unAspecto{grado = max 0 (cambioDeGrado.grado $ unAspecto)}

mejorSituacion :: Situacion -> Situacion -> Bool
mejorSituacion mejor peor = all (mejorQueAspectoEnPeor peor) mejor

mejorQueAspectoEnPeor :: Situacion -> Aspecto -> Bool
mejorQueAspectoEnPeor situacionPeor unAspecto =
    mejorAspecto unAspecto . buscarAspecto unAspecto $ situacionPeor

modificarSituacion :: (Float -> Float) -> String -> Situacion -> Situacion
modificarSituacion alteracion tipoAspectoBuscado situacion =
    flip reemplazarAspecto situacion . modificarAspecto alteracion. buscarAspectoDeTipo tipoAspectoBuscado $ situacion

--2)
data Gema = Gema {
    nombre :: String,
    fuerza :: Int,
    personalidad :: Personalidad
} deriving (Show)
type Personalidad = Situacion -> Situacion

vidente :: Personalidad
vidente =
    modificarSituacion disminuirALaMitad "incertidumbre" . bajarTensionEn 10
disminuirALaMitad :: Float -> Float
disminuirALaMitad x = x - (x/2)

relajada :: Float -> Personalidad
relajada nivelRelajacion =
    modificarSituacion (+ nivelRelajacion) "peligro" . bajarTensionEn 30

bajarTensionEn :: Float -> Situacion -> Situacion
bajarTensionEn valor = modificarSituacion (`subtract` valor) "tension"

gemaVidente :: Gema
gemaVidente = Gema "Garnet" 100 vidente
gemaDescuidada :: Gema
gemaDescuidada = Gema "Amatista" 65 (relajada 40)

--3)
gemaGana :: Gema -> Gema -> Situacion -> Bool
gemaGana ganadora perdedora situacion
    = masOIgualDeFuerte ganadora perdedora && mejorEnSituacion ganadora perdedora situacion

masOIgualDeFuerte :: Gema -> Gema -> Bool
masOIgualDeFuerte fuerte debil = fuerza fuerte >= fuerza debil

mejorEnSituacion :: Gema -> Gema -> Situacion -> Bool
mejorEnSituacion mejor peor situacion =
    mejorSituacion (personalidad mejor situacion) (personalidad peor situacion)

--4)
fusion :: Situacion -> Gema -> Gema -> Gema
fusion unaSituacion gema1 gema2 =
    Gema{
        nombre = fusionarNombres gema1 gema2,
        fuerza = fusionarFuerzas gema1 gema2 unaSituacion,
        personalidad = fusionarPersonalidades gema1 gema2
    }

fusionarNombres :: Gema -> Gema -> String
fusionarNombres gema1 gema2
    | nombre gema1 == nombre gema2 = nombre gema1
    | otherwise                    = nombre gema1 ++ nombre gema2

fusionarPersonalidades :: Gema -> Gema -> Personalidad
fusionarPersonalidades gema1 gema2 =
    personalidad gema2 . personalidad gema1 . bajarEn10TodosLosAspectos

bajarEn10TodosLosAspectos :: Situacion -> Situacion
bajarEn10TodosLosAspectos = map (modificarAspecto (`subtract` 10))

fusionarFuerzas :: Gema -> Gema -> Situacion -> Int
fusionarFuerzas gema1 gema2 unaSituacion
    | sonCompatibles gema1 gema2 unaSituacion = 10 * (fuerza gema1 + fuerza gema2)
    | otherwise                               = 7 * fuerza (gemaDominante gema1 gema2 unaSituacion)

sonCompatibles :: Gema -> Gema -> Situacion -> Bool
sonCompatibles gema1 gema2 unaSituacion =
    mejorSituacionFusionQueIndividual gema1 gema2 unaSituacion
    && mejorSituacionFusionQueIndividual gema2 gema1 unaSituacion

mejorSituacionFusionQueIndividual :: Gema -> Gema -> Situacion -> Bool
mejorSituacionFusionQueIndividual gema1 gema2 unaSituacion=
    mejorSituacion (fusionarPersonalidades gema1 gema2 unaSituacion) (personalidad gema1 unaSituacion)

gemaDominante :: Gema -> Gema -> Situacion -> Gema
gemaDominante gema1 gema2 unaSituacion
    | gemaGana gema1 gema2 unaSituacion = gema1
    | otherwise                         = gema2

--5)
fusionGrupal :: [Gema] -> Situacion -> Gema
fusionGrupal gemas unaSituacion = foldl1 (fusion unaSituacion) gemas

--6)

-- foo :: Eq b => c -> (c->b) -> (a -> [b]) -> a -> Bool
--foo x y z = any (== y x).z


-- Invocaciones de la función
--foo 5 (+7) [1..]  = any (==12) . [1,2,3,4 ,5,6,7,8,9,10,11,12,..] no tipa?
--foo 3 even (map (< 7)) =any (==even 3) . map( < 7) = any (==False) . map( < 7) = no funca?
--foo 3 even [1, 2, 3] =any (==even 3) . [1, 2, 3] = any (==False) . [1, 2, 3] = no funca?
--foo [1..] head (take 5) = [1.. ]-- any (==head [1..]) . (take 5) $ [1.. ] = any(==1)[1,2,3,4,5]=True

