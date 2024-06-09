import System.Win32 (xBUTTON1)
-- PRIMERA PARTE-------
-----------------------
-------PUNTO 1---------
-----------------------
data Guantelete = Guantelete{
    material :: String,
    gemas :: [Gema]
}
data Personaje = Personaje{
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving(Show, Eq)
type Universo = [Personaje]

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo 
    | puedeUsarse guantelete =  reducirMitad universo 
    | otherwise = universo

puedeUsarse ::Guantelete -> Bool
puedeUsarse guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo 
-----------------------
-------PUNTO 2---------
-----------------------
aptoParaPendex :: Universo -> Bool
aptoParaPendex = any personajeConMenosDe45

personajeConMenosDe45 :: Personaje -> Bool
personajeConMenosDe45 = (<45) . edad

energiaTotalDeUniverso :: Universo -> Int
energiaTotalDeUniverso = sum . map energia . filter tieneMasDeUnaHabilidad

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad = (>1) . length . habilidades
-----SEGUNDA PARTE-----
-----------------------
-------PUNTO 3---------
-----------------------
type Gema = Personaje -> Personaje

mente :: Int -> Gema
mente = reducirEnergiaEn

alma :: String -> Gema
alma habilidad = reducirEnergiaEn 10 . borrarHablilidad habilidad

espacio :: String -> Gema
espacio = enviarAlPlaneta

poder :: Gema
poder unPersonaje =
    quitarHabilidadesSiTieneMenosDe3 . reducirEnergiaEn (energia unPersonaje) $ unPersonaje

tiempo :: Gema
tiempo = reducirEnergiaEn 50 . reducirEdadALaMitad

gemaLoca :: Gema -> Gema
gemaLoca unaGema = unaGema . unaGema

reducirEnergiaEn :: Int -> Personaje -> Personaje
reducirEnergiaEn valor unPersonaje =
    unPersonaje{energia = max 0 (energia unPersonaje - valor)}

borrarHablilidad :: String -> Personaje -> Personaje
borrarHablilidad habilidad unPersonaje =
     unPersonaje{habilidades = filter (/= habilidad) . habilidades $ unPersonaje}

enviarAlPlaneta :: String -> Personaje -> Personaje
enviarAlPlaneta planetaAEnviar unPersonaje = unPersonaje{planeta = planetaAEnviar}

quitarHabilidadesSiTieneMenosDe3 :: Personaje -> Personaje
quitarHabilidadesSiTieneMenosDe3 unPersonaje
    | (<3) . length . habilidades $ unPersonaje = quitarHabilidades unPersonaje
    | otherwise = unPersonaje

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades unPersonaje = unPersonaje{habilidades=[]}

reducirEdadALaMitad :: Personaje -> Personaje
reducirEdadALaMitad unPersonaje = unPersonaje{edad = max 18 (edad unPersonaje `div` 2)}
-----------------------
-------PUNTO 4---------
-----------------------
guanteleteEjemplo :: Guantelete
guanteleteEjemplo = Guantelete {
    material = "Goma",
    gemas = [tiempo, alma "usar Mjolnir", gemaLoca (alma "programación en Haskell")]
}
-----------------------
-------PUNTO 5---------
-----------------------
utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldl (flip($)) enemigo gemas
-----------------------
-------PUNTO 6---------
-----------------------
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa (Guantelete _ [unaSolaGema]) _ = unaSolaGema
gemaMasPoderosa (Guantelete _ unasGemas) unPJ = gemaQueBajaMasLaEnergia unasGemas unPJ

gemaQueBajaMasLaEnergia :: [Gema] -> Personaje -> Gema
gemaQueBajaMasLaEnergia [x] unPersonaje = x
gemaQueBajaMasLaEnergia (x:y:xs) unPersonaje
    | energia (x unPersonaje) > energia (y unPersonaje) = gemaQueBajaMasLaEnergia (y:xs) unPersonaje
    | otherwise = gemaQueBajaMasLaEnergia (x:xs) unPersonaje

-----------------------
-------PUNTO 1---------
-----------------------















