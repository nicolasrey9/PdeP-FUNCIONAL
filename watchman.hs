algunosVigilantes :: [Vigilante]
algunosVigilantes =
    [ ("El Comediante", ["Fuerza"], 1942),
    ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963),
     ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964),
      ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962),
       ("Ozimandias", ["Inteligencia", "Más Inteligencia Aún"], 1968),
        ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939),
         ("Espectro de Seda", ["Lucha", "Sigilo"], 1940)]

type Vigilante = (String, [String], Int)

type Evento = [Vigilante] -> [Vigilante]

nombre :: Vigilante -> String
nombre (name,_,_) = name
habilidades :: Vigilante -> [String]
habilidades (_,hab,_) = hab
anio :: Vigilante -> Int
anio (_,_, year) = year


destruccionDeNY :: Evento
destruccionDeNY = muere "Rorschach" . seRetira "Dr Manhattan"

seRetira :: String -> [Vigilante] -> [Vigilante]
seRetira retirado = filter ((/= retirado).nombre)
muere :: String -> [Vigilante] -> [Vigilante]
muere = seRetira

guerraDeVietnam :: Evento
guerraDeVietnam unosVigilantes =
    (agregarlesCinismo. filter esAgenteDelGobierno) unosVigilantes ++ filter (not.esAgenteDelGobierno) unosVigilantes

esAgenteDelGobierno :: Vigilante -> Bool
esAgenteDelGobierno = (`elem` map fst agentesDelGobierno).nombre
agentesDelGobierno :: [(String, String)]
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]

agregarlesCinismo :: [Vigilante] -> [Vigilante]
agregarlesCinismo = map agregarCinismo

agregarCinismo :: Vigilante -> Vigilante
agregarCinismo unVigilante =
    (nombre unVigilante, "Cinismo": habilidades unVigilante, anio unVigilante)

accidenteDeLabo :: Int -> Evento
accidenteDeLabo unAnio losVigilantes =
    ("Doctor Manhattan", ["Manipulacion de Materia"], unAnio) : losVigilantes

actaDeKeene :: Evento
actaDeKeene unosVigilantes = filter (not.tieneSucesores unosVigilantes) unosVigilantes

tieneSucesores :: [Vigilante] -> Vigilante -> Bool
tieneSucesores listaTotal alguien = any (\x -> (nombre x == nombre alguien) && (anio x > anio alguien)) listaTotal

desarrolloDeUnaHistoria :: [Evento] -> [Vigilante] -> [Vigilante]
desarrolloDeUnaHistoria eventos vigilantes = foldl (flip ($)) vigilantes eventos


---------------------
--2) Grandes héroes--
---------------------
nombreDelSalvador :: [Vigilante] -> String
nombreDelSalvador = nombre . obtenerAlQueEsoTiene (comparacionSegun (length.habilidades)) . destruccionDeNY


elElegido :: [Vigilante] -> String
elElegido = head . habilidades . obtenerAlQueEsoTiene (comparacionSegun (contarPalabras.nombre)) . guerraDeVietnam

contarPalabras :: [Char] -> Int
contarPalabras = (+1) . length . filter (== ' ')


patriarca :: [Vigilante] -> Int
patriarca = (2024 -) . anio . obtenerAlQueEsoTiene (comparacionSegun ((*(-1)).anio)) . actaDeKeene




obtenerAlQueEsoTiene :: (Vigilante -> Vigilante -> Vigilante) -> [Vigilante] -> Vigilante
obtenerAlQueEsoTiene = foldl1

comparacionSegun :: Ord a => (Vigilante -> a) -> Vigilante -> Vigilante -> Vigilante
comparacionSegun f uno otro
    | f uno > f otro = uno
    | otherwise            = otro
