data Turista = Turista{
    cansancio :: Int,
    stress :: Int,
    estaViajandoSolo :: Bool,
    idiomasQueHabla :: [Idioma]
}deriving (Show, Eq)
type Idioma = String
type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista
    | estaViajandoSolo turista = modificarCansancioEn (subtract 5) turista
    | otherwise = modificarStressEn (subtract 1) turista

apreciarElementoDelPaisaje :: String -> Excursion
apreciarElementoDelPaisaje paisaje = modificarStressEn (subtract $ length paisaje)

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma idioma = continueAcompanado . aprendaIdioma idioma

modificarStressEn :: (Int -> Int) -> Turista -> Turista
modificarStressEn operacion unTurista = unTurista{stress = operacion . stress $ unTurista}
modificarCansancioEn :: (Int -> Int) -> Turista -> Turista
modificarCansancioEn operacion unTurista = unTurista{cansancio = operacion . cansancio $ unTurista}
aprendaIdioma :: Idioma -> Turista -> Turista
aprendaIdioma idioma unTurista = unTurista{idiomasQueHabla = idioma: idiomasQueHabla unTurista}
continueAcompanado :: Turista -> Turista
continueAcompanado unTurista = unTurista{estaViajandoSolo = False}

caminarCiertosMinutos :: Int -> Excursion
caminarCiertosMinutos tiempo =
    modificarCansancioEn (+ intensidadCaminata tiempo) . modificarStressEn (subtract $ intensidadCaminata tiempo)
intensidadCaminata :: Int -> Int
intensidadCaminata tiempo = tiempo `div` 4

data Marea = Fuerte | Moderada | Tranquila

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte = modificarCansancioEn (+10) . modificarStressEn (+5)
paseoEnBarco Moderada = id
paseoEnBarco Tranquila =
    salirAHablarUnIdioma "Aleman" . apreciarElementoDelPaisaje "mar" . caminarCiertosMinutos 10

ana :: Turista
ana = Turista{
    estaViajandoSolo = False,
    cansancio = 0,
    stress = 21,
    idiomasQueHabla = ["Espanol"]
}
betoYCathi :: (Turista, Turista)
betoYCathi = (Turista 15 15 True [], Turista 15 15 True ["Catalan"])

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista =
    modificarStressEn (subtract $ ((* div 10 100) . stress) unTurista) . unaExcursion $ unTurista


deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista -> Int
deltaExcursionSegun :: Indice -> Excursion -> Turista -> Int
deltaExcursionSegun indice excursion turista = deltaSegun indice (excursion turista) turista

excursionEducativa :: Excursion -> Turista -> Bool
excursionEducativa unaExcursion
    = (>0) . deltaExcursionSegun (length . idiomasQueHabla) unaExcursion

excursionDesestresante :: Excursion -> Turista -> Bool
excursionDesestresante unaExcursion =
    (<(-2)) . deltaExcursionSegun stress unaExcursion

type Tour = [Excursion]
completo :: Tour
completo = [caminarCiertosMinutos 20, apreciarElementoDelPaisaje "cascada",
    caminarCiertosMinutos 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]


ladoB :: Excursion -> Tour
ladoB elegidaPorTurista = [paseoEnBarco Tranquila, elegidaPorTurista , caminarCiertosMinutos 120]


islaVecina :: Marea -> Tour
islaVecina Fuerte = [paseoEnBarco Fuerte, apreciarElementoDelPaisaje "lago", paseoEnBarco Fuerte]
islaVecina otraMarea = [paseoEnBarco otraMarea, irALaPlaya, paseoEnBarco otraMarea]

hacerQueTuristaHagaTour :: Turista -> Tour -> Turista
hacerQueTuristaHagaTour unTurista unTour =
    hacerTour unTour . modificarStressEn (+ length unTour) $ unTurista

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = foldl (flip ($)) turista tour

algunoEsConvincente :: [Tour] -> Turista -> Bool
algunoEsConvincente tours unTurista = any (esConvincentePara unTurista) tours

esConvincentePara :: Turista -> Tour -> Bool
esConvincentePara turista = any (excursionDesestresanteYDejaAcompanadoA turista)

excursionDesestresanteYDejaAcompanadoA :: Turista -> Excursion -> Bool
excursionDesestresanteYDejaAcompanadoA unTurista unaExcursion
    = excursionDesestresante unaExcursion unTurista && dejaAcompanado unaExcursion unTurista

dejaAcompanado :: Excursion -> Turista -> Bool
dejaAcompanado excursion  = not . estaViajandoSolo . excursion

efectividadDeUnTour :: Tour -> [Turista] -> Int
efectividadDeUnTour unTour =
    sum . espiritualidadRecibida unTour . filter (flip esConvincentePara unTour)

espiritualidadRecibida :: Tour -> [Turista] -> [Int]
espiritualidadRecibida unTour = map (sumasDePerdidasDeEstresYCansancio unTour)

sumasDePerdidasDeEstresYCansancio :: Tour -> Turista -> Int
sumasDePerdidasDeEstresYCansancio unTour unTurista =
    abs (deltaTourSegun cansancio unTour unTurista - deltaTourSegun stress unTour unTurista)

deltaTourSegun :: Indice -> Tour -> Turista -> Int
deltaTourSegun indice tour turista = deltaSegun indice (hacerTour tour turista) turista

playasInf :: Tour
playasInf = repeat irALaPlaya

-- ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
-- Si, usando fst y snd

-- ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar
-- No, para devolver un resutado filter debe evaluar toda la lista, entonces una lista
-- infinita no puede ser filtrada.