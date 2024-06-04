
data Autobot =  Robot String (Int,Int,Int) ((Int,Int,Int) -> (Int,Int)) | 
    Vehiculo String (Int,Int)

optimus :: Autobot
optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)

jazz :: Autobot
jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)

wheeljack :: Autobot
wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)

bumblebee :: Autobot
bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)

autobots :: [Autobot]
autobots = [ optimus, jazz, wheeljack, bumblebee ]

maximoSegun3 :: Ord b => (a -> a -> b) -> a -> a -> a
maximoSegun3 funcion parametroA parametroB
    | funcion parametroA parametroB > funcion parametroB parametroA = parametroA
    | otherwise = parametroB
{-}
Implementar las diferentes funciones que permiten acceder a los atributos 
de los Autobots sin importar si 
están transformados o no. Como se dijo anteriormente, la fuerza de un vehículo es 0.
data Autobot =  Robot String (Int,Int,Int) ((Int,Int,Int) -> (Int,Int)) | 
    Vehiculo String (Int,Int)   -}

nombre :: Autobot -> String
nombre (Robot name _ _) = name
nombre (Vehiculo name _) = name

fuerza :: Autobot -> Int
fuerza (Robot _ (suFuerza, _, _) _) = suFuerza
fuerza (Vehiculo _ _) = 0

velocidad :: Autobot -> Int
velocidad (Robot _ (_ , suVelocidad, _) _) = suVelocidad
velocidad (Vehiculo _ (suVelocidad, _)) = suVelocidad

resistencia :: Autobot -> Int
resistencia (Robot _ (_ , _, suResistencia) _) = suResistencia
resistencia (Vehiculo _ (_, suResistencia)) = suResistencia

funcionTransformadora :: Autobot -> ((Int,Int,Int) -> (Int,Int))
funcionTransformadora (Robot _ _ func) = func
capacidades :: Autobot -> (Int, Int, Int)
capacidades (Robot _ capacidad _) = capacidad
capacidades (Vehiculo _ (x,y)) = (0, x, y)

transformar :: Autobot -> Autobot
transformar robot = 
    Vehiculo (nombre robot) (funcionTransformadora robot $ capacidades robot)

velocidadContra :: Autobot -> Autobot -> Int
velocidadContra primerAutobot segundoAutobot =
    velocidad primerAutobot - max 0 (fuerza segundoAutobot - resistencia primerAutobot)

elMasRapido :: Autobot -> Autobot -> Autobot
elMasRapido autobot1 autobot2
    | velocidadContra autobot1 autobot2 > velocidadContra autobot1 autobot2 = autobot1
    | otherwise = autobot2

domina :: Autobot -> Autobot -> Bool
domina dominante dominado
    | not (esRobot dominante && esRobot dominado) = dominanteEsMasRapido
    | dominanteEsMasRapido = domina (transformar dominante) dominado && domina dominante (transformar dominado) && domina (transformar dominante) (transformar dominado)
    | otherwise = False
    where
        dominanteEsMasRapido = capacidades (elMasRapido dominante dominado) == capacidades dominante

esRobot :: Autobot -> Bool
esRobot (Robot _ _ _) = True
esRobot _ = False

losDominaATodos :: Autobot -> [Autobot] -> Bool
losDominaATodos dominador = all (domina dominador)

quienesCumplen :: (Autobot -> Bool) -> [Autobot] -> [Autobot]
quienesCumplen = filter 

consulta :: Autobot -> [Autobot] -> Bool
consulta autobotX autobots = 
    losDominaATodos autobotX autobots && ((`elem` "aeiou") . last . nombre) autobotX
