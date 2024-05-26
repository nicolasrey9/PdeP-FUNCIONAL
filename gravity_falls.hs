---------------------------PUNTO 1
data Criatura = Criatura{
    peligrosidad :: Int,
    puedeSuperarla :: Persona -> Bool
}
data Persona = Persona{
    edad :: Int,
    items :: [String],
    experiencia :: Int
}
siempreDetras :: Criatura
siempreDetras = Criatura 0 (\_ ->True)

gnomos :: Int -> Criatura
gnomos gnomosAgrupados = Criatura (2^gnomosAgrupados) (poseeItem "soplador de hojas")
poseeItem :: String -> Persona -> Bool
poseeItem item = elem item . items

fantasma :: Int -> (Persona -> Bool) -> Criatura
fantasma categoria = Criatura (categoria * 20)

---------------------------PUNTO 2

enfrentar :: Persona -> Criatura -> Persona
enfrentar unaPersona unaCriatura
    | puedeSuperarla unaCriatura unaPersona = ganaTantaExperiencia (peligrosidad unaCriatura) unaPersona
    | otherwise = ganaTantaExperiencia 1 unaPersona

ganaTantaExperiencia :: Int -> Persona -> Persona
ganaTantaExperiencia valor unaPersona = unaPersona{experiencia= valor + experiencia unaPersona}

---------------------------PUNTO 3
experienciaAGanar :: Persona -> [Criatura] -> Int
experienciaAGanar unaPersona criaturas =
    (experiencia . enfrentarATodasLasCriaturas criaturas) unaPersona - experiencia unaPersona

enfrentarATodasLasCriaturas :: [Criatura] -> Persona -> Persona
enfrentarATodasLasCriaturas criaturas persona = foldl enfrentar persona criaturas

ejemploConsulta :: Persona -> Int
ejemploConsulta unaPersona =
    experienciaAGanar unaPersona [siempreDetras, gnomos 10, fantasma 3 tengaMenosDe13YDisfrazObeja, fantasma 1 masDe10DeExperiencia]

tengaMenosDe13YDisfrazObeja :: Persona -> Bool
tengaMenosDe13YDisfrazObeja unaPersona = ((>13) . edad) unaPersona && poseeItem "disfraz de oveja" unaPersona

masDe10DeExperiencia :: Persona -> Bool
masDe10DeExperiencia = (>10) . experiencia

------------------ SEGUNDA PARTE
---- 1)
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ _ [] = []
zipWithIf _ _ [] _ = []
zipWithIf operacion criterio (x:xs) (y:ys)
    | criterio y = operacion x y : zipWithIf operacion criterio xs ys
    | otherwise = y : zipWithIf operacion criterio (x : xs) ys

--- 2) --a
abecedario :: [Char]
abecedario = ['a' .. 'z']
abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = drop (length (takeWhile (/= letra) abecedario)) abecedario ++ takeWhile (/= letra) abecedario

--b
desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraMargen letraADesencriptar =
    obtenerLetraConDistancia letraADesencriptar . obtenerDistanciaMargen $ letraMargen

obtenerDistanciaMargen :: Char -> Int
obtenerDistanciaMargen = length . takeWhile (/= 'a') . abecedarioDesde

obtenerLetraConDistancia :: Char -> Int -> Char
obtenerLetraConDistancia letraADesencriptar distanciaEncriptado =
    (!! distanciaEncriptado) . abecedarioDesde $ letraADesencriptar

--c
cesar :: Char -> String -> String
cesar letraMargen = zipWithIf desencriptarLetra (`elem` abecedario) (repeat letraMargen)

ejemploConsulta2 :: Char -> String
ejemploConsulta2 letraMargen = cesar letraMargen "jrzel zrfaxal!"

----- 3)
vigenere :: String -> String -> String
vigenere textoClave mensaje = traducirLetraALetra mensaje . extenderTextoA (length mensaje) $ textoClave

extenderTextoA :: Int -> String -> String
extenderTextoA _ [] = []
extenderTextoA largo texto
    | length texto == largo = texto
    | length texto > largo = extenderTextoA largo (init texto)
    | length texto < largo = extenderTextoA largo (texto ++ texto)

traducirLetraALetra :: String -> String -> String
traducirLetraALetra mensajeATraducir clave =
    zipWithIf desencriptarLetra (\letra -> True) clave mensajeATraducir
