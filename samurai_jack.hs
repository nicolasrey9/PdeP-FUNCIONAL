
data Elemento = UnElemento
  { tipo :: String,
    ataque :: Personaje -> Personaje,
    defensa :: Personaje -> Personaje
  }

data Personaje = UnPersonaje
  { nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
  }

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio unAnio unPersonaje = unPersonaje{anioPresente = unAnio}

meditar :: Personaje -> Personaje
meditar = afectarSalud (*1.5)

causarDanio ::Float -> Personaje -> Personaje
causarDanio danio = afectarSalud (subtract danio)

----
afectarSalud :: (Float -> Float) -> Personaje -> Personaje
afectarSalud modificador unPersonaje = 
    unPersonaje{salud = max 0 (modificador . salud $ unPersonaje)}
------

esMalvado :: Personaje -> Bool
esMalvado = any ((=="Maldad") . tipo) . elementos

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje unElemento = 
    (salud unPersonaje -) . salud . ataque unElemento $ unPersonaje

type Enemigo = Personaje
enemigosMortales :: Personaje -> [Enemigo] -> [Enemigo]
enemigosMortales unPersonaje = filter (puedeMatarA unPersonaje)

puedeMatarA :: Personaje -> Enemigo -> Bool
puedeMatarA personaje enemigo = 
    any ((== salud personaje) . danioQueProduce personaje) (elementos enemigo)

------------PUNTO 3
concentracion :: Int -> Elemento
concentracion cantidad = UnElemento "Magia" id (meditarTantasVeces cantidad)

meditarTantasVeces :: Int -> Personaje -> Personaje
meditarTantasVeces cantidad = componerEfectos . replicate cantidad $ meditar

componerEfectos :: [Personaje -> Personaje] -> (Personaje -> Personaje)
componerEfectos = foldl1 (.)

esbirro :: Elemento
esbirro = UnElemento "Maldad" (causarDanio 1) id
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirro

jack :: Personaje
jack = UnPersonaje "Jack" 300 [concentracion 3, katanaMagica] 200

katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (causarDanio 1000) id

aku :: Int -> Float -> Personaje
aku anio cantidadSalud = 
    UnPersonaje "Aku" cantidadSalud (concentracion 4 : portalAlFuturo anio : esbirrosMalvados (100*anio)) anio

portalAlFuturo :: Int -> Elemento
portalAlFuturo anioPresente = UnElemento "Magia" (mandarAlAnio $ 2800 + anioPresente) generarAkuFuturo

generarAkuFuturo :: Personaje -> Personaje
generarAkuFuturo unPersonaje = aku (anioPresente unPersonaje) (salud unPersonaje)

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
    | estaMuerto atacante = (defensor, atacante)
    | otherwise = luchar (aplicarOfensivosDeA atacante defensor) (aplicarDefensivos atacante)

estaMuerto :: Personaje -> Bool
estaMuerto unPersonaje = salud unPersonaje == 0

aplicarOfensivosDeA :: Personaje -> Personaje -> Personaje
aplicarOfensivosDeA = componerEfectosDe ataque

aplicarDefensivos :: Personaje -> Personaje
aplicarDefensivos atacante = componerEfectosDe defensa atacante atacante

componerEfectosDe :: (Elemento -> Personaje -> Personaje) -> Personaje -> Personaje -> Personaje
componerEfectosDe tipoEfecto atacante = componerEfectos (map tipoEfecto (elementos atacante))

