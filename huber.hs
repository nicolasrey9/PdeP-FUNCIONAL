import Distribution.Utils.Generic (trdOf3, fstOf3)
data Chofer = Chofer{
    nombreChofer :: String,
    kiometraje :: Int,
    viajes :: [Viaje],
    condicion :: Condicion
}
type Condicion = Viaje -> Bool
data Cliente = Cliente{
    nombreCliente :: String,
    direccion :: String
}
type Viaje = (Cliente, Fecha, Costo)
type Fecha = String
type Costo = Int

tomarCualquierViaje :: Condicion
tomarCualquierViaje _ = True

tomarViajesDeMasDe200 :: Condicion
tomarViajesDeMasDe200 = (>200) . trdOf3

masDeNLetras :: Int -> Condicion
masDeNLetras n = (>n) . length . nombreCliente . fstOf3

queNoVivaEn :: String -> Condicion
queNoVivaEn localidad = (/= localidad) . direccion . fstOf3

lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [(lucas, "20/04/2017", 150)] (queNoVivaEn "Olivos")

alejandra :: Chofer
alejandra = Chofer "Alejandra" 180000 [] tomarCualquierViaje

puedeTomarViaje :: Chofer -> Viaje -> Bool
puedeTomarViaje (Chofer _ _ _ condicion) = condicion

liquidacionDeChofer :: Chofer -> Int
liquidacionDeChofer = sum . map trdOf3 . viajes

realizarUnViaje :: [Chofer] -> Viaje -> Chofer
realizarUnViaje choferes unViaje = efectuarElViaje unViaje . quienTieneMenosViajes . quienesToman choferes unViaje

quienesToman :: [Chofer] -> Viaje -> [Chofer]
quienesToman choferes unViaje = filter (flip puedeTomarViaje unViaje) choferes

quienTieneMenosViajes :: [Chofer] -> Chofer
quienTieneMenosViajes [x] = x
quienTieneMenosViajes (x:y:ys)
    | (length . viajes) x < (length . viajes) y = quienTieneMenosViajes (x:ys)
    | otherwise                                 = quienTieneMenosViajes (y:ys)

quienTieneMenosViajes' :: [Chofer] -> Chofer
quienTieneMenosViajes' choferes = head . filter ((== menorViajes choferes) . cantidadDeViajes) $ choferes
menorViajes :: [Chofer] -> Int
menorViajes = maximum . map cantidadDeViajes
cantidadDeViajes :: Chofer -> Int
cantidadDeViajes = length . viajes























