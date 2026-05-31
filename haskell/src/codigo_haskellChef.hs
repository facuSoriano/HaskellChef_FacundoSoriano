import Data.Bool (Bool)
import Data.Foldable (Foldable(length))

-- PARTE A --

data Participantes = UnParticipante{
    nombre :: String,
    trucos :: [Trucazos],
    plato :: Plato
}

type Trucazos = (Plato -> Plato)


data Plato = UnPlato{
    dificultad :: Int,
    componentes :: [Ingredientes]
}deriving (Eq, Show)

type Ingredientes = (String, Float)

agregarIngrediente :: Ingredientes -> Plato -> Plato
agregarIngrediente unIngrediente unPlato = unPlato {componentes = unIngrediente : componentes unPlato}

endulzar :: Float -> Plato -> Plato
endulzar cantAzucar unPlato = agregarIngrediente ("azucar", cantAzucar) unPlato

salar :: Float -> Plato -> Plato
salar cantSal unPlato = agregarIngrediente ("sal", cantSal) unPlato

darSabor :: Float -> Float -> Plato -> Plato
darSabor cantSal cantAzucar unPlato = endulzar cantAzucar . salar cantSal $ unPlato

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato {componentes = map duplicarCantidad (componentes unPlato)}

duplicarCantidad :: Ingredientes -> Ingredientes
duplicarCantidad (nombre , unPeso) = (nombre, unPeso * 2)

simplificar :: Plato -> Plato
simplificar unPlato
    | esComplejo unPlato = unPlato {dificultad = 5, componentes = filter componentesConMasDe10Gramos (componentes unPlato)}
    | otherwise = unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = (length (componentes unPlato) > 5) && (dificultad unPlato > 7)

componentesConMasDe10Gramos :: Ingredientes -> Bool
componentesConMasDe10Gramos (_, unPeso) = unPeso >= 10

esVegano :: Plato -> Bool
esVegano unPlato =  not (any (`elem` map fst (componentes unPlato)) ["carne", "huevos", "lacteos"]) 

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not ("harina" `elem` map fst (componentes unPlato))

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = any masDe2GramosDeSal (componentes unPlato)

masDe2GramosDeSal :: Ingredientes -> Bool
masDe2GramosDeSal (ingrediente, unPeso) = ingrediente == "sal" && unPeso > 2

-- PARTE B --

pepe_Ronccino = UnParticipante{
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    plato = especialidadPepe
}

especialidadPepe = UnPlato{
    dificultad = 8,
    componentes = [sal, harina, cafe, galleta, helado, pinia]
}

pinia :: Ingredientes
pinia = ("pinia", 9)

helado :: Ingredientes
helado = ("helado", 8)

galleta :: Ingredientes
galleta = ("galleta", 12)

cafe :: Ingredientes
cafe = ("cafe", 12)

harina :: Ingredientes
harina = ("harina", 10)

sal :: Ingredientes
sal = ("sal", 3)

-- PARTE C --

cocinar :: Participantes -> Plato
cocinar unParticipante = foldl (\plato truco -> truco plato) (plato unParticipante) (trucos unParticipante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = tieneMasDificultad unPlato otroPlato && tieneMenorPeso unPlato otroPlato

tieneMasDificultad :: Plato -> Plato -> Bool
tieneMasDificultad unPlato otroPlato = (dificultad unPlato) > (dificultad otroPlato) 

tieneMenorPeso :: Plato -> Plato -> Bool
tieneMenorPeso unPlato otroPlato = pesoTotal unPlato < pesoTotal otroPlato

pesoTotal :: Plato -> Float
pesoTotal unPlato = sum (map snd (componentes unPlato))

mejorParticipante :: Participantes -> Participantes -> Participantes
mejorParticipante unParticipante otroParticipante
    | cocinar unParticipante `esMejorQue` cocinar otroParticipante = unParticipante
    | otherwise = otroParticipante

participanteEstrella :: [Participantes] -> String
participanteEstrella = nombre . foldl1 mejorParticipante 

-- Participantes para probar el codigo --
jorge = UnParticipante{
    nombre = "Jorge",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    plato = especialidadJorge
}

especialidadJorge = UnPlato{
    dificultad = 7,
    componentes = [sal, harina, cafe, galleta, helado, pinia, chocolate]
}

chocolate :: Ingredientes
chocolate = ("chocolate", 2)

karina = UnParticipante{
    nombre = "Karina",
    trucos = [endulzar 2 , duplicarPorcion],
    plato = especialidadKarina
}

especialidadKarina = UnPlato{
    dificultad = 9,
    componentes = [sal, harina, cafe, galleta, helado]
}
