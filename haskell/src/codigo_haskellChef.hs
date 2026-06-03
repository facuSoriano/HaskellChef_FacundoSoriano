import Data.Bool (Bool)
import Data.Foldable (Foldable(length))

-- PARTE A --

data Participante = UnParticipante{
    nombre :: String,
    trucos :: [Trucazo],
    plato :: Plato
}

type Trucazo = (Plato -> Plato)


data Plato = UnPlato{
    dificultad :: Int,
    componentes :: [Ingrediente]
}deriving (Eq, Show)

type Ingrediente = (String, Float)

agregarIngrediente :: Ingrediente -> Plato -> Plato
agregarIngrediente unIngrediente unPlato = unPlato {componentes = unIngrediente : componentes unPlato}

endulzar :: Float -> Plato -> Plato
endulzar cantAzucar unPlato = agregarIngrediente ("azucar", cantAzucar) unPlato

salar :: Float -> Plato -> Plato
salar cantSal unPlato = agregarIngrediente ("sal", cantSal) unPlato

darSabor :: Float -> Float -> Plato -> Plato
darSabor cantSal cantAzucar unPlato = endulzar cantAzucar . salar cantSal $ unPlato

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato {componentes = map duplicarPeso (componentes unPlato)}

duplicarPeso :: Ingrediente -> Ingrediente
duplicarPeso (nombre , unPeso) = (nombre, unPeso * 2)

simplificar :: Plato -> Plato
simplificar unPlato
    | esComplejo unPlato = unPlato {dificultad = 5, componentes = filter componentesConMasDe10Gramos (componentes unPlato)}
    | otherwise = unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = (length (componentes unPlato) > 5) && (dificultad unPlato > 7)

componentesConMasDe10Gramos :: Ingrediente -> Bool
componentesConMasDe10Gramos (_, unPeso) = unPeso >= 10

tieneIngredientes :: [String] -> Plato -> Bool
tieneIngredientes ingredientesProhibidos unPlato = not $ any (`elem` map fst (componentes unPlato)) ingredientesProhibidos

esVegano :: Plato -> Bool
esVegano unPlato =  tieneIngredientes ["carne", "huevos", "lacteos"] unPlato

esSinTacc :: Plato -> Bool
esSinTacc unPlato = tieneIngredientes ["harina"] unPlato

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = any masDe2GramosDeSal (componentes unPlato)

masDe2GramosDeSal :: Ingrediente -> Bool
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

pinia :: Ingrediente
pinia = ("pinia", 9)

helado :: Ingrediente
helado = ("helado", 8)

galleta :: Ingrediente
galleta = ("galleta", 12)

cafe :: Ingrediente
cafe = ("cafe", 12)

harina :: Ingrediente
harina = ("harina", 10)

sal :: Ingrediente
sal = ("sal", 3)

-- PARTE C --

cocinar :: Participante -> Plato
cocinar unParticipante = foldl (\plato truco -> truco plato) (plato unParticipante) (trucos unParticipante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = tieneMasDificultad unPlato otroPlato && tieneMenorPeso unPlato otroPlato

tieneMasDificultad :: Plato -> Plato -> Bool
tieneMasDificultad unPlato otroPlato = (dificultad unPlato) > (dificultad otroPlato) 

tieneMenorPeso :: Plato -> Plato -> Bool
tieneMenorPeso unPlato otroPlato = pesoTotal unPlato < pesoTotal otroPlato

pesoTotal :: Plato -> Float
pesoTotal unPlato = sum (map snd (componentes unPlato))

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
    | cocinar unParticipante `esMejorQue` cocinar otroParticipante = unParticipante
    | otherwise = otroParticipante

participanteEstrella :: [Participante] -> String
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

chocolate :: Ingrediente
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

-- Parte D --

--endulzar: va a añadir una cantidad de azucar al inicio de la lista pero nunca va a terminar de mostrar la lista
--salara: lo mismo que endulzar
--darSabor: añade la sal y el azucar al inicio de la lista pero no termina de mostrarla entera
--duplicarPorcion: va a ir duplicando el peso de todos los ingredientes pero nunca va a terminar la lista
--simplificar: no va a funcionar ya que va a tener que ejecutar esComplejo, pero no va a poder terminar de contar los
-- componentes de un plato

--esVegano: se va a quedar leyendo los ingredientes uno por uno, y nunca va a encontrar alguno que lo frene
--esSinTacc: lo mismo que esVegano
--esComplejo: no va a poder terminar de contar los componentes de un plato
--noAptoHipertension: se va a quedar buscando el ingrediente con nombre sal eternamente

--No se puede, porque nunca va a terminar de calcular el peso total del plato en la función pesoTotal
