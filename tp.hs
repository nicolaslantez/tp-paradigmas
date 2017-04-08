module Tp where
import Data.List

data Raton = CRaton {edad :: Float, peso :: Float, altura :: Float, enfermedades :: [String] }deriving (Show,Eq)

mickeyMouse = CRaton 88 20 0.8 ["disneymania","hipotermia"]
jerry = CRaton 76 2 0.3 ["tuberculosis","varicela","endemia"]

type Estudio = Raton -> Float

estudioMasaCorporal :: Estudio
estudioMasaCorporal raton = peso raton/(altura raton ^ 2)

estudioAntiguedad :: Estudio
estudioAntiguedad = calcularAntiguedad . edad

calcularAntiguedad = (/85) . (+5)

type Analisis = Estudio -> Diagnostico  
type Diagnostico = Raton -> Bool

analisisExceso ::  Float -> Analisis
analisisExceso valorMaximo estudio = (valorMaximo <) . estudio

analisisRangoMedio ::  (Float,Float) -> Analisis
analisisRangoMedio rango estudio = not . (enRango rango) . estudio

analisisBerreta :: Analisis
analisisBerreta _  = const False

enRango :: (Float,Float) -> Float -> Bool
enRango rango estudio = estudio > fst rango && estudio < snd rango

type Hierba = Raton -> Raton 

hierbaBuena :: Hierba
hierbaBuena = cambiarEdad (/2)

hierbaMala :: Hierba
hierbaMala = cambiarEdad (*2)

alcachofa :: Float -> Hierba
alcachofa  porcentaje = cambiarPeso (calcularPorcentaje porcentaje)

hierbaZort :: Hierba
hierbaZort _ = CRaton 0 0 0 []

cambiarEdad f raton = raton { edad = (f . edad) raton}
cambiarPeso f raton = raton { peso = (f . peso) raton}

calcularPorcentaje:: Float -> Float -> Float
calcularPorcentaje porcentaje peso = peso - ((porcentaje * peso)/100)


mezclarHierbas :: Hierba -> Hierba -> Hierba
mezclarHierbas = (.)

type Medicamento = Raton -> Raton

medicamento :: [Hierba] -> Medicamento
medicamento hierbas raton = foldl tomarHierba raton hierbas

tomarHierba :: Raton -> Hierba -> Raton
tomarHierba raton hierba = hierba raton

tratamiento diagnostico raton hierbas = foldl (aplicarHastaQueDeFalse diagnostico) raton (concat hierbas)

aplicarHastaQueDeFalse diagnostico raton hierba
	|diagnostico raton = hierba raton
	|otherwise = raton


ratisalil = [hierbaZort, hierbaMala]
pondsAntiAge = [alcachofa 10 , hierbaBuena, hierbaBuena, hierbaBuena]

-- PUNTO 2 --
--1)a)
estudioCantidadEnfermedades :: Estudio
estudioCantidadEnfermedades = genericLength . listaEnfermedades

listaEnfermedades :: Raton -> [String]
listaEnfermedades raton = enfermedades raton

--1)b)
diagnosticoEnfermedad :: String -> Diagnostico
diagnosticoEnfermedad enfermedad = (elem enfermedad) . listaEnfermedades

--1)c)
crearPinky :: Float -> Float -> Float -> Raton
crearPinky edad peso altura = CRaton edad peso altura []

--2)a)
hierbaVerde :: String -> Hierba
hierbaVerde palabra = cambiarEnfermedades (eliminarEnfermedades palabra)

cambiarEnfermedades efecto raton = raton { enfermedades = (efecto.enfermedades) raton }

eliminarEnfermedades palabra enfermedades = filter (not.enfermedadTerminaCon palabra) enfermedades 

enfermedadTerminaCon palabra enfermedad = elem palabra (tails enfermedad)

--2)b)
-- :: Hierba?
pdpCilina :: Raton -> Raton
-- para mi medicamento esta demás porque hierbaVerde ya te devuelve un raton curado. o no? --La consigna te dice que es un medicamento
-- hierbasVerdes devuelve una lista de ratones. o no? --no, devuelve una lista de hierbas verdes que el medicamento las va aplicando al raton
pdpCilina = medicamento hierbasVerdes

hierbasVerdes = map hierbaVerde enfermedadesInfecciosas

enfermedadesInfecciosas = ["sis", "itis", "emia", "cocos"]

type Colonia = [Raton]
type Indice = Float
--3)a)
promedioEstudio :: Colonia -> Estudio -> Indice
promedioEstudio colonia estudio = promedio (map estudio colonia)

promedio xs = realToFrac (sum xs) / genericLength xs

--3)b)
cantidadEnfermos :: Colonia -> Diagnostico -> Indice
cantidadEnfermos colonia diagnostico = genericLength (filter (==True) (map diagnostico colonia))

--3)c)
deLimite :: Colonia -> Diagnostico -> Estudio -> Indice
deLimite  colonia diagnostico = maximum . aplicarEstudioEnPeligro diagnostico colonia   

aplicarEstudioEnPeligro diagnostico colonia estudio  = map estudio (ratonesEnPeligro diagnostico colonia)

ratonesEnPeligro diagnostico = filter diagnostico

--4
enfermedadesPeligrosas :: Colonia -> [String]
enfermedadesPeligrosas colonia = nub (filter (condicion colonia) (listaEnfermedadesColonia colonia))

-- el nub podría estar acá nub (concat .....) asi no filtras directamente elementos duplicados y dsp los sacas ( ponele que por performance..)
-- no, el nub te borra los elementos duplicados, aca se usa porque tiene que pasar por todos los ratones, entonces al hacer "condicion" 
-- si una enfermedad se repite te la va a poner en la lista 2 veces. por ejemplo: si "varicela" es la que aparece en todos los ratones,
--va a aparecer 2 veces en la lista final entonces se usa el "nub" para borrar el/los duplicado/s.
listaEnfermedadesColonia colonia = concat (map listaEnfermedades colonia)

condicion colonia enfermedad = all (diagnosticoEnfermedad enfermedad) colonia

--5
funcionaMedicina :: Diagnostico -> Medicamento -> Colonia -> Bool
funcionaMedicina diagnostico medicamento = any (==False) . diagnosticoConMedicamentoAplicado diagnostico medicamento

diagnosticoConMedicamentoAplicado diagnostico medicamento = map diagnostico . medicamentoParaRatonesEnPeligro medicamento diagnostico

medicamentoParaRatonesEnPeligro medicamento diagnostico = map medicamento . ratonesEnPeligro diagnostico

--6
--mejorMedicina :: ALGO -> [Medicamento] -> Colonia -> Medicamento
-- esto devuelve una lista de floats ( es decir, una lista de los valores despues de aplicar el medicamento a cada raton) ahora debería sacar el promedio y dsp compararlo contra las otras medicinas
-- ejemplo [[1,2,3],[3,2,1],[1,1,1]]
-- aca tengo q calcular el promedio que seria 2,2,1 por ende me quedo con el 1 y devuelvo esa hierba!!! POLE HELP HERE! :D
--mejorMedicina estudioPunto3 listaDeMedicamentos = map (diagnosticoMedicamentoAplicado estudioPunto3) listaMedicamento  


--Modelado para tests
diagnosticoEnfermedadDisneymania raton = diagnosticoEnfermedad "disneymania" raton
coloniaPDP = [jerry,mickeyMouse]
