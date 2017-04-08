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
-- para mi medicamento esta demás porque hierbaVerde ya te devuelve un raton curado. o no?
-- hierbasVerdes devuelve una lista de ratones. o no?
pdpCilina = medicamento hierbasVerdes

hierbasVerdes = map hierbaVerde enfermedadesInfecciosas

enfermedadesInfecciosas = ["sis", "itis", "emia", "cocos"]

type Colonia = [Raton]
-- yo haria un type Indice = Float
--3)a)
promedioEstudio :: Colonia -> Estudio -> Float
promedioEstudio colonia estudio = promedio (map estudio colonia)

promedio xs = realToFrac (sum xs) / genericLength xs

--3)b)
cantidadEnfermos :: Colonia -> Diagnostico -> Float
cantidadEnfermos colonia diagnostico = genericLength (filter (==True) (map diagnostico colonia))

--3)c)
--esto esta bien y teoricamente funciona pero en lugar de hacerlo con guardas por que no usar la funcion del punto 3)b)?
deLimite :: Colonia -> Diagnostico -> Estudio -> Float
deLimite  colonia diagnostico estudio = maximum (map (aplicarEstudioEnPeligro diagnostico estudio) colonia)

aplicarEstudioEnPeligro diagnostico estudio raton
	| diagnostico raton = estudio raton
	| otherwise = 0

--4
enfermedadesPeligrosas :: Colonia -> [String]
enfermedadesPeligrosas colonia = nub (filter (condicion colonia) (listaEnfermedadesColonia colonia))

-- el nub podría estar acá nub (concat .....) asi no filtras directamente elementos duplicados y dsp los sacas ( ponele que por performance..)
listaEnfermedadesColonia colonia = concat (map listaEnfermedades colonia)

condicion colonia enfermedad = all (diagnosticoEnfermedad enfermedad) colonia

--5
funcionaMedicina :: Diagnostico -> Medicamento -> Colonia -> Bool
funcionaMedicina diagnostico medicamento = any (==False) . diagnosticoConMedicamentoAplicado diagnostico medicamento

diagnosticoConMedicamentoAplicado diagnostico medicamento = map diagnostico . medicamentoParaRatonesEnPeligro medicamento diagnostico

medicamentoParaRatonesEnPeligro medicamento diagnostico = map medicamento . filter diagnostico

--6
--mejorMedicina :: ALGO -> [Medicamento] -> Colonia -> Medicamento
--mejorMedicina estudioPunto3 listaDeMedicamentos colonia = map estudioPunto3ConSusParametros colonia [medicamento] (esto rompe porque en el otro lo ultimo q recibe no es la lista de medicamentos 

--Modelado para tests
diagnosticoEnfermedadDisneymania raton = diagnosticoEnfermedad "disneymania" raton
coloniaPDP = [jerry,mickeyMouse]
