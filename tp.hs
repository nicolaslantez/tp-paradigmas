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
pdpCilina :: Medicamento
pdpCilina = medicamento hierbasVerdes

hierbasVerdes = map hierbaVerde enfermedadesInfecciosas

enfermedadesInfecciosas = ["sis", "itis", "emia", "cocos"]

type Colonia = [Raton]
type Observacion = Float
--3)a)
promedioEstudio :: Estudio -> Colonia -> Observacion
promedioEstudio estudio colonia = promedio (map estudio colonia)

promedio xs = realToFrac (sum xs) / genericLength xs

--3)b)
cantidadEnfermos :: Diagnostico -> Colonia -> Observacion
cantidadEnfermos diagnostico colonia = genericLength (filter (==True) (map diagnostico colonia))

--3)c)
deLimite :: Diagnostico -> Estudio -> Colonia -> Observacion
deLimite  diagnostico estudio = maximum . aplicarEstudioEnPeligro diagnostico estudio 

aplicarEstudioEnPeligro diagnostico estudio colonia  = map estudio (ratonesEnPeligro diagnostico colonia)

ratonesEnPeligro diagnostico = filter diagnostico

--4
enfermedadesPeligrosas :: Colonia -> [String]
enfermedadesPeligrosas colonia = filter (condicion colonia) (listaEnfermedadesColonia colonia)

listaEnfermedadesColonia colonia = nub (concat (map listaEnfermedades colonia))

condicion colonia enfermedad = all (diagnosticoEnfermedad enfermedad) colonia

--5
funcionaMedicina :: Diagnostico -> Medicamento -> Colonia -> Bool
funcionaMedicina diagnostico medicamento = any (==False) . diagnosticoConMedicamentoAplicado diagnostico medicamento

diagnosticoConMedicamentoAplicado diagnostico medicamento = map diagnostico . medicamentoParaRatonesEnPeligro medicamento diagnostico

medicamentoParaRatonesEnPeligro medicamento diagnostico = map medicamento . ratonesEnPeligro diagnostico

-- 6
-- [[hierbaBuena,(alcachofa 10)], [hierbaMala]]
--mejorMedicina observacion listaMedicamentos colonia = minimum (estudioEnColonias observacion listaMedicamentos colonia)

mejorMedicina observacion listaMedicamentos colonia = map (listaMedicamentos !!) (elemIndices (minimum (estudioEnColonias observacion listaMedicamentos colonia)) (estudioEnColonias observacion listaMedicamentos colonia))

estudioEnColonias observacion listaDeMedicamentos colonia = map observacion (aplicarListaDeMedicamentosAColonia listaDeMedicamentos colonia)


aplicarListaDeMedicamentosAColonia listaDeMedicamentos colonia = map  (aplicarMedicamentoAColonia colonia) listaDeMedicamentos  


aplicarMedicamentoAColonia colonia medicina = map (medicamento medicina) colonia 

--Modelado para tests
diagnosticoAntiguedad = analisisExceso 1 estudioAntiguedad
diagnosticoMasaCorporal = analisisRangoMedio (18.5,25) estudioMasaCorporal
tratamientoContraAntiguedad raton = tratamiento (analisisExceso 1 estudioAntiguedad) raton [[hierbaBuena],pondsAntiAge]
diagnosticoEnfermedadDisneymania = diagnosticoEnfermedad "disneymania"
coloniaPDP = [jerry,mickeyMouse]
