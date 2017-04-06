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

medicamento :: Raton -> [Hierba] -> Raton
medicamento raton hierbas = foldl tomarHierba raton hierbas

tomarHierba :: Raton -> Hierba -> Raton
tomarHierba raton hierba = hierba raton

tratamiento diagnostico raton hierbas = foldl (aplicarHastaQueDeFalse diagnostico) raton (concat hierbas)

aplicarHastaQueDeFalse diagnostico raton hierba
	|diagnostico raton = hierba raton
	|otherwise = raton


ratisalil = [hierbaZort, hierbaMala]
pondsAntiAge = [alcachofa 10 , hierbaBuena, hierbaBuena, hierbaBuena]

cantidadEnfermedades :: Raton -> Int
cantidadEnfermedades = length . comboEnfermedades

-- nombre choto , mejor listaDeEnfermedades?
comboEnfermedades :: Raton -> [String]
comboEnfermedades raton = enfermedades raton

diagnosticoEnfermedad :: String -> Diagnostico
diagnosticoEnfermedad enfermedad = (elem enfermedad) . comboEnfermedades

crearPinky :: Float -> Float -> Float -> Raton
crearPinky edad peso altura = CRaton edad peso altura []

hierbaVerde :: String -> Raton -> Raton



["sarasa","peces","papasa"]
palabraBuscada = "asa"


funcionX listaDePalabras palabraBuscada =  map (funcion palabraBuscada) (listaDePalabras)


-- 	"asa"  		"sarasa"
funcion  palabraBuscada palabra = hayQueEliminarPalabra palabraBuscada (tails palabra)
-- ["sarasa","arasa","rasa","asa","sa","a",""]



----funcioncita :: Raton -> String -> Raton
----funcioncita raton palabra = raton { enfermedades = ((eliminarPalabra palabra) . comboEnfermedades) raton}

--eliminarPalabra palabra [] = []
--eliminarPalabra palabra (x:xs)
--		| x == palabra = eliminarPalabra palabra xs
--		| otherwise = x: eliminarPalabra palabra xs
hayQueEliminarPalabra palabra lista = elem palabra lista 
