module Tp where 

data Raton = CRaton {edad :: Float, peso :: Float, altura :: Float, enfermedades :: [String] }deriving (Show,Eq)

mickeyMouse = CRaton 88 20 0.8 ["disneymania","hipotermia"]
jerry = CRaton 76 2 0.3 ["tuberculosis","varicela","endemia"]

type Estudio = Raton -> Indice
type Indice = Float
estudioMasaCorporal :: Estudio
estudioMasaCorporal raton = peso raton/(altura raton ^ 2)

estudioAntiguedad :: Estudio
estudioAntiguedad = calcularAntiguedad . edad

calcularAntiguedad edad = (edad + 5) / 85
-- calcularAntiguedad (/85) . (+5)

type Analisis = Estudio -> Diagnostico
type Diagnostico = Raton -> Bool

deExceso :: Float -> Analisis
deExceso valorMaximo estudio = (valorMaximo <) . estudio

deRangoMedio :: (Float, Float) -> Analisis 
analisisDeRangoMedio rango estudio = not . (enRango rango) . estudio

berreta :: Analisis
berreta _ = const False

hierbaBuena :: Hierba
hierbaBuena (CRaton edad peso altura enfermedades) = CRaton (rejuvenecerRaton edad) peso altura enfermedades
--analisisExceso :: (Raton -> Float) -> Float -> Raton -> Bool
--analisisExceso estudio valor raton = estudio raton > valor

hierbaMala :: Hierba
hierbaMala (CRaton edad peso altura enfermedades) = CRaton (envejecerRaton edad) peso altura enfermedades

alcachofa :: Float -> Hierba
alcachofa porcentaje (CRaton edad peso altura enfermedades) = CRaton edad (reducirPeso porcentaje peso) altura enfermedades

hierbaZort :: Hierba
hierbaZort (CRaton _ _ _ enfermedades) = CRaton 0 0 0 enfermedades


type Hierba = Raton -> Raton 

hierbaBuena raton = cambiarEdad (/2)

hiberbaMala raton = cambiarEdad (*2)

alcachofa :: Float -> Hierba
alcachofa porcentaje raton = cambiarPeso (calcularPorcentaje porcentaje)

cambiarEdad f raton = raton { edad = (f . getEdad) raton}

cambiarPeso f raton = raton { peso = (f. getPeso) raton}

hierbaZort _ = CRaton 0 0 0


mezclarHierbas :: Hierba -> Hierba -> Hierba
mezclarHierbas = (.)

medicamento :: Raton -> [(Raton -> Raton)] -> Raton
medicamento raton hierbas = foldl tomarHierba raton hierbas

tomarHierba :: Raton -> (Raton -> Raton) -> Raton
tomarHierba raton hierba = hierba raton

tratamiento diagnostico raton hierbas = foldl (aplicarHastaQueDeFalse diagnostico) raton (concat hierbas)

aplicarHastaQueDeFalse diagnostico raton hierba
	|diagnostico raton = hierba raton
	|otherwise = raton


ratisalil = [hierbaZort, hierbaMala]
pondsAntiAge = [alcachofa 10 , hierbaBuena, hierbaBuena, hierbaBuena]

cantidadEnfermedades :: Raton -> Int
cantidadEnfermedades raton = length (enfermedades raton)

diagnosticoEnfermedad :: Raton -> String -> Bool
diagnosticoEnfermedad raton enfermedad = elem enfermedad (enfermedades raton)
