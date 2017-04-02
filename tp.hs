module Tp where 

data Raton = CRaton {edad :: Float, peso :: Float, altura :: Float, enfermedades :: [String] }deriving (Show,Eq)

mickeyMouse = CRaton 88 20 0.8 ["disneymania","hipotermia"]
jerry = CRaton 76 2 0.3 ["tuberculosis","varicela","endemia"]


estudioMasaCorporal :: Raton -> Float
estudioMasaCorporal raton = peso raton/(altura raton ^ 2)

estudioAntiguedad :: Raton -> Float
estudioAntiguedad raton = (edad raton + 5) / 85


analisisExceso :: (Raton -> Float) -> Float -> Raton -> Bool
analisisExceso estudio valor raton = estudio raton > valor

analisisRangoMedio :: (Raton -> Float) -> Raton -> Float -> Float -> Bool
analisisRangoMedio estudio raton valorMinimo valorMaximo = not(estudio raton > valorMinimo && estudio raton < valorMaximo)

analisisBerreta :: (Raton -> Float) -> Raton -> Bool
analisisBerreta _ _ = False


hierbaBuena :: Hierba
hierbaBuena (CRaton edad peso altura enfermedades) = CRaton (rejuvenecerRaton edad) peso altura enfermedades


hierbaMala :: Hierba
hierbaMala (CRaton edad peso altura enfermedades) = CRaton (envejecerRaton edad) peso altura enfermedades

alcachofa :: Float -> Hierba
alcachofa porcentaje (CRaton edad peso altura enfermedades) = CRaton edad (reducirPeso porcentaje peso) altura enfermedades

hierbaZort :: Hierba
hierbaZort (CRaton _ _ _ enfermedades) = CRaton 0 0 0 enfermedades


rejuvenecerRaton :: Float -> Float
rejuvenecerRaton =(/2)

envejecerRaton :: Float -> Float
envejecerRaton = (*2)

reducirPeso:: Float -> Float -> Float
reducirPeso porcentaje peso = peso - ((porcentaje * peso)/100)


type Hierba = Raton -> Raton 
mezclarHierbas :: Hierba -> Hierba -> Hierba
mezclarHierbas = (.)

medicamento :: Raton -> [(Raton -> Raton)] -> Raton
medicamento raton hierbas = foldl tomarHierba raton hierbas

tomarHierba :: Raton -> (Raton -> Raton) -> Raton
tomarHierba raton hierba = hierba raton

tratamiento diagnostico raton hierbas = foldl (aplicarHastaQueDeFalse diagnostico) raton (concat hierbas)

aplicarHastaQueDeFalse diagnostico raton hierba  
	| diagnostico raton = hierba raton
	| otherwise = raton


ratisalil = [hierbaZort, hierbaMala]
pondsAntiAge = [alcachofa 10 , hierbaBuena, hierbaBuena, hierbaBuena]

cantidadEnfermedades :: Raton -> Int
cantidadEnfermedades raton = length (enfermedades raton) 