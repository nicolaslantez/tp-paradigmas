module TpSpec where

import Test.Hspec
import Tp

main :: IO()
main = hspec $do
        describe "Tests de diagnosticos:" $do
                it "el analisis de exceso para el estudio de antiguedad con indice maximo 1 de Micky Mouse debería dar positivo" $do
                        analisisExceso 1 estudioAntiguedad mickeyMouse `shouldBe` True
                it "el analisis de exceso para el estudio de antiguedad con indice maximo 1 de Jerry debería dar negativo" $do
                        analisisExceso 1 estudioAntiguedad jerry `shouldBe` False
                it "el analisis de rango medio para el estudio de masa coporal con indices entre 18.5 y 25 de Micky Mouse deberia dar positivo" $do
                        analisisRangoMedio (18.5,25) estudioMasaCorporal mickeyMouse `shouldBe` True
                it "el analisis de rango medio para el estudio de masa coporal con indices entre 18.5 y 25 de Jerry deberia dar negativo" $do
                        analisisRangoMedio (18.5,25) estudioMasaCorporal jerry `shouldBe` False
                it "realiza el diagnostico para la enfermedad Disneymania" $do
                        diagnosticoEnfermedadDisneymania jerry `shouldBe` False
                        diagnosticoEnfermedadDisneymania mickeyMouse `shouldBe` True
        describe "Tests medicinas y operaciones:" $do
                it "mezclar una Hierba buena con una Hierba mala crea una hierba que no produce efecto" $do
                        mezclarHierbas hierbaBuena hierbaMala jerry `shouldBe` jerry
                it "mezclar una Hierba buena y una alcachofa 0 crea una hierba que produce el mismo efecto que una Hierba buena" $do
                        mezclarHierbas hierbaBuena (alcachofa 0) jerry `shouldBe` hierbaBuena jerry
                it "mezclar una Hierba mala  y una alcachofa 1 debe crear una hierba pero no producir el mismo efecto que una hierba buena" $do
                        mezclarHierbas hierbaMala (alcachofa 1) jerry `shouldNotBe` hierbaMala jerry
                it  "el medicamento ratisalil combina una Hierba hierbaMala y una Hierba Zort que produce los mismos resultados que una Hierba Zort" $do
                        medicamento ratisalil jerry  `shouldBe` medicamento [hierbaZort] jerry 
                it "el medicamento ponds anti age combina una alcachofa de 10 con 3 hierbas buenas que se le aplica a jerry quedando con edad = 9.5, peso = 1.8 y altura = 0.3" $do
                        medicamento pondsAntiAge jerry `shouldBe` CRaton 9.5 1.8 0.3 ["tuberculosis","varicela","endemia"]
                it "aplicarle un tratamiento contra la antiguedad a mickeyMouse con una hierbaBuena y una pondsAntiAge debe producir los mismos resultados que aplicarle una hierbaBuena" $do
                        tratamiento (analisisExceso 1 estudioAntiguedad) mickeyMouse [[hierbaBuena],pondsAntiAge] `shouldBe` hierbaBuena mickeyMouse
                it "aplicarle un tratamiento contra la antiguedad a jerry con una hierbaBuena y una pondsAntiAge debe producir los mismos resultados que aplicarle una hierbaBuena" $do
                        tratamiento (analisisExceso 1 estudioAntiguedad) jerry [[hierbaBuena],pondsAntiAge] `shouldBe` alcachofa 0 jerry
               -- PUNTO 8 -- 
        describe "Tests de diagnosticos/observaciones :" $do
                it "mickeyMouse tiene disneymania" $do
                        diagnosticoEnfermedad "disneymania" mickeyMouse `shouldBe` True
                
                it "jerry no tiene disneymania" $do
                        diagnosticoEnfermedad "disneymania" jerry `shouldBe` False

                it "al darle pdpCilina a jerry solo le queda varicela como enfermedad" $do
                        pdpCilina jerry `shouldBe` CRaton 76 2 0.3 ["varicela"]
                
                -- PUNTO 9 --
        describe "Tests de colonias :" $do
                it "" $do
                        promedioEstudio coloniaPDP estudioCantidadEnfermedades `shouldBe` 2.5 

                it "la cantidad de enfermos que poseen la enfermedad disneymania en la coloniaPDP es de 1" $do
                        cantidadEnfermos coloniaPDP (diagnosticoEnfermedad "disneymania") `shouldBe` 1

                -- PUNTO 10 --
        describe "Tests de funcionamiento de medicinas :" $do
                it "la pdpCilina no funciona para tratar la enfermedad disneyMania" $do
                        funcionaMedicina (diagnosticoEnfermedad "disneymania") pdpCilina coloniaPDP `shouldBe` False
                
                it "una hierba verde de 'ania' funciona para tratar la enfermedad disneymania" $do
                        funcionaMedicina (diagnosticoEnfermedad "disneymania") (hierbaVerde "ania") coloniaPDP `shouldBe` True
