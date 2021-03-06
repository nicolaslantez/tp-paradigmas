module TpSpec where

import Test.Hspec
import Tp

main :: IO()
main = hspec $do
        describe "Tests de diagnosticos:" $do
                it "el analisis de exceso para el estudio de antiguedad con indice maximo 1 de Micky Mouse debería dar positivo" $do
                        diagnosticoAntiguedad mickeyMouse `shouldBe` True
                it "el analisis de exceso para el estudio de antiguedad con indice maximo 1 de Jerry debería dar negativo" $do
                        diagnosticoAntiguedad jerry `shouldBe` False
                it "el analisis de rango medio para el estudio de masa coporal con indices entre 18.5 y 25 de Micky Mouse deberia dar positivo" $do
                        diagnosticoMasaCorporal mickeyMouse `shouldBe` True
                it "el analisis de rango medio para el estudio de masa coporal con indices entre 18.5 y 25 de Jerry deberia dar negativo" $do
                        diagnosticoMasaCorporal jerry `shouldBe` False                        
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
                        tratamientoContraAntiguedad mickeyMouse `shouldBe` hierbaBuena mickeyMouse
                it "aplicarle un tratamiento contra la antiguedad a jerry con una hierbaBuena y una pondsAntiAge debe producir los mismos resultados que aplicarle una hierbaBuena" $do
                        tratamientoContraAntiguedad jerry `shouldBe` alcachofa 0 jerry
               -- PUNTO 8 -- 
        describe "Tests de diagnosticos/observaciones :" $do
                it "mickeyMouse tiene disneymania" $do
                        diagnosticoEnfermedadDisneymania mickeyMouse `shouldBe` True
                
                it "jerry no tiene disneymania" $do
                        diagnosticoEnfermedadDisneymania jerry `shouldBe` False

                it "al darle pdpCilina a jerry solo le queda varicela como enfermedad" $do
                        pdpCilina jerry `shouldBe` CRaton 76 2 0.3 ["varicela"]
                
                -- PUNTO 9 --
        describe "Tests de colonias :" $do
                it "" $do
                        promedioEstudio estudioCantidadEnfermedades coloniaPDP `shouldBe` 2.5 

                it "la cantidad de enfermos que poseen la enfermedad disneymania en la coloniaPDP es de 1" $do
                        cantidadEnfermos (diagnosticoEnfermedad "disneymania") coloniaPDP `shouldBe` 1

                -- PUNTO 10 --
        describe "Tests de funcionamiento de medicinas :" $do
                it "la pdpCilina no funciona para tratar la enfermedad disneyMania" $do
                        funcionaMedicina (diagnosticoEnfermedad "disneymania") pdpCilina coloniaPDP `shouldBe` False
                
                it "una hierba verde de 'ania' funciona para tratar la enfermedad disneymania" $do
                        funcionaMedicina (diagnosticoEnfermedad "disneymania") (hierbaVerde "ania") coloniaPDP `shouldBe` True
                it "es mejorMedicina una basada en hierbaBuena y alcachofa 100 que una basada en hierbaMala." $do
                        medicamento ((mejorMedicina ((deLimite (diagnosticoEnfermedad "disneymania")) estudioMasaCorporal) [[hierbaBuena,(alcachofa 100)], [hierbaMala]] coloniaPDP) !! 0) mickeyMouse `shouldBe` medicamento [hierbaBuena,(alcachofa 100)] mickeyMouse
