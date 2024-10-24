-----------------------------------------------------------
--------------------- MARTIN DORE -------------------------
-----------------------------------------------------------
import Text.CSV
import Data.List (maximumBy)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Debug.Trace

type Row = [String]
type Rule = (Int, [String], [(Int, String)]) -- (soporte, [antecedente], [(confianza, clase)])
type RuleSet = [Rule]

nuevaRegla :: Row -> Rule
nuevaRegla     []   = error "Registro vacio..."
nuevaRegla  (x:[])  = error "Registro con un solo item (falta consecuente)"
nuevaRegla registro = (1, init registro, [(1, last registro)])

-- regarde si une règle à le même antécédent qu'une ligne et renvoie T/F
antecedenteIgual :: Rule -> Row -> Bool
antecedenteIgual (_, ant, _) reg = ant == init reg

actualizarRegla :: Rule -> String -> Rule
actualizarRegla (sop, ant, consec) clase = (sop+1, ant, actualizarConsecuente consec clase)
  where
  actualizarConsecuente consec clase
    | 0 == length [(conf, clase0) | (conf, clase0)<-consec , clase0==clase] = (1, clase):consec
    | otherwise = map ( \(conf,clase0) -> if clase0==clase then (conf+1,clase0) else (conf,clase0)  ) consec

mejorRegla :: Rule -> Rule -> Bool
mejorRegla (sop1, _ , _) (sop2, _ , _) = (sop1>sop2)

reglasClasificacion:: RuleSet -> Row -> RuleSet
reglasClasificacion [] reg = [ nuevaRegla reg ]
reglasClasificacion (r:resto) reg
  | antecedenteIgual r reg = actualizarRegla r (last reg) : resto
  | otherwise = r : reglasClasificacion resto reg

-- LEER FICHERO CSV --
loadCSV file = do
  text <- readFile file
  case (parseCSV file text) of
    Right csv -> if (length $ last csv) > 1 then return csv
                 else return $ init csv
    Left err -> error $ "fallo al leer '" ++ file ++ "':\n" ++ (show err)

-- QUITSORT DE ALTO ORDEN (recibe la función con la que comparar y la lista a ordenar)
qs :: (a -> a -> Bool) -> [a] -> [a]
qs _ [] = []
qs comp (p:cola) = qs comp izq ++ [p] ++ qs comp der
    where izq = [n | n<-cola, comp n p]
          der = [n | n<-cola, not (comp n p)]




---------------------------------------------------------------------
-------------------- FUNCIONES A PROGRAMAR --------------------------
---------------------------------------------------------------------
-- toma un conjunto de reglas y devuelve la primera regla
primeraReglaRuleSet :: RuleSet -> Rule
primeraReglaRuleSet [] = error "RuleSet vacío"
primeraReglaRuleSet (primeraRegla:_) = primeraRegla

-- toma una regla y devuelve la clase con mejor confianza
-- por ejemplo getClassRule [(4,["b","y","p","c","v","f","f","t","g"],[(1,"POS"),(3,"NEG")])] >>>>> "NEG"
getClassRule :: Rule -> String
getClassRule (_, _, clases) = snd $ maximumBy (comparing fst) clases

-- toma una linea y devuelve la clase
-- por ejemplo getClassRow ["b","u","g","w","v","t","t","f","g","POS"] >>>>> "POS"
getClassRow :: Row -> String
getClassRow [] = error "Linea vacía"
getClassRow row = last row

-- toma un conjunto de regla y una linea y devuelve el conjunto de reglas con antecedente iguales
matchingRules :: RuleSet -> Row -> RuleSet
matchingRules [] _ = []
matchingRules (r:resto) reg
  | antecedenteIgual r reg  = r : matchingRules resto reg
  | otherwise = matchingRules resto reg

-- predecir
-- toma un conjunto de regla y una linea y predice la clase de la linea con reglas indenticas
predecir :: RuleSet -> Row -> String
predecir [] _ = "NULL"
predecir rules row
  | null reglasAcuerdas = "NULL"
  | otherwise = getClassRule $ primeraReglaRuleSet sortedRules
  where
    reglasAcuerdas = {-traceShowId $-} matchingRules rules row
    sortedRules = qs mejorRegla reglasAcuerdas

-- Cuenta el número de columnas idénticas entre el antecedente de una regla y una línea.
-- rule_ejemplo = (12,["b","u","g","c","v","f","f","f","g"],[(10,"NEG"),(2,"POS")])
-- row_ejemplo = ["b","u","g","c","h","f","f","t","g","NEG"] 
-- Por ejemplo antecedanteSimilar rule_ejemplo row_ejemplo >>>> 7
antecedanteSimilar :: Rule -> Row -> Int
antecedanteSimilar (_, ant, _) reg = length $ filter id $ zipWith (==) ant reg

-- toma un conjunto de regla y una linea y devuelve el conjunto de reglas con antecedente similares
matchingRules2 :: RuleSet -> Row -> RuleSet
matchingRules2 [] _ = []
matchingRules2 (r:resto) reg
  | antecedanteSimilar r reg > 0 = r : matchingRules resto reg -- si queremos modificar el numero de columna identica minimal
  | otherwise = matchingRules resto reg

-- Compara las reglas teniendo en cuenta el soporte y el número de antecedentes similares con la línea dada.
mejorRegla2 :: Rule -> Rule -> Row -> Bool
mejorRegla2 (sop1, ant1, _) (sop2, ant2, _) row =
    antecedanteSimilar (sop1, ant1, []) row > antecedanteSimilar (sop2, ant2, []) row || 
    (antecedanteSimilar (sop1, ant1, []) row == antecedanteSimilar (sop2, ant2, []) row && sop1 > sop2)

-- Toma un conjunto de regla y una linea y predice la clase de la linea con la regla mas parecida
predecir2 :: RuleSet -> Row -> String
predecir2 [] _ = "NULL"
predecir2 rules row
  | null reglasAcuerdas = "NULL"
  | otherwise = getClassRule $ primeraReglaRuleSet sortedRules
  where
    reglasAcuerdas = matchingRules2 rules row
    sortedRules = qs (\r1 r2 -> mejorRegla2 r1 r2 row) reglasAcuerdas

-- ParticionNegCSV
-- toma un csv con un inicio y un fin y devuelve el csv EXCEPTO las líneas entre inico y fin
particionNegCSV :: CSV -> Int -> Int -> CSV
particionNegCSV csv inicio fin
  | inicio < 1 || fin > length csv || inicio > fin = error "Intervalo no válido"
  | otherwise = take (inicio - 1) csv ++ drop fin csv

-- ParticionPosCSV
-- toma un csv con un inicio y un fin y devuelve SOLO las filas del csv entre el inicio y el fin
particionPosCSV :: CSV -> Int -> Int -> CSV
particionPosCSV csv inicio fin
  | inicio < 1 || fin > length csv || inicio > fin = error "Intervalo no válido"
  | otherwise = take (fin - inicio + 1) $ drop (inicio - 1) csv

-- para separar el conjunto de datos en entrenamiento y prueba con una proporción de entrenamiento elegida
-- Por ejemplo, si la proporción es 0,8, habrá un 80% de entrenamiento y un 20% de prueba.
splitData :: Float -> CSV -> (CSV, CSV)
splitData proportion datos
  | proportion < 0.0 || proportion > 1.0 = error "Proportion invalide"
  | otherwise =
    let total = length datos
        inicioEntrenamiento = 1
        finEntrenamiento = floor (proportion * fromIntegral total)
        datos_train = particionPosCSV datos inicioEntrenamiento finEntrenamiento
        datos_test = particionNegCSV datos inicioEntrenamiento finEntrenamiento
    in (datos_train, datos_test)

-- Entrenar modelo
-- escanea los datos y devuelve una lista de las mejores reglas
entrenarModelo :: CSV -> RuleSet
entrenarModelo datos
  | null datos = error "Error cargamiento datos"
  | otherwise =
    let rs = foldl reglasClasificacion [] datos
        rs2 = qs mejorRegla rs
    in [(sop, a, b) | (sop, a, b) <- rs2, sop >= 0] -- si queremos modificar el soporte minimal de una regla

-- Predecir todas las clases de un conjunto de datos
-- toma un modelo y un dataset y devuelve un conjunto de clases previstas
predecirAll :: RuleSet -> CSV -> [String]
predecirAll _ [] = [] 
predecirAll modelo datosPrueba = map (predecir modelo) datosPrueba

-- con predecir2
predecirAll2 :: RuleSet -> CSV -> [String]
predecirAll2 _ [] = [] 
predecirAll2 modelo datosPrueba = map (predecir2 modelo) datosPrueba

-- toma un CSV y devuelve todas las clases reales
getClasseAllRows :: CSV -> [String]
getClasseAllRows [] = []  
getClasseAllRows csv = map getClassRow csv

{-
Para probar nuestro modelo, vamos a utilizar las funciones que hemos creado anteriormente:
para predecir todas las clases del conjunto de datos de prueba y compararlas con las clases reales.
La métrica utilizada para evaluar el modelo será la precisión
(el porcentaje de individuos clasificados correctamente).

Vamos a crear dos funciones:
la primera probará el modelo que utiliza la función predecir y la segunda utilizará la función predecir2.
-}

-- testarModelo
-- Probar la precision del modelo
testarModelo :: RuleSet -> CSV -> Float
testarModelo _ [] = error "Conjunto de datos de prueba vacío"
testarModelo modelo datosPrueba   =
  let clasesReales = getClasseAllRows datosPrueba
      predicciones = predecirAll modelo datosPrueba
      validPairs = zip clasesReales predicciones
      correctas = filter (\(real, pred) -> real == pred && pred /= "NULL") validPairs
      correctasCount = length correctas
  in fromIntegral correctasCount / fromIntegral (length validPairs)

-- testarModelo2
-- con predecir2
testarModelo2 :: RuleSet -> CSV -> Float
testarModelo2 _ [] = error "Conjunto de datos de prueba vacío"
testarModelo2 modelo datosPrueba   =
  let clasesReales = getClasseAllRows datosPrueba
      predicciones = predecirAll2 modelo datosPrueba
      validPairs = zip clasesReales predicciones
      correctas = filter (\(real, pred) -> real == pred && pred /= "NULL") validPairs
      correctasCount = length correctas
  in fromIntegral correctasCount / fromIntegral (length validPairs)






-------------------------------------------------------------------
---------------------- EXAMEN -------------------------------------
-------------------------------------------------------------------
-- Función para mover la columna en la posición especificada a la última posición
moverColumna :: Int -> Row -> Row
moverColumna idx row
  | idx < 0 || idx >= length row = row
  | otherwise = take idx row ++ drop (idx + 1) row ++ [row !! idx]

-- Función para mover la columna en la posición especificada a la última posición para cada fila del CSV
moverColumnaEnCSV :: Int -> CSV -> CSV
moverColumnaEnCSV idx = map (moverColumna idx)

-- Función principal para seleccionar la clase
seleccionarClase :: CSV -> Int -> CSV
seleccionarClase csv idx
  | idx < 0 || idx >= length (head csv) = csv  -- Si el índice no es válido, devolver el CSV sin cambios
  | otherwise = moverColumnaEnCSV idx csv





  
-- Función para seleccionar las columnas especificadas
seleccionarColumnas :: CSV -> [Int] -> CSV
seleccionarColumnas csv indices =
  map (\row -> [row !! idx | idx <- indices]) csv





-- Funcion para hacer crossValidation (devolve la "accuracy" media de los 5 bloques)
crossValidation :: CSV -> Float
crossValidation datos =
  let numFilas = length datos
      bloqueTamano = div numFilas 5
      bloques = [ (i, i + bloqueTamano - 1) | i <- [1, bloqueTamano+1..numFilas], i <= numFilas - bloqueTamano + 1]
      accuracies = map (\(inicio, fin) ->
                         let conjuntoPrueba = particionPosCSV datos inicio fin
                             conjuntoEntrenamiento = particionNegCSV datos inicio fin
                             modelo = entrenarModelo conjuntoEntrenamiento
                         in testarModelo2 modelo conjuntoPrueba
                       ) bloques
  in sum accuracies / 5.0



-- para ver las precisiones de cada bloques : 
crossValidationPrint :: CSV -> IO ()
crossValidationPrint datos =
  let numFilas = length datos
      bloqueTamano = div numFilas 5
      bloques = [ (i, i + bloqueTamano - 1) | i <- [1, bloqueTamano+1..numFilas], i <= numFilas - bloqueTamano + 1]
      accuracies = mapM (\(inicio, fin) -> do
                         let conjuntoPrueba = particionPosCSV datos inicio fin
                             conjuntoEntrenamiento = particionNegCSV datos inicio fin
                             modelo = entrenarModelo conjuntoEntrenamiento
                             accuracy = testarModelo2 modelo conjuntoPrueba
                         putStrLn $ "Accuracy para el bloque " ++ show inicio ++ "-" ++ show fin ++ ": " ++ show accuracy
                         return accuracy
                       ) bloques
  in do
    avgAccuracy <- fmap (\accs -> sum accs / 5.0) accuracies
    putStrLn $ "Accuracy Media " ++ show avgAccuracy









{-
--- EJEMPLO DE CODIGO PARA ENTRENAR Y PROBAR UN MODELO ---
-- Compilar el archivo Haskell -- 
:l reglas_DORE_MARTIN.hs                            

(cab:dat) <- loadCSV "12_crx.csv"                   -- Leer datos
(train,test) = splitData 0.8 dat                    -- Separar datos entre train y test dataset
modelo = entrenarModelo train                       -- Entrenar el modelo sobre el conjunto de train
accuracy = testarModelo modelo test                 -- Probar el modelo sobre el conjuto de test
accuracy2 = testarModelo2 modelo test               -- Probar el modelo2 (predecir2) sobre el conjuto de test
putStrLn $ "Accuracy: " ++ show (accuracy * 100) ++ "%"       -- Imprimir accuracy
putStrLn $ "Accuracy_2: " ++ show (accuracy2 * 100) ++ "%"       -- Imprimir accuracy2

--- IMPRIMIR LAS MEJORES REGLAS ---
rs2 = qs mejorRegla modelo                                     -- ORDENAR EL CONJUNTO DE REGLAS
reglasMejores = [(sop, a, b) | (sop, a, b)<-rs2, sop >= 4 ]    -- OBTENER LAS MEJORES REGLAS
saltoLinea a b = a ++ "\n" ++ b                                -- DEFINE FUNCIÓN PARA MOSTRAR REGLAS
putStrLn $ foldr (saltoLinea.show) "" reglasMejores            -- IMPRIME REGLAS
-}
