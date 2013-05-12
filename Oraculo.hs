-- Alumnos: Grupo 14
--	Miguel Fagundez / 09-10264
--	Hector Dominguez / 09-10241

-- ** Declaracion del modulo **
module Oraculo
(
Oraculo(..),
crearPrediccion,
crearPregunta,
obtenerCadena,
obtenerEstadisticas
) where 
  
import Data.Maybe

-- ** Declaracion tipo de datos **

data Oraculo = Prediccion  {prediccion :: String}
	     | Pregunta    {pregunta :: String
			   ,positivo :: Oraculo
			   ,negativo :: Oraculo}
	deriving (Show,Read)
	
-- ** FIN Declaracion tipo de datos **

-- ** Funciones de construccion **

crearPrediccion :: String -> Oraculo
crearPrediccion s = Prediccion s

crearPregunta :: String -> Oraculo -> Oraculo -> Oraculo
crearPregunta s o1 o2 = Pregunta s o1 o2

-- ** FIN Funciones de construccion **

-- ** Funciones de acceso **

-- 	Definidas automaticamente por Haskell por la forma de declaracion del tipo

-- ** FIN Funciones de acceso **

-- ** Funciones de Consulta ** 

obtenerCadena :: Oraculo -> String -> Maybe [(String, Bool)]
obtenerCadena (Prediccion s1) s = if s == s1 then Just [] else Nothing
obtenerCadena oraculo str = if list == [] then Nothing else Just list
  where
    list = reverse (buscar oraculo str [])

obtenerEstadisticas :: Oraculo -> (Int,Int,Float)
obtenerEstadisticas o = (minimo, maximo, promedio)
  where 
	todoscaminos = caminos o 
	minimo = minimum todoscaminos
	maximo = maximum todoscaminos
	promedio = fromIntegral (sum (todoscaminos) ) / fromIntegral (length (todoscaminos ) )

-- ** FIN Funciones de Consulta **

-- ** Funciones auxiliares **

-- 	Auxiliares para obtenerCadena
buscar :: Oraculo ->  String ->[(String,Bool)]->[(String, Bool)]
buscar (Pregunta s1 o1 o2) str l = if (esta o1 str || esta o2 str) then
				      if esta o1 str then (buscar o1 str ((s1, True):l) )
				      else (buscar o2 str ((s1, False):l))
				   else [] 
				
buscar (Prediccion _) _ l = l

esta :: Oraculo -> String -> Bool
esta (Prediccion s1) s = (s1 == s)
esta (Pregunta s1 o1 o2) s = esta o1 s || esta o2 s 

-- 	FIN auxiliares para obtenerCadena

-- 	Auxiliares para obtenerEstadisticas

caminos :: Oraculo -> [ Int ]
caminos (Prediccion s) = [ 0 ]
caminos (Pregunta s o1 o2) = map (1+) (caminos o1) ++ map (1+) (caminos o2)

-- 	FIN auxiliares para obtenerEstadisticas

-- ** FIN Funciones auxiliares **

-- ** FIN Declaracion del modulo **
