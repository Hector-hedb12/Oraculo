-- Alumnos: Grupo 14
--	Miguel Fagundez / 09-10264
--	Hector Dominguez / 09-10241

-- Importamos lo necesario
module Haskinator (main) where 
  
import Oraculo
import Data.Maybe
import Char
import System.Exit
import System.IO

-- Decide cual opcion a ejecutar
decidirOpcion :: String -> Maybe Oraculo -> IO (Maybe Oraculo)
decidirOpcion "1" o = limpiarOraculo o 		-- crear un oraculo nuevo
decidirOpcion "2" o = predecir o		-- predecir
decidirOpcion "3" o = persistir o		-- persistir
decidirOpcion "4" o = leerArchivo o		-- cargar
decidirOpcion "5" o = consultarpregunta o	-- consultar pregunta crucial
decidirOpcion "6" o = consultarestadisticas o	-- consultar estadisticas
decidirOpcion "7" o = salir			-- salir de la ejecucion
decidirOpcion _ o = do 
		    print "Esa opcion no es valida" 
		    return (o)

-- ** Opcion 1 **
-- Nos devuelve un oraculo nuevo, simulando un oraculo vacio
limpiarOraculo ::  Maybe Oraculo -> IO (Maybe Oraculo)
limpiarOraculo o = return ( Nothing ) 
  
-- ** Opcion 2 **
-- Comienza a predecir dado un oraculo
predecir :: Maybe Oraculo -> IO ( Maybe Oraculo )
predecir o =
  if (esVacio o) then do 
			print "El oraculo es vacio, ingrese una prediccion"
			respuesta <- getLine
			return (Just (Prediccion respuesta))
  else (case o of
	    Just (Pregunta str o1 o2 ) -> do
					print str
					resp <- getLine
					validacion <- validarRespuesta(resp)
					if validacion 
						then do 
					  o11 <- predecir(Just o1)
					  return (Just (Pregunta str (fromJust o11) o2))
						else do 
					  o22 <- predecir(Just o2)
					  return (Just (Pregunta str o1 (fromJust o22)))
				
				    
	    Just (Prediccion str ) -> do 
				    print "Usted piensa en: "
				    print str
				    print "Es correcto?" 
				    resp <- getLine
				    validacion <- validarRespuesta(resp)
				    if validacion 
					then return ( o )
				    	else do 
				      print "Ingrese prediccion correcta"
				      respCorrecta <- getLine
				      print "Ingrese pregunta que distinga prediccion"
				      pregCorrecta <- getLine
				      return ( Just (Pregunta pregCorrecta (Prediccion respCorrecta) (Prediccion str)))
	)
  
-- ** Opcion 3 **
-- Guarda en un archivo el oraculo dado
persistir :: Maybe Oraculo -> IO ( Maybe Oraculo )
persistir o = do 
  print "Ingrese nombre del archivo :"
  archivo <- getLine
  writeFile archivo ( toString o )
  return (o)

-- ** Opcion 4 **
-- Carga de un archivo dado el oraculo 
leerArchivo :: Maybe Oraculo -> IO (Maybe Oraculo)
leerArchivo o = do 
     print "Ingrese el nombre de archivo :"
     archivo <- getLine 			-- Toma la linea ingresada
     handle <- openFile archivo ReadMode 	-- Abre el archivo para lectura
     contents <- hGetContents handle 		-- GUarda el contenido
     
     return ( Just (read contents ) )
-- ** Opcion 5 **    
-- Consulta la pregunta que lleva a decidir por una prediccion o por la otra
consultarpregunta :: Maybe Oraculo-> IO (Maybe Oraculo)
consultarpregunta o = do
  print "Primera cadena: "
  s1 <- getLine
  print "Segunda cadena: "
  s2 <- getLine
  if not (s1 == s2) 
	then
    	if not (esVacio o) 
		then
	      let maybeX = (obtenerCadena (fromJust o) (s1) )
		  maybeY = (obtenerCadena (fromJust o) (s2) )
	      in case maybeX of
		    Nothing -> print "Consulta no valida"
		    Just listaX -> case maybeY of 
			 Nothing -> print "Consulta no valida"
			 Just listaY -> print (buscarCoinc (listaX) (listaY))
    		else print "No hay Oraculo"
  	else print "Son las mismas predicciones"
  return (o)

-- ** Opcion 6 **
-- Da las estadisticas de un Oraculo
consultarestadisticas :: Maybe Oraculo -> IO(Maybe Oraculo)
consultarestadisticas o = do
  if not (esVacio o) then do
			putStr "(Minimo, Maximo, Promedio)= " 
			print (obtenerEstadisticas (fromJust o) )
		     else print "No hay Oraculo"
  return (o)

 -- ** Opcion 7 **
 -- Opcion creada para poder salir del programa de manera limpia
salir :: IO (Maybe Oraculo) 
salir = exitSuccess

-- Menu
-- Funcion que permite interactuar al usuario con el programa
menu :: Maybe Oraculo -> IO ()
menu  o = do
  putStrLn "1. Crear un oraculo nuevo"
  putStrLn "2. Predecir" 
  putStrLn "3. Persistir"
  putStrLn "4. Cargar"
  putStrLn "5. Consultar Pregunta Crucial"
  putStrLn "6. Consultar Estadisticas "
  putStrLn "7. Salir de Haskinator" 

  c <- getLine
  o <- decidirOpcion (c) o
  menu o
  
  
-- Main
main :: IO () 
main =  menu Nothing


-- ** Funciones auxiliares **

-- Funcion que devuelve el string (o representacion) de un oraculo
toString :: Maybe Oraculo -> String
toString (Just o) = show o
toString (Nothing) = ""

-- Funcion que dado un oraculo devuelve si es vacio o no
esVacio :: Maybe Oraculo -> Bool
esVacio (Just o) = False
esVacio (Nothing) = True


-- Funcion que valida que una respuesta sea valida
validarRespuesta :: String -> IO ( Bool )
validarRespuesta resp
  | resp == "si" = return True 
  | resp == "no" = return False
  | otherwise = do
		print "Ingrese respuesta con formato correcto: si/no "
		nuevaResp <- getLine
		validarRespuesta(nuevaResp)

-- Funcion auxiliar de la opcion 5
-- Busca el primer elemento distinto en dos listas
buscarCoinc :: [(String,Bool)] ->  [(String, Bool)] -> String
buscarCoinc (x:xs) (y:ys)  = if x == y then buscarCoinc xs ys 
					 else fst x 
-- ** FIN Funciones auxiliares **
