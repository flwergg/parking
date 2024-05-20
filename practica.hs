import Data.Time.Clock
import Data.Time.Format
import Data.List
import System.IO
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)  -- Agregamos Read aquí

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)

-- Función para formatear el tiempo en un formato legible
formatearTiempo :: NominalDiffTime -> String
formatearTiempo diffTime = 
    let totalSeconds = round diffTime :: Int
        (days, rem1) = totalSeconds `divMod` 86400
        (hours, rem2) = rem1 `divMod` 3600
        (minutes, seconds) = rem2 `divMod` 60
    in show days ++ "d " ++ show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s"

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    writeFile "parqueadero.txt" (unlines (map mostrarVehiculo parqueadero))
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    fileExists <- doesFileExist "parqueadero.txt"
    if not fileExists
        then return []  -- Si el archivo no existe, devolver una lista vacía
        else do
            contenido <- readFile "parqueadero.txt"
            let lineas = lines contenido
            let vehiculos = mapM leerVehiculo lineas
            case vehiculos of
                Just vs -> return vs
                Nothing -> return []  -- Si hay algún problema de parseo, devolver lista vacía
    where
        leerVehiculo :: String -> Maybe Vehiculo
        leerVehiculo linea = readMaybe linea :: Maybe Vehiculo  -- Usar readMaybe en lugar de read

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    placa vehiculo ++ "," ++ show (entrada vehiculo) ++ "," ++ salidaStr
    where
        salidaStr = case salida vehiculo of
            Just tiempo -> show tiempo
            Nothing     -> "N/A"

-- Función para mostrar la información de los vehículos en el parqueadero
mostrarVehiculosEnParqueadero :: [Vehiculo] -> IO ()
mostrarVehiculosEnParqueadero parqueadero = do
    putStrLn "Vehículos en el parqueadero:"
    mapM_ (\v -> putStrLn (mostrarVehiculo v)) parqueadero

-- Función para mostrar y guardar la información de los vehículos en el parqueadero
mostrarYguardarVehiculos :: [Vehiculo] -> IO ()
mostrarYguardarVehiculos parqueadero = do
    mostrarVehiculosEnParqueadero parqueadero
    guardarParqueadero parqueadero

-- Función principal del programa
main :: IO ()
main = do

    -- Crear el archivo de texto parqueadero.txt si no existe
    let fileName = "parqueadero.txt"
    fileExist <- doesFileExist fileName
    if not fileExist
        then do
            putStrLn "Creando archivo parqueadero.txt..."
            writeFile fileName ""
        else return ()
    
    -- Cargar el parqueadero desde el archivo de texto
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero! :D"

    -- Ciclo principal del programa
    cicloPrincipal parqueadero

-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehículos en el parqueadero"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ha ingresado al parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ha salido del parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ formatearTiempo(tiempoTotal)
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            mostrarVehiculosEnParqueadero parqueadero
            cicloPrincipal parqueadero

        "5" -> putStrLn "Gracias por usar el Sistema de Gestión de Parqueadero. ¡Hasta luego! :D"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero
