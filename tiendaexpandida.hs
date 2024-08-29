-- Agregar el producto
agregarProducto :: [(String, Double, Int)] -> String -> Double -> Int -> [(String, Double, Int)]
agregarProducto inventario nombre precio cantidad = inventario ++ [(nombre, precio, cantidad)]

-- Actualizar existencias de un producto
actualizarCantidad :: [(String, Double, Int)] -> String -> Int -> [(String, Double, Int)]
actualizarCantidad inventario nombreProducto nuevaCantidad = 
    map (\(nombre, precio, cantidad) -> if nombre == nombreProducto 
                                         then (nombre, precio, nuevaCantidad) 
                                         else (nombre, precio, cantidad)) inventario

-- Eliminar un producto del inventario
eliminarProducto :: [(String, Double, Int)] -> String -> [(String, Double, Int)]
eliminarProducto inventario nombreProducto = filter (\(nombre, _, _) -> nombre /= nombreProducto) inventario

-- Resumen del inventario: total de productos y valor total
resumenInventario :: [(String, Double, Int)] -> (Int, Double)
resumenInventario inventario = (cantidadTotal, valorTotal)
  where
    cantidadTotal = sum [cantidad | (_, _, cantidad) <- inventario]
    valorTotal = sum [precio * fromIntegral cantidad | (_, precio, cantidad) <- inventario]

-- Buscar un producto por su nombre
buscarProducto :: [(String, Double, Int)] -> String -> Maybe (Double, Int)
buscarProducto inventario nombreProducto = 
    case filter (\(nombre, _, _) -> nombre == nombreProducto) inventario of
        [(nombre, precio, cantidad)] -> Just (precio, cantidad)
        _ -> Nothing

-- Aplicar un descuento a todos los productos del inventario
aplicarDescuento :: [(String, Double, Int)] -> Double -> [(String, Double, Int)]
aplicarDescuento inventario descuento 
    | descuento < 0 || descuento > 1 = inventario -- Validar el rango del descuento
    | otherwise = map (\(nombre, precio, cantidad) -> (nombre, precio * (1 - descuento), cantidad)) inventario


main :: IO ()
main = do
    let inventarioInicial = []
    let inventario1 = agregarProducto inventarioInicial "Manzanas" 0.5 100
    let inventario2 = agregarProducto inventario1 "Platanos" 0.3 150
    let inventario3 = actualizarCantidad inventario2 "Manzanas" 120
    let inventario4 = eliminarProducto inventario3 "Platanos" 
    
    let (cantidadTotal, valorTotal) = resumenInventario inventario4
    
    putStrLn $ "Inventario Final: " ++ show inventario4
    putStrLn $ "Total de productos en stock: " ++ show cantidadTotal
    putStrLn $ "Valor total del inventario: " ++ show valorTotal
    
    -- Buscar producto 
    case buscarProducto inventario4 "Manzanas" of
        Just (precio, cantidad) -> putStrLn $ "Manzanas: Precio = " ++ show precio ++ ", Cantidad = " ++ show cantidad
        Nothing -> putStrLn "Producto no encontrado"
    
   --descuento 
    let inventario5 = aplicarDescuento inventario4 0.50
    putStrLn $ "Inventario después del descuento: " ++ show inventario5
