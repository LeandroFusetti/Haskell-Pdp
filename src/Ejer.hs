module Ejer where
import PdePreludat

data Animal= Raton {nombre :: String, edad :: Number, peso :: Number,enfermedades :: [String]} deriving Show
 
modificarEdad:: (Number->Number) -> Animal -> Animal
modificarEdad f unAnimal= unAnimal{edad = (f.edad)unAnimal}

modificarNombre:: (String->String)-> Animal -> Animal
modificarNombre f unAnimal= unAnimal{nombre= (f.nombre)unAnimal}

modificarPeso:: (Number->Number)->Animal -> Animal
modificarPeso f unAnimal = unAnimal {peso= (f.peso)unAnimal}

modificarEnfermedades:: ([String]->[String])->Animal -> Animal
modificarEnfermedades f unAnimal = unAnimal {enfermedades= (f.enfermedades)unAnimal}

hierbaBuena :: Animal -> Animal
hierbaBuena unAnimal=modificarEdad sqrt unAnimal

hierbaVerde:: String-> Animal-> Animal
hierbaVerde unaEnf unAnimal = modificarEnfermedades (filter(/=unaEnf))  unAnimal

alcachofa:: Animal-> Animal
alcachofa unAnimal = modificarPeso nuevoPeso  unAnimal

nuevoPeso:: Number->Number
nuevoPeso peso | peso> 2 = peso *0.9
    | otherwise= peso *0.95

data Ciudad= Ciudad {nombres:: String, anioFundacion:: Number, atraccionesPri:: [String], costoDeVida ::Number}  deriving Show

city:: Ciudad
city = Ciudad "Madrid" 1800 [] 140

valorCiudad:: Ciudad->Number
valorCiudad ciudad |anioFundacion ciudad <1800 = ((5*).(1800-).anioFundacion) ciudad
                   | ((0==).(length.atraccionesPri)) ciudad= ((*2).costoDeVida) ciudad
                   | otherwise =((3*).costoDeVida) ciudad

