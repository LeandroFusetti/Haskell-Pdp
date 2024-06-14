module EjerParc where
import PdePreludat
{-

--Ejercicio Parcial

type Bien = (String,Number)
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Number, 
cantidadDeHijos :: Number, bienes :: [Bien] } deriving Show

homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

type Ciudad= [Ciudadano]
springfield :: [Ciudadano]
springfield = [homero, burns, frink, krabappel] 

diferenciaDePatrimonio:: Ciudad->Number
diferenciaDePatrimonio ciudad =   (patrimonio.ciudadanoSegun mayorPatrimonio) ciudad -(patrimonio.ciudadanoSegun menorPatrimonio) ciudad

ciudadanoSegun:: (Ciudadano->Ciudadano->Ciudadano)-> Ciudad ->Ciudadano
ciudadanoSegun f ciudad = foldl1 f ciudad

patrimonio:: Ciudadano->Number
patrimonio  ciudadano= foldl (\sem (_,bien)-> sem + bien) (sueldo ciudadano) (bienes ciudadano)

mayorPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
mayorPatrimonio ciudadano1 ciudadano2 | patrimonio ciudadano1 > patrimonio ciudadano2 =ciudadano1
    |otherwise = ciudadano2

menorPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
menorPatrimonio ciudadano1 ciudadano2 | patrimonio ciudadano1 < patrimonio ciudadano2 =ciudadano1
    |otherwise = ciudadano2    

tieneAutoAltaGama:: Ciudadano->Bool
tieneAutoAltaGama ciudadano = any (altaGama).bienes$ciudadano 

altaGama:: (String ,Number)->Bool
altaGama ("auto", valor)= valor>100000 
altaGama _ =False

type Medida = Ciudadano ->Ciudadano

auh:: Medida
auh ciudadano = aplicarMedidaSegun (patrimonio ciudadano < 0) (modificarSueldo ((incremento.cantidadDeHijos)ciudadano) )ciudadano 

aplicarMedidaSegun:: Bool -> (Ciudadano -> Ciudadano)->Ciudadano -> Ciudadano
aplicarMedidaSegun condicion f ciudadano | condicion = f ciudadano
    |otherwise = ciudadano

modificarSueldo:: Number -> Ciudadano -> Ciudadano
modificarSueldo cantidad ciudadano = ciudadano {sueldo = sueldo ciudadano + cantidad}

incremento ::Number-> Number
incremento cantHijos = cantHijos * 1000

impuestoGanancias :: Number-> Medida
impuestoGanancias minimo ciudadano = aplicarMedidaSegun (sueldo ciudadano > minimo) (modificarSueldo (diferencia minimo (sueldo ciudadano))) ciudadano

diferencia :: Number -> Number -> Number
diferencia minimo sueldo = (minimo - sueldo)* 0.3

impuestoAltaGama :: Medida
impuestoAltaGama ciudadano = aplicarMedidaSegun (tieneAutoAltaGama ciudadano)(modificarSueldo ((impuesto.bienes)ciudadano) )ciudadano

impuesto :: [Bien]-> Number
impuesto bienes = (*(-0.1)).snd.head.filter altaGama $ bienes

negociarSueldoProfesion :: String -> Number-> Medida
negociarSueldoProfesion unaProfesion porcentaje ciudadano = aplicarMedidaSegun((==unaProfesion).profesion $ ciudadano) (modificarSueldo (aumento porcentaje (sueldo ciudadano))) ciudadano

aumento:: Number-> Number -> Number
aumento porcentaje sueldo = (sueldo * porcentaje)/100

data Gobierno = UnGobierno {años :: [Number], medidas :: [Medida]}

--funcion constante
gobiernoA :: Gobierno
gobiernoA = UnGobierno [1999..2003][impuestoGanancias 30000, negociarSueldoProfesion "Profesor" 10,negociarSueldoProfesion "Empresario" 40, impuestoAltaGama, auh]

gobiernoB :: Gobierno
gobiernoB = UnGobierno [2004..2008][impuestoGanancias 40000,negociarSueldoProfesion "Profesor" 30,negociarSueldoProfesion "Camioneros" 40]

gobernarUnAño :: Gobierno -> Ciudad -> Ciudad
gobernarUnAño gobierno ciudad = map (aplicarMedidas gobierno) ciudad 

aplicarMedidas:: Gobierno -> Ciudadano -> Ciudadano
aplicarMedidas gobierno ciudadano = foldl  (flip ($)) ciudadano. medidas $ gobierno

gobernarPeriodoCompleto :: Gobierno -> Ciudad -> Ciudad
gobernarPeriodoCompleto gobierno ciudad = foldl (\unaCiudad _ -> gobernarUnAño gobierno unaCiudad)ciudad. años $ gobierno

--distribuyoRiqueza::Gobierno -> Ciudad -> Bool
--distribuyoRiqueza gobierno ciudad= diferenciaDePatrimonio ciudad > (diferenciaDePatrimonio gobierno)

kane = UnCiudadano "Empresario" 100000 0 [("Rosebud",valor)| valor  <- [5,10..]]

-}