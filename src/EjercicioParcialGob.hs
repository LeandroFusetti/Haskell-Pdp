module EjercicioParcialGob where
import PdePreludat

type Bien = (String,Number)
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Number, 
cantidadDeHijos :: Number, bienes :: [Bien] } deriving Show

homero :: Ciudadano
homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink :: Ciudadano
frink = UnCiudadano "Profesor" 12000 1 []
krabappel :: Ciudadano
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns :: Ciudadano
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

type Ciudad= [Ciudadano]
springfield :: Ciudad
springfield = [homero, burns, frink, krabappel] 

diferenciaDePatrimonio:: Ciudad->Number
diferenciaDePatrimonio ciudad = mayorPatri ciudad - menorPatri ciudad

mayorPatri::Ciudad->Number
mayorPatri ciudad = maximum(map patrimonio ciudad)

menorPatri::Ciudad->Number
menorPatri ciudad = minimum (map patrimonio ciudad)

patrimonio:: Ciudadano->Number
patrimonio ciudadano = foldl (+) (sueldo ciudadano)  [x | (_,x)<-bienes ciudadano]

tieneAutoAltaGama:: Ciudadano->Bool
tieneAutoAltaGama ciudadano = (any altaGama.bienes )ciudadano

altaGama:: (String ,Number)->Bool
altaGama (bien, precio)= bien=="auto" && precio>100000

type Medida = Ciudadano ->Ciudadano

auh::Medida
auh ciudadano |patrimonio ciudadano >0= ciudadano{sueldo = sueldo ciudadano + cantidadDeHijos ciudadano *1000}
    |otherwise =ciudadano

impuestoGanancias::Number->Medida
impuestoGanancias minimo ciudadano | sueldo ciudadano >minimo= ciudadano{sueldo= sueldo ciudadano- 0.3*(sueldo ciudadano - minimo)}
    |otherwise  = ciudadano

impuestoAltaGama::Medida
impuestoAltaGama ciudadano | elem "auto" [x | (x,_)<- bienes ciudadano] = ciudadano{sueldo= sueldo ciudadano- 0.1 *head[y | (x,y)<-bienes ciudadano, x == "auto" ]}
    |otherwise  = ciudadano

negociarSueldoProfesion::String->Number->Medida
negociarSueldoProfesion profe porcentaje ciudadano|  profesion ciudadano == profe =ciudadano{sueldo= sueldo ciudadano *(porcentaje/100 +1)}
    |otherwise  = ciudadano

data Gobierno = UnGobierno {años :: [Number], medidas :: [Ciudadano->Ciudadano ]}

gobiernoA :: Gobierno
gobiernoA =  UnGobierno [1999..2003][impuestoGanancias 30000,negociarSueldoProfesion "Profesor"10,negociarSueldoProfesion "Empresario"40,impuestoAltaGama,auh]

gobiernoB :: Gobierno
gobiernoB = UnGobierno [2004..2008] [impuestoGanancias 40000, negociarSueldoProfesion "Profesor" 30, negociarSueldoProfesion "Camionero" 40]

gobernarUnAño::Gobierno->Ciudad->Ciudad
gobernarUnAño gobierno ciudad = map (aplicarMedidasCiudadano gobierno)ciudad

aplicarMedidasCiudadano:: Gobierno->Medida
aplicarMedidasCiudadano gobierno ciudadano = (foldr ( $) ciudadano.medidas) gobierno

--gobernarPeriodoCompleto::Gobierno->Ciudad->Ciudad

--gobernarPeriodoCompleto gobierno ciudad = gobernarUnAño gobierno ciudad

--añosGobierno::Gobierno->Number
--añosGobierno gobierno= (length.años)gobierno
gobernarPeriodoCompleto :: Gobierno -> Ciudad  -> Ciudad
gobernarPeriodoCompleto gobierno ciudad = (foldl (\unaCiudad _ -> gobernarUnAño gobierno unaCiudad)  ciudad. años ) gobierno

distribuyóRiqueza::Gobierno->Ciudad->Bool
distribuyóRiqueza gobierno ciudad= diferenciaDePatrimonio ciudad > (diferenciaDePatrimonio.(gobernarPeriodoCompleto gobierno))ciudad


kane::Ciudadano
kane= UnCiudadano "Empresario" 100000 0 [ ("Roseburt",x) | x<-[5,10..30]]

