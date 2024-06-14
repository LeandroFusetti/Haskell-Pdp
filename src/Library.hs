module Library where
import PdePreludat

{- doble :: Number -> Number
doble numero = numero + numero -}


siguiente :: Number -> Number
siguiente nro = nro +1

doble :: Number -> Number
doble nro | even nro = nro *2
             |otherwise = nro 

imp :: Number -> Number   
imp nro | even nro= nro 
        | otherwise = nro +1

tupla :: (Number,Number)->(Number,Number)
tupla  (pri,sec) =(doble pri, imp sec)

and' :: Bool -> Bool ->Bool
and' cond1 cond2 | cond1 = cond2 --si la condicion1 es verdadera devuelve condicion 2
                |otherwise =False

or' :: Bool -> Bool ->Bool
or' cond1 cond2 | cond1 = True
                
                |otherwise = cond2


or1 :: Bool->Bool->Bool
or1 False False = False
or1 _ _ =True

esMayorA :: Number->Bool
esMayorA nro = doble (siguiente (2 +nro))>10

suma:: Number->Number->Number
suma nro otronro = nro + otronro

data Bebida =Cafe {nombreBebida::String }|
        Gaseosa{sabor::String,azucar::Number}

esEnergizante ::Bebida-> Bool
esEnergizante (Cafe nombre)= nombre=="capuchino"
esEnergizante (Gaseosa "pomelo"cantAzucar) = cantAzucar > 10 
esEnergizante  _ =False

speed ::Bebida
speed=  Gaseosa "pomelo" 50

masAzucar :: Bebida->Bebida
masAzucar speed = speed { azucar =azucar speed *2}

find' ::(a->Bool)-> [a]->a
find' condicion lista = (head.filter condicion) lista

data Politico = Politico {proyectosPresentados :: [String], sueldo :: Number,  edad :: Number } deriving Show 

politicos :: [Politico]
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

puedoAvanzar :: String -> Bool
puedoAvanzar color = color == "verde"

esPar :: Number->Bool
esPar nro = mod nro 2 == 0

type Persona =(String, Number)

laura=("hola",5)

empConP:: String -> Bool
empConP  = (=='p').head

esP :: Char -> Bool
esP = (=='p')

costoEst :: Number-> Number
costoEst tiempo|tiempo ==1 = 2*50
        |otherwise= tiempo *50

pesoPino:: Number ->Number
pesoPino h | h<= 3 = h *3 *100
        | otherwise =h*2*100

esUtil ::Number->Bool
esUtil pesoPino= pesoPino >=400 && pesoPino<=1000 

sirvepino ::Number -> Bool
sirvepino = esUtil.pesoPino 

inversa :: Number ->Number
inversa nro | nro /=0 = 1/nro
       | otherwise = 0

priT :: (Number,Number,Number)->Number
priT (num ,_,_)= num

sumarList :: [Number]->Number
sumarList []=0
sumarList (x:xs)= x + sumarList xs



data Figura = Cuadrado {b::Number,h::Number}| Triangulo{b::Number,h::Number}|Circulo{r::Number} deriving Show

circulo1 :: Figura
circulo1 =  Circulo 4

cuadrado1 :: Figura
cuadrado1= Cuadrado {b=2,h=4}

triangulo1 ::Figura
triangulo1 = Triangulo 2 4

areaFigura::Figura->Number
areaFigura (Cuadrado b h)=b*h
areaFigura (Triangulo b h)=(b*h)/2
areaFigura (Circulo r)= r^2 * pi

circulo2::Figura
circulo2= Circulo 2

cuadrado2::Figura
cuadrado2= Cuadrado 2 3

data Empleado= Comunes{sueldoB::Number,nombre::String}|Jerarquicos{sueldoB::Number,cantGent::Number,nombre::String}

sueldoEmp:: Empleado->Number
sueldoEmp  (Comunes sueldoB _ )  = sueldoB
sueldoEmp  (Jerarquicos sueldoB cant _)= sueldoB + cant *100

type Nombre = String
type Notas = [Number]
data Persona1 = Alumno {name :: Nombre, notas :: Notas} deriving Show

promedioAlumnos::[Persona1]->[(String,Number)]
promedioAlumnos alumnos =map (\alumno->(name alumno,(promedioAlum.notas) alumno)) alumnos

promedioAlum:: Notas->Number
promedioAlum notas =  sum notas/length notas

promedio :: [Number]->Number
promedio numeros= sumarList numeros/length numeros

promedioSinAplazos::[Notas]->Notas
promedioSinAplazos listaNotas= map(promedio.filter(>=6)) listaNotas

promedioConAplazos :: [[Number]] -> [Number]
promedioConAplazos listaNotas = map promedio listaNotas

aprobo::Persona1->Bool
aprobo alum= (all(>5).notas)alum

aprobaron:: [Persona1]->[Nombre]
aprobaron alumnos=  (map name.filter aprobo)alumnos

aprobaronLista:: [Persona1]->[Persona1]
aprobaronLista alumnos = filter aprobo alumnos

productos:: [String]->[Number]->[(String,Number)]
productos prod precios= zip prod precios

productos':: [String]->[Number]->[(String,Number)]
productos' nombres precios= zipWith (\nombre precio->(nombre,precio)) nombres precios

data Flor= Flor{nombreFlor:: String, aplicacion:: String, cantidadDemanda:: Number}deriving Show

rosa :: Flor
rosa= Flor "rosa" "decorativo" 120
jazmin :: Flor
jazmin= Flor "jazmin" "aromatizante" 100
violeta :: Flor
violeta = Flor "violeta" "infusion" 110
orquidea :: Flor
orquidea = Flor "orquidea" "decorativo" 90
flores :: [Flor]
flores = [orquidea,rosa,violeta,jazmin]

maximaFlorSegun::[Flor]->(Flor->Number)->String
maximaFlorSegun listFlores f = (nombreFlor.florMaximaSegun f)listFlores

florMaximaSegun::(Flor->Number)->[Flor]->Flor
florMaximaSegun _ [flor]= flor
florMaximaSegun f(x:xs)|f x> f (florMaximaSegun f xs)=x 
        | otherwise=florMaximaSegun f xs

cantidadDeElementos :: [a] -> Number
cantidadDeElementos lista = foldl (\sem _->sem+1) 0 lista {-elemento de la lista "_"-}

cantidadDeElementos' :: [a] -> Number
cantidadDeElementos' lista = foldr (\_ sem->sem+1) 0 lista

masGastador ::[(String,Number)]->(String,Number)
masGastador (cab:cola)= foldl mayorGasto  cab cola
{-masGastador lista= foldl mayorGasto  (head lista) (tail lista)-}

masGastador' ::[(String,Number)]->(String,Number)
masGastador' (cab:cola)= foldr mayorGasto  cab cola

mayorGasto:: (String,Number)-> (String,Number)-> (String,Number)
mayorGasto tupla1 tupla2 | snd(tupla1) > snd(tupla2) = tupla1
        | otherwise =tupla2

monto ::[(String,Number)]->Number
monto lista=     foldl (\sem (_,gasto)->sem + gasto)  0 lista

monto' ::[(String,Number)]->Number
monto' lista = foldr (\(_,gasto) sem->sem + gasto) 0 lista


type InversionInicial = Number
type Profesionales = [String]

data  Proyecto = Proy {nombres:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show
proyectos = [Proy "red social de arte"  20000 ["ing. en sistemas", "contador"], Proy "restaurante" 5000 ["cocinero", "adm. de empresas", "contador"],Proy "ventaChurros" 1000 ["cocinero"] ]

maximoProySegun:: (Proyecto->Number)->[Proyecto]->Proyecto
maximoProySegun f (proyecto : proyectos)= foldl (maximoSegun f)proyecto proyectos

maximoSegun :: (Proyecto ->Number)-> Proyecto -> Proyecto-> Proyecto
maximoSegun f unProyecto otroProyecto | f unProyecto > f otroProyecto =unProyecto
        | otherwise = otroProyecto
