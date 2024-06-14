module ParcialCarreras where
import PdePreludat

data Auto= UnAuto {color::String,velocidad::Number,distancia::Number}deriving Show

type Carrera= [Auto]

pepe :: Auto
pepe = UnAuto "Rojo" 80 8

roco :: Auto
roco= UnAuto "Verde" 90 12
jose :: Auto
jose = UnAuto "Azul" 120 39

carrera = [pepe,jose,roco] 

estaCerca::Auto->Auto->Bool
estaCerca auto1 auto2 =color auto1 /= color auto2 && abs(distancia auto1 - distancia auto2) <10

vaTranquilo::Auto->Carrera->Bool
vaTranquilo auto listaAutos= vaPrimero auto listaAutos&& (not.all(estaCerca auto))listaAutos

vaPrimero:: Auto->Carrera->Bool
vaPrimero auto listaAutos =  all (\x -> color auto == color x||(distancia x < distancia auto) ) listaAutos

listaSinAuto:: Auto->Carrera->Carrera
listaSinAuto auto carrera= filter((/=color auto).color)carrera 

puesto::Auto->Carrera->Number
puesto auto carrera=    (length(filter ((>distancia auto).distancia)(listaSinAuto auto carrera)))+1

corra::Number->Auto->Auto
corra tiempo auto= auto{distancia= distancia auto + (tiempo * velocidad auto)}

type ModificadorVel= Number->Number

alterarVelocidad:: ModificadorVel->Auto->Auto
alterarVelocidad modificador auto= auto{velocidad =(modificador.velocidad) auto}

