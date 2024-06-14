module ParcialJujutsu where
import PdePreludat
import GHC.Base (BCO)

data Hechicero =Hechicero{antiguedad::Number,clan::String,grado::Number}deriving Show

nobara= Hechicero 0 "Kugasaki" 3

satoru= Hechicero 15 "Gojo" 0

maki= Hechicero 3 "Zenin" 4

yuki= Hechicero 0 "Itadori" 1

equipo1::[Hechicero]
equipo1=[nobara,satoru,maki,yuki]
equipo3::[Hechicero]
equipo3=[satoru,maki]

equipo2::[Hechicero]
equipo2=[nobara,maki,yuki]

tieneExperiencia::Hechicero->Bool
tieneExperiencia hechicero = antiguedad hechicero >1

estaPreparado::[Hechicero]->Bool
estaPreparado grupo= length grupo >3

subirGrado::Hechicero->Hechicero
subirGrado hechicero | grado hechicero/=0 = hechicero{grado= grado hechicero -1}
    |otherwise =hechicero

esPrestigioso:: Hechicero->Bool
esPrestigioso hechicero= elem(clan hechicero) prestigio

prestigio::[String]
prestigio=["Zenin","Gojo","Kano"]

esEquipoInvensible::[Hechicero]->Bool
esEquipoInvensible equipo=  any((0==).grado) equipo

esFavorito::[Hechicero]->Bool
esFavorito equipo = all(esPrestigioso)equipo

sonExpertos::[Hechicero]->[Hechicero]
sonExpertos equipo = filter(tieneExperiencia)equipo

esCapazDeHacerFrenteACualquierMaldicion::[Hechicero]->Bool
esCapazDeHacerFrenteACualquierMaldicion equipo = esEquipoInvensible equipo || estaPreparado equipo



powerUpEquipo::[Hechicero]->[Hechicero]
powerUpEquipo equipo = map(subirGrado)equipo

elegidoParaMision::(Hechicero->Number)->Hechicero->Hechicero->Hechicero
elegidoParaMision f hechicero1 hechicero2 | f hechicero1 > f hechicero2 = hechicero1
    |otherwise = hechicero2
    
nivelTryhard::Hechicero->Number
nivelTryhard hechicero= 1/(grado hechicero +1)

nivelBurocratico::Hechicero->Number
nivelBurocratico hechicero= length.clan $ hechicero

nivelIntimidante::Hechicero->Char
nivelIntimidante hechicero = maximum .clan $ hechicero

nivelSigilo::Hechicero->Number
nivelSigilo hechicero = antiguedad hechicero *6