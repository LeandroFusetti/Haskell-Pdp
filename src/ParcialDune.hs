module ParcialDune where
import PdePreludat


data Fremen=Fremen{nombre::String,nivelTolerancia:: Number,titulos::[String],cantidadReconocimientos::Number} deriving Show

type Tribu=[Fremen]

tribu::Tribu
tribu = [stylgar, paulAtreides, chani, jessica, lietKynes, gurneyHalleck]
stylgar :: Fremen
stylgar= Fremen "Stylgar" 150 ["Guia","Domador"] 3

paulAtreides :: Fremen
paulAtreides = Fremen "Paul Atreides" 200 ["Muad'Dib", "Kwizatz Haderach","Domador","Montador de Gusano"] 5

chani :: Fremen
chani = Fremen "Chani" 90 ["Liet's Daughter", "Sietch's Warrior","Domador"] 4

jessica :: Fremen
jessica = Fremen "Lady Jessica" 170 ["Reverend Mother", "Bene Gesserit"] 4

lietKynes :: Fremen
lietKynes = Fremen "Liet-Kynes" 160 ["Planetologist", "Ecologist"] 3

gurneyHalleck :: Fremen
gurneyHalleck = Fremen "Gurney Halleck" 140 ["Warrior", "Mentat"] 3

reconocimiento::Fremen->Fremen
reconocimiento fremen = fremen{cantidadReconocimientos=cantidadReconocimientos fremen +1 }

hayCandidato::Tribu->Bool
hayCandidato tribu = any(posibleElegido)tribu

posibleElegido::Fremen->Bool
posibleElegido fremen= nivelTolerancia fremen >100 && (elem("Domador") .titulos) fremen

hallarElegido::Tribu->Fremen
hallarElegido tribu = masTitulos.filter(posibleElegido)$tribu

masTitulos::Tribu->Fremen
masTitulos [y]= y
masTitulos (x:y:xs)| (length.titulos ) x > (length.titulos ) y  = masTitulos (x:xs)
    |otherwise = masTitulos (y:xs)


data Gusano= Gusano{longitud::Number,hidratacion::Number,descripcion::String} deriving Show

-- Gusanos
gusano1 :: Gusano
gusano1 = Gusano 1000 80 "Gusano arenoso"

gusano2 :: Gusano
gusano2 = Gusano 15 70 "Gusano del desierto"

gusano3 :: Gusano
gusano3 = Gusano 20 60 "Gusano de arena"

gusano4 :: Gusano
gusano4 = Gusano 25 50 "Gusano gigante"

gusano5 :: Gusano
gusano5 = Gusano 30 40 "Gusano de la muerte"

gusano6 :: Gusano
gusano6 = Gusano 35 30 "Gusano devorador"

gusano7 :: Gusano
gusano7 = Gusano 40 20 "Gusano colosal"

gusanos1to4 :: [Gusano]
gusanos1to4 = [gusano1, gusano2, gusano3, gusano4]


gusanos5to7 :: [Gusano]
gusanos5to7 = [gusano5, gusano6, gusano7]

apareoGusano:: Gusano->Gusano->Gusano
apareoGusano gus1 gus2 = Gusano ((/10) (max (longitud gus1)  (longitud gus2))) 0  (descripcion gus1 ++ " - " ++ descripcion gus2)

ritualApareo::[Gusano]->[Gusano]->[Gusano]
ritualApareo _ []= []
ritualApareo  [] _ = []
ritualApareo (x:xs) (y:ys)= apareoGusano x y : ritualApareo xs ys

domarGusanoDeArena::Gusano->Fremen->Fremen
domarGusanoDeArena gus fremen| nivelTolerancia fremen > longitud gus /2 = (modificarNivelTolerancia (+100).agregarTitulo) fremen
    |otherwise =modificarNivelTolerancia (*0.9) fremen

agregarTitulo::Fremen->Fremen
agregarTitulo fremen= fremen{titulos= "Domador":titulos fremen}

modificarNivelTolerancia::(Number->Number)->Fremen->Fremen
modificarNivelTolerancia f fremen = fremen{nivelTolerancia= f.nivelTolerancia $ fremen }

destruirGusano::Gusano->Fremen->Fremen
destruirGusano gus fremen |nivelTolerancia fremen < longitud gus /2 && (elem("Domador").titulos)fremen= modificarNivelTolerancia (+100).aumentarReconocimiento $ fremen
    |otherwise = modificarNivelTolerancia (*0.8) fremen

aumentarReconocimiento:: Fremen->Fremen
aumentarReconocimiento fremen = fremen{cantidadReconocimientos= cantidadReconocimientos fremen +1}

montarGusano::Gusano->Fremen->Fremen
montarGusano gus fremen |elem("Montador de Gusano").titulos $ fremen = modificarNivelTolerancia (+150).aumentarReconocimiento $ fremen
    |otherwise = modificarNivelTolerancia (/2) fremen

realizarMision:: (Gusano->Fremen->Fremen)->Gusano->Tribu->Tribu
realizarMision mision gus tribu = map(mision gus)tribu


misionCambiaElegido ::  (Gusano->Fremen->Fremen) -> Gusano ->Tribu-> Bool
misionCambiaElegido  mision gusano tribu = (nombre.hallarElegido ) tribu /= nombre (hallarElegido (realizarMision mision gusano tribu))