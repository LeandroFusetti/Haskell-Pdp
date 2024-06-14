module ParcialClase where
import PdePreludat
--import Text.Show.Functions

data Personaje = Personaje{experiencia::Number,fuerzaBasica::Number,elemento::Elemento} deriving Show

nivel::Personaje->Number
nivel personaje= ceiling((((^2).experiencia) personaje)/(((+1).experiencia) personaje))

type Elemento= Number->Number
type Capacidad= Personaje->Number
capacidadCaza:: Capacidad
capacidadCaza (Personaje _ fuerzaBasica elemento) = elemento fuerzaBasica

lin:: Personaje
lin= Personaje 0 10 espadaOxidada

zelda::Personaje
zelda = Personaje 20 15 baculoDuplicador

espadaOxidada :: Number -> Number
espadaOxidada = (1.2*)
katanaFilosa :: Number -> Number
katanaFilosa = (10+).(0.9*)
sableLambdico :: Number -> Number -> Number
sableLambdico cm = ((1+cm/100)*)
redParadigmatica :: Number -> Number
redParadigmatica = sqrt
baculoDuplicador :: Number -> Number
baculoDuplicador x = x * 2
espadaMaldita :: Number -> Number
espadaMaldita = espadaOxidada.sableLambdico 89


type Alquimista= Personaje ->Personaje

aprendiz:: Alquimista
aprendiz personaje = alterarElemento (2*) personaje

alterarElemento:: Elemento->Alquimista
alterarElemento f personaje = personaje {elemento= f.elemento personaje}

maestroAlquimista::Number->Alquimista
maestroAlquimista años personaje =  alterarElemento (extraPorAnti años).aprendiz $ personaje


extraPorAnti :: Number -> Number -> Number
extraPorAnti 0= id
extraPorAnti años = (*1.1).extraPorAnti (años -1)

estafadores:: Alquimista
estafadores personaje = alterarElemento id personaje

nuevaClaseAlq:: Number->Alquimista
nuevaClaseAlq num personaje= alterarElemento (\num-> 3* num) personaje

listaAlquimistas :: [Alquimista]
listaAlquimistas=[aprendiz,maestroAlquimista 10,estafadores,nuevaClaseAlq 2]

listaSubeCap::Number->Personaje->[Alquimista]->[Alquimista]
listaSubeCap num personaje listaAlquimistas = filter (subeCapacidad num personaje) listaAlquimistas


subeCapacidad::Number->Personaje->Alquimista->Bool
subeCapacidad  num personaje alquimista = ((>num).capacidadCaza.alquimista) personaje 

convieneATodos::Personaje->[Alquimista]->Bool
convieneATodos personaje listaAlquimistas = all (subeCapacidad 20 personaje)listaAlquimistas

data Monstruo = Monstruo{especie::String,resistencia::Number,listaHabilidades::[(String,String)]}

sherck :: Monstruo
sherck= Monstruo "ogro" 10 [("descripcion","magica"),("descripcion","fisica"),("descripcion","pantano")]

sonic::Monstruo
sonic=Monstruo "eriso" 12 [("descripcion","velocidad"),("descripcion","salto"),("descripcion","fisica"),("descripcion","fisica")]

snorlax::Monstruo
snorlax=Monstruo "gordo" 0 [("descripcion","fisica"),("descripcion","fisica")]

chocobo::Monstruo
chocobo=Monstruo "chocobo" 20 [("descripcion","volador"),("descripcion","fisica"),("descripcion","magica")]

perro::Monstruo
perro=Monstruo "animal" 3 [("descripcion","fisica"),("descripcion","fisica"),("descripcion","guardian"),("descripcion","fisica")]

coloso::Monstruo
coloso=Monstruo "gigante" 19 [("descripcion","fisica"),("descripcion","fisica"),("descripcion","escudo"),("descripcion","fisica")]

demonio::Monstruo
demonio=Monstruo "gigante" 25 [("descripcion","fisica"),("descripcion","magica"),("descripcion","magica"),("descripcion","fuego"),("descripcion","latigo")]

esAgresivo::Monstruo->Bool
esAgresivo (Monstruo especie resistencia listaHab )= especie /="animal" && especie /="chocobo" &&resistencia >0 &&cantHabAgresivas listaHab > div (length listaHab) 2 
      
  

cantHabAgresivas::[(String,String)]->Number
cantHabAgresivas listHab = length  $ filter((\x-> x =="magica"|| x=="fisica").snd)listHab

leGana::Personaje->Monstruo->Bool
leGana personaje monstruo = capacidadCaza personaje > resistencia monstruo

mapa::[Monstruo]
mapa=[sherck,sonic,snorlax,chocobo,perro,coloso,demonio]

mapaGana :: [Monstruo]
mapaGana=[sherck,coloso,sonic,snorlax,chocobo,perro]

batalla::Personaje->Monstruo->Personaje
batalla personaje monstruo| leGana personaje monstruo = personaje{experiencia= experiencia personaje +100}
    |otherwise =personaje{experiencia= experiencia personaje -50,elemento= (*0.9).elemento personaje}

aventura::Personaje->[Monstruo]->Personaje
aventura personaje listaMonst= foldl (batalla) personaje (filter(esAgresivo) listaMonst)

monstruoInvencible::Personaje->Alquimista->[Monstruo]->Bool
monstruoInvencible personaje alquimista listaMonst = any (not.leGana (alquimista personaje)) listaMonst