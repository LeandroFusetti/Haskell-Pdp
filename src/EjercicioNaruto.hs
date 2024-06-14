module EjercicioNaruto where
import PdePreludat


data Ninja= UnNinja {nombre::String,herramientas::[(String,Number)],jutsus::[Jutsu],rango::Number} deriving Show
data Mision= UnaMision{cantNinjas::Number,rangoReq::Number,ninjasEnemigos::[Ninja],recompensa::Herramienta}
type Jutsu= Mision->Mision
naruto :: Ninja
naruto = UnNinja "Naruto" [("kunai", 30), ("shuriken", 20), ("selloExplosivo", 5), ("bombaDeHumo", 15)] [fuerzaDeUnCentenar, clonesDeSombra 2] 3

sasuke :: Ninja
sasuke = UnNinja "Sasuke Uchiha" [("kunai", 25), ("shuriken", 40), ("selloExplosivo", 20), ("bombaDeHumo", 15)] [fuerzaDeUnCentenar] 5

sakura :: Ninja
sakura = UnNinja "Sakura Haruno" [("kunai", 20), ("shuriken", 10), ("selloExplosivo", 10), ("bombaDeHumo", 5)] [ ] 4

kakashi :: Ninja
kakashi = UnNinja "Kakashi Hatake" [("kunai", 30), ("shuriken", 25), ("selloExplosivo", 20), ("bombaDeHumo", 25)] [ clonesDeSombra 1] 6

shikamaru :: Ninja
shikamaru = UnNinja "Shikamaru Nara" [("kunai", 15), ("shuriken", 10), ("selloExplosivo", 5), ("bombaDeHumo", 10)] [ clonesDeSombra 1] 3

hinata :: Ninja
hinata = UnNinja "Hinata Hyuga" [("kunai", 15), ("shuriken", 10), ("selloExplosivo", 5), ("bombaDeHumo", 10)] [] 2

-- Ninjas Enemigos

zetsu::Ninja
zetsu= UnNinja "Zetsu " [][]600
orochimaru :: Ninja
orochimaru = UnNinja "Orochimaru" [("kunai", 20), ("shuriken", 30), ("selloExplosivo", 20), ("bombaDeHumo", 20)] [fuerzaDeUnCentenar, clonesDeSombra 1] 700

itachi :: Ninja
itachi = UnNinja "Itachi Uchiha" [("kunai", 25), ("shuriken", 40), ("selloExplosivo", 10), ("bombaDeHumo", 15)] [fuerzaDeUnCentenar, clonesDeSombra 1] 600

kisame :: Ninja
kisame = UnNinja "Kisame Hoshigaki" [("kunai", 15), ("shuriken", 20), ("selloExplosivo", 15), ("bombaDeHumo", 20)] [fuerzaDeUnCentenar, clonesDeSombra 1] 555

deidara :: Ninja
deidara = UnNinja "Deidara" [("kunai", 10), ("shuriken", 20), ("selloExplosivo", 50), ("bombaDeHumo", 10)] [fuerzaDeUnCentenar, clonesDeSombra 1] 200

hidan :: Ninja
hidan = UnNinja "Hidan" [("kunai", 20), ("shuriken", 10), ("selloExplosivo", 10), ("bombaDeHumo", 10)] [fuerzaDeUnCentenar, clonesDeSombra 1] 55

misionS :: Mision
misionS=UnaMision 6 3 [hidan,deidara,kisame,itachi,orochimaru] ("kunai", 11)
misionA :: Mision
misionA=UnaMision 3 2 [hidan,deidara,kisame] ("shuriken", 5)

misionC::Mision
misionC=UnaMision 1 2 [hidan] ("bombaDeHumo", 3)

misionD::Mision
misionD=UnaMision 1 2 [deidara] ("kunai", 10)


equipoA :: [Ninja]
equipoA=[naruto,sasuke,sakura,kakashi,shikamaru,hinata]

equipoB::[Ninja]
equipoB= [kakashi,shikamaru,hinata]

equipoC :: [Ninja]
equipoC=[naruto,sasuke,sakura]


type Herramienta= (String,Number)

obtenerHerramienta::Herramienta->Ninja->Ninja
obtenerHerramienta (nombre,cant)  ninja |  cantidadHerramientasTotales ninja + cant <=100 = reponerHerramientas (nombre,cant) ninja
   |otherwise = reponerHerramientas (nombre, 100 -(cantidadHerramientasTotales ninja)) ninja

--cantidadAReponer::Number->Herramienta->Ninja->Number
--cantidadAReponer cant herramienta ninja= (cantidadHerramienta herramienta ninja) +cant

cantidadHerramientasTotales::Ninja->Number
cantidadHerramientasTotales ninja= foldl(\ sem x->sem + snd x)0.herramientas $ ninja

--cantidadHerramienta::Herramienta->Ninja->Number
--cantidadHerramienta (nom,cant) ninja= sum[cantidad | (nombre,cantidad)<-herramientas ninja,nombre==nom]

reponerHerramientas::Herramienta->Ninja->Ninja
reponerHerramientas (nom,cant) ninja= ninja{herramientas= (nom,cant):herramientas ninja}

usarHerramienta::Herramienta->Ninja->Ninja
usarHerramienta herramienta ninja =ninja{herramientas= filter(\ x ->fst x /= fst herramienta).herramientas $ ninja}

esDesafiante::[Ninja]->Mision->Bool
esDesafiante ninjas mision = any((rangoReq mision >).rango) ninjas && (length.ninjasEnemigos) mision >= 2

esCopada::Mision->Bool
esCopada  mision= elem (recompensa mision) recompensaCopada


recompensaCopada::[Herramienta]
recompensaCopada=[("bombaDeHumo",3),("shuriken",5),("kunai",14)]

esFactible::[Ninja]->Mision->Bool
esFactible ninjas mision = (not.esDesafiante ninjas)  mision && (length(ninjas) >= cantNinjas mision ||    500 < foldl (\ sem x-> sem + cantidadHerramientasTotales x ) 0 ninjas)

fallarMision::[Ninja]->Mision->[Ninja]
fallarMision ninjas mision = map(modificarRango (abs.(2-))).filter(\x-> tienenRangoRecomendado x mision )$ninjas

tienenRangoRecomendado::Ninja->Mision->Bool
tienenRangoRecomendado ninja mision= rango ninja >= rangoReq mision

modificarRango::(Number->Number)->Ninja->Ninja
modificarRango f ninja = ninja{rango= (f.rango )ninja }

cumplirMision::[Ninja]->Mision->[Ninja]
cumplirMision ninjas mision = map (obtenerHerramienta (recompensa mision)).map(modificarRango (1+)) $ ninjas

clonesDeSombra::Number->Mision->Mision
clonesDeSombra cant mision =mision{cantNinjas= cantNinjas mision -cant}

fuerzaDeUnCentenar::Jutsu
fuerzaDeUnCentenar mision= mision{ninjasEnemigos= filter(\x -> rango x >500 ).ninjasEnemigos$ mision}

ejecutarMision::[Ninja]->Mision->Mision
ejecutarMision ninjas mision = foldl(ejecutarJutsu)mision ninjas

ejecutarJutsu:: Mision->Ninja->Mision
ejecutarJutsu mision ninja= foldr($)mision (jutsus ninja)

seCumple::Mision->[Ninja]->[Ninja]
seCumple mision ninjas | esCopada mision || esFactible ninjas mision = cumplirMision ninjas mision
    |otherwise = fallarMision ninjas mision

granGuerraNinja :: Mision
granGuerraNinja=UnaMision 100000 100 [ agregarSufijo (show suf) zetsu |suf<-[1..]]("Abanico de Madara",1)


agregarSufijo::String->Ninja->Ninja
agregarSufijo suf ninja = ninja{nombre= nombre ninja ++ suf}
