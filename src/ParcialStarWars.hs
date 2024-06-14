module ParcialStarWars where
import PdePreludat


data Nave= Nave{durabilidad::Number,escudo::Number,ataque::Number,poder::Nave->Nave} deriving Show

type Poder=Nave->Nave

tIEFighter :: Nave
tIEFighter=Nave 200 100 50 turbo

xWing :: Nave
xWing= Nave 300 150 100 reparacionEmerg

naveDeDarthVader :: Nave
naveDeDarthVader =Nave 500 300 200 superTurbo
millenniumFalcon :: Nave
millenniumFalcon= Nave 1000 500 50 reparacionEmergYEscudos

cazadorRebelde::Nave
cazadorRebelde = Nave 250 200 50 canionesPlasma

flota::[Nave]
flota= [cazadorRebelde,millenniumFalcon,naveDeDarthVader,xWing,tIEFighter]

superTurbo::Nave->Nave
superTurbo nave=  modificaDurabilidad(-45).turbo.turbo.turbo $ nave

turbo::Nave->Nave
turbo  nave= modificarAtaque (25) nave

modificarAtaque::Number->Poder
modificarAtaque valor nave| valor + ataque nave >=0 = nave {ataque= valor + ataque nave}
    | otherwise = nave {ataque =0 }

modificaDurabilidad::Number->Poder
modificaDurabilidad valor nave|valor + durabilidad nave >=0 = nave {durabilidad =valor + durabilidad nave }
    | otherwise = nave {durabilidad =0 }

modificiarEscudo::Number->Poder
modificiarEscudo valor nave| valor + escudo nave >= 0 =nave{escudo= valor + escudo nave}
    | otherwise = nave {escudo =0 }

reparacionEmerg::Nave->Nave 
reparacionEmerg nave= (  modificarAtaque (-20) .modificaDurabilidad (50) ) nave

reparacionEmergYEscudos::Poder
reparacionEmergYEscudos nave=  modificiarEscudo 100 .reparacionEmerg $ nave

canionesPlasma::Poder
canionesPlasma nave  = modificarAtaque (50) nave

durabilidadTotal::[Nave]->Number
durabilidadTotal flota =  sum.map(durabilidad)$  flota

ataqueEspacial::Nave->Nave->Nave
ataqueEspacial atacante golpeada | (ataque.usarPoder )atacante >  (escudo.usarPoder ) golpeada =  modificaDurabilidad (-((ataque.usarPoder )atacante -  (escudo.usarPoder ) golpeada))golpeada
    |otherwise = golpeada

usarPoder::Nave->Nave
usarPoder nave= (poder nave) nave

fueraDeCombate:: Nave->Bool
fueraDeCombate nave= durabilidad nave ==0

navesDebiles::Nave->Bool
navesDebiles nave= escudo nave <200

navesPeligrosas::Number->Nave->Bool
navesPeligrosas valor nave= ataque nave > valor

--navesQuedariasFueraDeCombate:: Nave->Nave
--navesQuedariasFueraDeCombate nave  ataqueEspacial

type Estrategia = Nave -> Bool

naveDebilPeligrosa :: Estrategia
naveDebilPeligrosa nave = navesDebiles nave && navesPeligrosas 100 nave


mision:: Nave->[Nave]->Estrategia->[Nave]
mision _ [] _ = []
mision nave (x:xs) estrategia | estrategia x=    ataqueEspacial nave x :mision nave xs estrategia
    |otherwise= x : mision nave xs estrategia