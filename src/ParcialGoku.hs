
module ParcialGoku where
import PdePreludat

data Guerrero= Guerrero{nombre::String,ki::Number,raza::String,fatiga::Number} deriving Show

goku :: Guerrero
goku= Guerrero "goku" 10000 "saiyan" 0

esPoderoso::Guerrero->Bool
esPoderoso guerrero = ki guerrero >8000|| raza guerrero =="saiyan"

--ejercicios !!
pressBanca:: Guerrero->Guerrero
pressBanca guerrero = modificarFatiga(+100).modificarKi(+90) $ guerrero

modificarKi::(Number->Number)->Guerrero->Guerrero
modificarKi f guerrero= guerrero{ki= f.ki $ guerrero }

modificarFatiga::(Number->Number)->Guerrero->Guerrero
modificarFatiga f guerrero= guerrero{fatiga= f.fatiga $ guerrero}

flexionesDeBrazo::Guerrero->Guerrero
flexionesDeBrazo  guerrero= modificarFatiga(+50) guerrero

esExperimentado::Guerrero->Bool
esExperimentado guerrero= ki guerrero >22000 

saltoCajon::Number->Guerrero->Guerrero
saltoCajon medida guerrero =modificarKi(+ (medida/10)).modificarFatiga (+ (medida/5)) $ guerrero


snatch::Guerrero->Guerrero
snatch  guerrero | esExperimentado guerrero = modificarFatiga(*1.10).modificarKi(*1.05) $ guerrero
    | otherwise = modificarFatiga(+100) guerrero

