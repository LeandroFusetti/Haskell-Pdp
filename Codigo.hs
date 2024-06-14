import Text.Show.Functions ()
import Data.List ()

-- Gianlucca Bolocco

data Personaje = Personaje{
    nombre :: String,
    ki :: Float,
    raza :: String,
    cansancio :: Float,
    personalidadPersonaje :: Personalidad
}deriving(Show)

type Ejercicio = Personaje -> Personaje -- por ahora

modificarKi :: (Float -> Float) -> Personaje -> Personaje
modificarKi modificacion unPersonaje = unPersonaje{ki = modificacion (ki unPersonaje)}

modificarCansancio :: (Float -> Float) -> Personaje -> Personaje
modificarCansancio modificacion unPersonaje = unPersonaje{cansancio = max 0 (modificacion (cansancio unPersonaje))}
                                                                    -- Con max le pongo un "tope" para que no sea negativo
-- 1
gohan :: Personaje
gohan = Personaje "gohan" 10000 "sayajin" 10000 Perezoso

-- 2
esPoderoso :: Personaje -> Bool
esPoderoso unPersonaje = ((>8000) . ki $ unPersonaje) || ((=="sayajin") . raza $ unPersonaje)

-- 3
pressDeBanca :: Ejercicio
pressDeBanca = modificarKi (+ 100) . modificarCansancio (+ 90)

flexionDeBrazo :: Ejercicio -- no habla nada de repeticiones
flexionDeBrazo = modificarCansancio (+50)

saltosAlCajon :: Float -> Ejercicio
saltosAlCajon medida = modificarKi (+ (medida / 10)). modificarCansancio (+ (medida/5))

snatch :: Ejercicio
snatch unPersonaje
    | (>22000) . ki $ unPersonaje = modificarKi (*1.05) . modificarCansancio (*1.1) $ unPersonaje
    | otherwise = modificarCansancio (+100) unPersonaje

-- ==========

data Estado = Exausto | Cansado | Fresco deriving(Show,Eq)

definirEstado :: Personaje -> Estado
definirEstado unPersonaje
    | (> cansancio unPersonaje ) . (*0.72) . ki $ unPersonaje = Exausto
    | (> cansancio unPersonaje ) . (*0.44) . ki $ unPersonaje = Cansado
    | otherwise = Fresco

deltaCansancio :: Personaje -> Personaje -> Float
deltaCansancio pers1 pers2 = cansancio pers2 - cansancio pers1

deltaKi :: Personaje -> Personaje -> Float
deltaKi pers1 pers2 = ki pers2 - ki pers1


realizarEjercicio :: Ejercicio -> Personaje -> Personaje
realizarEjercicio ejercicio unPersonaje
    | definirEstado unPersonaje == Exausto = modificarKi (*0.98) unPersonaje
    | definirEstado unPersonaje == Cansado = modificarKi (+ ( 2 * deltaKi unPersonaje (ejercicio unPersonaje))) . modificarCansancio (+ (4 * deltaCansancio unPersonaje (ejercicio unPersonaje))) $ unPersonaje
    | otherwise = ejercicio unPersonaje -- esta fresco


-- 4

-- armo la rutina para los perezosos intercalando un descanso cada 5m

type Rutina = [Ejercicio]

armarRutina :: Personaje -> Rutina -> Rutina
armarRutina unPersonaje unaRutina
    | (== Sacado) . personalidadPersonaje $ unPersonaje = unaRutina
    | (== Perezoso) . personalidadPersonaje $ unPersonaje = intercalarDescansos (descansar 5) unaRutina
    | otherwise = []


-- Parte teorica : No se podria realizar ya que se quedaria iterando en los infinitos ejercicios
--                 sin converger a ningun valor (en este caso personaje) en especifico, por ende diverge


-- funcion para intercalar descansos (aunque se puede intercalar cualquier otro tipo de ejercicio ;) )
intercalarDescansos :: (Personaje -> Personaje) -> [Ejercicio] -> [Ejercicio]
intercalarDescansos _ [] = []
intercalarDescansos descansar [ejercicio] = [ejercicio]
intercalarDescansos descansar (e:es) = e : descansar : intercalarDescansos descansar es

data Personalidad = Sacado | Perezoso | Tramposo deriving(Show,Eq)

-- 6

descansar :: Float -> Personaje -> Personaje
descansar minutos = modificarCansancio (+ ( -1 * descanso minutos))

descanso :: Float -> Float
descanso 0 = 0
descanso n = n + descanso (n-1)

-- Funcion Realizar rutina
realizarRutina :: Personaje -> [Ejercicio] -> Personaje
realizarRutina unPersonaje unaRutina = foldr realizarEjercicio unPersonaje (armarRutina unPersonaje unaRutina)

-- 7

listaMinutos :: Personaje -> Float -> [Float]
listaMinutos unPersonaje n
    | (/= 0) . cansancio $ unPersonaje = n : listaMinutos (descansar n unPersonaje) (n+1)
    | otherwise = []

cantidadOptimaDeMinutos :: Personaje -> Int  -> Int
cantidadOptimaDeMinutos unPersonaje n = (2*n) * length (listaMinutos unPersonaje 0)