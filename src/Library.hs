
module Library where
import PdePreludat
import Data.Char(toUpper)  --para poder usar la conversion de mayusculas en el punto 2) 
import Data.Char(isUpper) --para poder usar punto 3) nos dice si una letra es mayuscula

doble :: Number -> Number --no dar bola a esto 
doble numero = numero + numero --no dar bola a esto 

---------------------------------------------------------------------------------------------
type Objetos= Barbaro->Barbaro

data Barbaro= Barbaro{
    nombre :: String,
    fuerza :: Number,
    --porque me dice que tiene habilidadES, osea mas de una=> tiene una lista de habilidades
    habilidades :: [String],
    --xq me dice que tiene objetoS, osea mas de uno => tiene una lista de objetos
    objetos :: [Objetos]
} deriving Show


dave = Barbaro{
    nombre= "Dave",
    fuerza= 100,
    habilidades= ["tejer","escribirPoesia"],
    objetos = [ardilla]
    }


--1)---------------------------------------------------------------------------------------------------

--espada :: Number -> Barbaro -> Barbaro
espada :: Number -> Objetos
espada peso barbarian= barbarian{fuerza= (2*peso) + fuerza barbarian}

amuletosMisticos :: String->Objetos
amuletosMisticos habilidad barbarian = barbarian{habilidades= habilidades barbarian ++ [habilidad] }

varitasDefectuosas :: String -> Objetos
varitasDefectuosas habilidad barbarian = barbarian{habilidades= habilidades barbarian ++ [habilidad], objetos= []}

ardilla :: Objetos
ardilla = id 

--compongo ambos objetos y asi mediante la composicion obtengo otro objeto 
cuerda :: Objetos -> Objetos -> Objetos
cuerda unObjeto otroObjeto = unObjeto.otroObjeto
--cuerda' unObjeto otroObjeto = (.) unObjeto otroObjeto    // Este es con notacion prefija
--cuerda'' = (.)                                          //Este es con notacion point free 

--2)---------------------------------------------------------------------------------------------------

megafono :: Objetos
megafono barbarian = barbarian{habilidades= ((ponerEnMayusculas.concatenar). habilidades) barbarian}


concatenar :: [String]->[String]
concatenar unasHabilidades = [concat unasHabilidades] 
--pongo los corchetes [concat habilidades] aca xq como en el tipo puse q me duelve una lista de [String], 
--entonces lo encierro en una lista a todas las habilidades concatenadas q pasan a ser 1 unico elemento 
--concatenar habilidades = concat habilidades  //sin corchetes no funciona, xq no estaria devolviendo aca una lista de strings, solo un string concatenado sin estar dentro de una lista

ponerEnMayusculas :: [String]->[String]
ponerEnMayusculas unasHabilidades= map (map toUpper) unasHabilidades

megafonoBarbarico :: Objetos
--ESTO ESTA MAL!!! = megafonoBarbarico barbarian = barbarian{objetos= [ardilla,cuerda,megafono]}
megafonoBarbarico = cuerda ardilla megafono 
--vemos que la cuerda recibe dos objetos: ardilla y megafono

--3)-------------------------------------------------------------------------------------

---ESTO ESTA MAL!! type Evento = String

type Evento= Barbaro->Bool --xq nos dice que en cada evento un barbaro sobrevive o no al evento (sobrevive sabemos q yaes de tipo Bool)
type Aventura = [Evento] --xq dice que se compone de uno o mas eventos


--ESTA MAL SI PLANTEO ESTO: 
-- invasionDeSuciosDuendes habilidad barbarian= map (sobrevive habilidad) habilidades $ barbarian 
--sobrevive :: Barbaro->Bool
---sobrevive habilidad= habilidad == "Escribir poesia atroz"

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbarian= elem "Escribir poesia atroz" (habilidades barbarian)
--invasionDeSuciosDuendes' barbarian= (elem "Escribir poesia atroz".habilidades) barbarian  //Este es con composicion 

cremalleraDelTiempo :: Evento 
cremalleraDelTiempo barbarian = (not.tienePulgares.nombre) barbarian


tienePulgares :: String -> Bool
--Por pattern matching hago: 
tienePulgares "Faffy" = False 
tienePulgares "Astro"= False 
tienePulgares _= True  



saqueo:: Barbaro->Bool
saqueo barbarian = elem "robar" (habilidades barbarian) && fuerza barbarian >= 80

--Otra forma de hacer lo mismo pero mejor, porque delegamos el problema en 2 funciones aparte: 
--saqueo' barbarian = tieneHabilidad "robar" barbarian && esFuerte barbarian

--tieneHabilidad :: String-> Barbaro->Bool
--tieneHabilidad' habilidad barbarian= (elem habilidad.habilidades) barbarian
--tieneHabilidad'' habilidad = elem habilidad.habilidades  //Este es con aplicacion parcial

--esFuerte:: Barbaro-> Bool 
--esFuerte barbarian = fuerza barbarian > 80 
--esFuerte' barbarian = ((>80).fuerza) barbarian 
--esFuerte'' = (>80).fuerza   //Este es con aplicacion parcial 

gritoDeGuerra :: Evento
gritoDeGuerra barbarian= poderGritoDeGuerra barbarian >= cantLetrasHabilidades barbarian


poderGritoDeGuerra :: Barbaro->Number 
poderGritoDeGuerra barbarian = (*4).length.objetos $ barbarian 
--version mejor con aplicacion parcial: 
--poderGritoDeGuerra  = (*4).length.objetos 


cantLetrasHabilidades:: Barbaro -> Number
cantLetrasHabilidades   =  sum.map length.habilidades 

caligrafia :: Evento
caligrafia barbarian= all tieneMasDe3VocalesYEmpiezaConMayuscula (habilidades barbarian)


tieneMasDe3VocalesYEmpiezaConMayuscula :: String->Bool
tieneMasDe3VocalesYEmpiezaConMayuscula habilidad= tieneMasDe3Vocales habilidad && empiezaConMayuscula habilidad 

tieneMasDe3Vocales :: String -> Bool 
tieneMasDe3Vocales habilidad = (>3).length.filter esVocal $ habilidad 

--Por pattern matching: 
esVocal :: Char->Bool
esVocal 'a'= True 
esVocal 'e'= True
esVocal 'i'= True
esVocal 'o'= True
esVocal 'u'= True
esVocal  _ = False

empiezaConMayuscula :: String -> Bool 
empiezaConMayuscula= isUpper.head --Con aplicacion parcial 
--empiezaConMayuscula habilidad = isUpper.head $ habilidad  //peor version porque repite logica 


--Esta es una forma de hacerlo pero no respeta la consigna porque nos dice que no repitamos logica, y aca estamos repitiendo la logica de lambda:
--ritualDeFechorias::[Evento]-> Evento 
--ritualDeFechorias eventos barbarian = any (\evento-> evento barbarian) eventos
--sobrevivientes :: [Barbaro]->Aventura->[Barbaro]  
--sobrevivientes barbarians aventura= filter(\barbarian -> all (\evento-> evento barbarian) aventura) barbarians


--Recibe una lista de eventos xq esta recibiendo varias pruebas y devuelve un Evento xq alfin y al cabo 
--devuelve un barbaro si pasa alguna de las pruebas o no. 
ritualDeFechorias::[Evento]-> Evento 
ritualDeFechorias eventos barbarian = pasaUnaAventura any barbarian eventos

sobrevivientes :: [Barbaro]->Aventura->[Barbaro]  --devuelve una lista de brbaros q van a ser los que van a terminar pasando esa aventura
sobrevivientes barbarians aventura= filter(\barbarian -> pasaUnaAventura all barbarian aventura) barbarians

pasaUnaAventura criterio barbarian aventura= criterio (\evento-> evento barbarian) aventura

--4)---------------------------------------------------------------------------------------------

--a)
sinRepetidos :: (Eq a)=> [a]->[a]
sinRepetidos []=[]
sinRepetidos (cabeza:cola)
    |elem cabeza cola =cola  --si cabeza esta dentro de la cola => solo dejo la cola, para no volver a incluir la cabeza q esta repetida dentro de cola
    |otherwise = (cabeza:cola) -- si cabeza No esta dentro de la cola, la incluyo en la cola 

--b)

-- accessors --
mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre f unBarbaro = unBarbaro { nombre = f . nombre $ unBarbaro }

mapHabilidades :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidades f unBarbaro = unBarbaro { habilidades = f . habilidades $ unBarbaro }


descendiente :: Barbaro-> Barbaro
descendiente= utilizarObjetos.mapNombre(++ "*").mapHabilidades sinRepetidos 

utilizarObjetos :: Barbaro -> Barbaro 
utilizarObjetos barbarian= foldr ($) barbarian (objetos barbarian) --hay muchas maneras de hacer este punto, podria ser con foldl tmb y agregar flip 

descendientes :: Barbaro-> [Barbaro]
descendientes barbarian= iterate descendiente barbarian

--c)

-- Se podría aplicar sinRepetidos sobre la lista de objetos? ¿Por qué?
 
--Los objetos de barbaro son Funciones, las funciones no son comparables entre si. 
--Si vemos que definimos a sinRepetidos con Eq a => vemos que se le pasa funciones comparables si o si 
--Entonces la respuesta a la pregunta es NO, porque vemos que los objetos no son comparables entre si por ser funciones 

--Y sobre el nombre de un bárbaro? ¿Por qué?

--La respuesta a la pregunta es SI, porque el nombre de un barbaro es un string, que es una lista de chars
--y por ser una lista de elementos comparables entre si se podria aplicar. 