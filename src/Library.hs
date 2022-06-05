module Library where
import PdePreludat
import Data.Char(toUpper)  --para poder usar la conversion de mayusculas en el punto 2) 


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

ritualDeFechorias::Evento
ritualDeFechorias prueba barbarian = prueba==True 

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



--4)---------------------------------------------------------------------------------------