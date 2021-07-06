import Text.Show.Functions

type Planeta = [Colonia]

data Colonia = UnaColonia {
    superficie :: Int,
    habitantes :: [Habitante]
} deriving (Show)

coloniaDePrueba = UnaColonia 100 [UnHabitante ["cantar"] 5 10, UnHabitante ["bailar"] 2 7]

type Habilidad = String
data Habitante = UnHabitante{
    habilidades :: [Habilidad],
    capacidadSupervivencia :: Int,
    salud :: Int
} deriving (Show)

data Mision = UnaMision {
    funcionPorHabitante  :: (Habitante -> Habitante),
    funcionHabilitadora :: (Colonia -> Bool)
} deriving (Show)

-- 1.Hacer que la salud de un astronauta se modifique en una cierta cantidad de unidades. Definir qué sucede con los que lleguen a salud negativa.
modificarSalud:: Int -> Habitante -> Habitante
modificarSalud num habitante = habitante {salud = max 0 (salud habitante + num) }

-- 2.Hacer que se enseñe una nueva habilidad a todos loshabitantes de una colonia.
enseñarHabilidad:: Habilidad -> Colonia -> Colonia
enseñarHabilidad habilidad colonia = colonia {habitantes = map (agregarHabilidad habilidad) (habitantes colonia)}

agregarHabilidad:: Habilidad -> Habitante -> Habitante
agregarHabilidad habilidad habitante = habitante {habilidades = (habilidad : habilidades habitante)}


-- 3.Calcular la cantidad de habitantes de un planeta.
cantidadDeHabitantesColonia :: Colonia -> Int
cantidadDeHabitantesColonia = length.habitantes

-- 4.Saber si una colonia es habitable
multiplicadorDeSuperficie = 10

esGrande :: Colonia -> Bool
esGrande colonia = superficie colonia * multiplicadorDeSuperficie > cantidadDeHabitantesColonia colonia

sumatoriaDe funcion colonia = (sum.(map funcion)) (habitantes colonia)

promedioSalud:: Colonia -> Int
promedioSalud colonia = div (sumatoriaDe salud colonia) (cantidadDeHabitantesColonia colonia)  

esSaludable :: Colonia -> Bool
esSaludable colonia = promedioSalud colonia > 50

esHabitable:: Colonia -> Bool
esHabitable colonia = esGrande colonia && esSaludable colonia

-- 5.Saber si la colonia esta bien manejada
habilidadesMinimasParaSerHabil = 10
supervivenciaAlta = 5

tieneMuchasHabilidades:: Habitante -> Bool
tieneMuchasHabilidades habitante = ((> habilidadesMinimasParaSerHabil).length.habilidades) habitante

todoAstronautaEsUtil:: Colonia -> Bool
todoAstronautaEsUtil colonia = all tieneMuchasHabilidades (habitantes colonia)

esUnCapo :: Habitante -> Bool
esUnCapo habitante = capacidadSupervivencia habitante > supervivenciaAlta

elPrimerAstronautaEsUnCapo :: Colonia -> Bool
elPrimerAstronautaEsUnCapo = esUnCapo.head.habitantes

estaBienManejada :: Colonia -> Bool
estaBienManejada colonia = todoAstronautaEsUtil colonia || elPrimerAstronautaEsUnCapo colonia

-- 1.Obtener como queda el planeta luego de una mision e implementar las misiones mencionadas
aplicarMisionAColonia :: Mision -> Colonia -> Colonia
aplicarMisionAColonia (UnaMision funcionPorHabitante funcionHabilitadora) colonia  
  | funcionHabilitadora colonia = colonia {habitantes = map funcionPorHabitante (habitantes colonia)}
  | otherwise = colonia

aplicarMision :: Mision -> Planeta -> Planeta
aplicarMision mision planeta = map (aplicarMisionAColonia mision) planeta

misionDeCapacitacion         = UnaMision  duplicarCapacidad (not.estaBienManejada)
misionDeClonacion astronauta = UnaMision  (\_ -> astronauta) (\_ -> True)
misionFuncional              =  UnaMision ((agregarHabilidad "programacion funcional").modificarSalud (-20)) esHabitable
misionApagadoDeIncendio n    = UnaMision (modificarSalud (-20)) (not.(existeAstronautaConNHabilidades n))

-- 3.Saber el aumento de habitabilidad de un planeta comoconsecuencia de una mision,que se calcula como la diferencia entre la cantidadde colonias habitables antes ydespués de la mision
cantidadDeColoniasHabitables :: Planeta -> Int
cantidadDeColoniasHabitables = length.(filter esHabitable)

cambioDeHabitabilidad :: Mision -> Planeta -> Int
cambioDeHabitabilidad mision planeta =  cantidadDeColoniasHabitables (aplicarMision mision planeta) - cantidadDeColoniasHabitables planeta

  -- 4.Saber cual es la mejor mision para un planeta, a partirde un conjunto de posiblesmisiones.a.En aumento de habitabilidadb.En aumento de cantidad de habitantes
mejorMision _ [mision] planeta = mision
mejorMision criterio (mision : mision2 : otrasMisiones) planeta  
  | criterio mision planeta > criterio mision2 planeta = mejorMision criterio (mision : otrasMisiones) planeta
  | otherwise = mejorMision criterio (mision2 : otrasMisiones) planeta 

--
duplicarCapacidad astronauta = astronauta {capacidadSupervivencia = (capacidadSupervivencia astronauta * 2) }
existeAstronautaConNHabilidades n colonia = any ((>n).length.habilidades) (habitantes colonia)

-- 1.Escribir una funcion que devuelva una colonia de estas caracteristicas
habitantesInfinitos = (UnHabitante [] 0 0):habitantesInfinitos
coloniaInfinita = UnaColonia 1 habitantesInfinitos

respuestaFinita colonia = ((take 5).habitantes) colonia
respuestaInfinita colonia = (habitantes colonia) ++ (habitantes colonia)
respuestaQueNuncaTermina colonia = ((filter esUnCapo).habitantes) colonia
respuestaQueDependaDeLosParametros valor colonia = any ((>valor).salud) (habitantes colonia)
