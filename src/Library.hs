module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa { precioBase = 20, ingredientes = [Pan, Carne, Cheddar, Pan] }

agrandar :: Hamburguesa -> Hamburguesa
agrandar unaHamburguesa = agregarIngrediente (ingredienteBase unaHamburguesa) unaHamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente unIngrediente unaHamburguesa = unaHamburguesa { ingredientes = unIngrediente : ingredientes unaHamburguesa }



ingredientesBase :: [Ingrediente]
ingredientesBase = [Carne, Pollo, PatiVegano]

ingredienteBase :: Hamburguesa -> Ingrediente
ingredienteBase unaHamburguesa = elegirIngredienteBase (filter (estaEn unaHamburguesa) ingredientesBase)

estaEn :: Hamburguesa -> Ingrediente -> Bool
estaEn unaHamburguesa unIngrediente = elem unIngrediente (ingredientes unaHamburguesa)

elegirIngredienteBase :: [Ingrediente] -> Ingrediente
elegirIngredienteBase (unIngrediente : _) = unIngrediente
elegirIngredienteBase [] = Carne



descuento :: Number -> Hamburguesa -> Hamburguesa
descuento unPorcentaje unaHamburguesa = unaHamburguesa { precioBase = precioConDescuento unPorcentaje (precioBase unaHamburguesa) }

precioConDescuento :: Number -> Number -> Number
precioConDescuento unPorcentaje unPrecio = unPrecio * (100 - unPorcentaje) / 100

precioDeIngredientesDeHamburguesa :: Hamburguesa -> Number
precioDeIngredientesDeHamburguesa = sum . map precioIngrediente . ingredientes

precioFinal :: Hamburguesa -> Number
precioFinal unaHamburguesa = precioBase unaHamburguesa + precioDeIngredientesDeHamburguesa unaHamburguesa

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra






dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 30 . agregarIngrediente Papas









cambiarIngredientes :: (Ingrediente -> Ingrediente) -> Hamburguesa -> Hamburguesa
cambiarIngredientes unaFuncion unaHamburguesa = unaHamburguesa { ingredientes = map unaFuncion (ingredientes unaHamburguesa) }

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = cambiarIngredientes convertirIngredienteVeggie

convertirIngredienteVeggie :: Ingrediente -> Ingrediente
convertirIngredienteVeggie Carne = PatiVegano
convertirIngredienteVeggie Pollo = PatiVegano
convertirIngredienteVeggie PatiVegano = PatiVegano
convertirIngredienteVeggie Cheddar = QuesoDeAlmendras
convertirIngredienteVeggie Panceta = BaconDeTofu
convertirIngredienteVeggie unIngrediente = unIngrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati = cambiarIngredientes cambiarPanIntegral

cambiarPanIntegral :: Ingrediente -> Ingrediente
cambiarPanIntegral Pan = PanIntegral
cambiarPanIntegral unIngrediente = unIngrediente

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto