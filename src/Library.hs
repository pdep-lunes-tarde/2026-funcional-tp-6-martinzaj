module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

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
ingredientesBase = [Carne, Pollo]

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