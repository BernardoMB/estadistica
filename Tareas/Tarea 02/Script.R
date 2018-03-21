# Tarea 02
# Estadistica Aplicada II

# Lesctura de datos
datos <- read.csv("./Data/casas.csv", header=TRUE, sep = " ", quote = "\"", dec = ".")
# head(datos)
# Limpieza de datos
names <- names(datos)
data <- c()
n <- ncol(datos)
for (i in 1:n) {
  data <- cbind(data, as.numeric(as.character(datos[[i]]))) 
}
datos <- data.frame(data)
names(datos) <- names
head(datos)

# Regresion que incluye todos los regresores
lm(formula = Price ~ BDR + FLR + FP + RMS + ST + LOT + TAX + BTH + CON + GAR + CDN + L1 + L2, data = datos)

# Regresion excluyendo ciertos regressores 
lm(formula = Price ~ BDR + FLR + RMS + LOT + TAX + BTH + GAR + CDN + L1 + L2, data = datos)
