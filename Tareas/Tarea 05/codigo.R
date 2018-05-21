install.packages("leaps")
library(leaps)

install.packages("car")
library(car)

data <- read.csv("Demographic.csv",sep=",")

data <- data.frame(
  data$M,
  data$POP,
  data$UR,
  data$IN,
  data$PR,
  data$CR,
  data$PI,
  data$PL,
  data$BL,
  data$SP,
  data$AI,
  data$MH,
  data$RP
)
names <- c(
  "M",
  "POP",
  "UR",
  "IN",
  "PR",
  "CR",
  "PI",
  "PL",
  "BL",
  "SP",
  "AI",
  "MH",
  "RP"
)
names(data) <- names

x <- model.matrix(M ~ . - 1, data=data)
y <- data$M



bestmods.cp <- leaps(x, y, nbest=1, names=names[2:length(names)], method=c("Cp"))
min(bestmods.cp$Cp)
# Entonces el mejor modelo 



bestmods.r2 <- leaps(x, y, nbest=2, names=names[2:length(names)], method=c("r2"))
max(bestmods.r2$r2)
# Entonces el mejor modelo de acuerdo a R2 es el modelo que incluye todos los regresores

install.packages("olsrr")
library(olsrr)

model <- lm(M ~ data$POP+
            data$UR+
            data$IN+
            data$PR+
            data$CR+
            data$PI+
            data$PL+
            data$BL+
            data$SP+
            data$AI+
            data$MH+
            data$RP, data=data)
ols_plot_dffits(model)


outlierTest(model)



