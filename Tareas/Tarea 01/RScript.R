slope <- function() {
  sum.of.products <- 0
  sum.of.squares <- 0
  for (i in 1:length(datos$peso)) {
    xi <- datos$peso[i]
    yi <- datos$presion[i]
    sum.of.products <- sum.of.products + xi*yi
    sum.of.squares <- sum.of.squares + xi*xi
  }
  sum.of.products/sum.of.squares
}
slope()


ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.1 <- function(x) x^2 + x
p + stat_function(fun = fun.1) + xlim(-5,5)
