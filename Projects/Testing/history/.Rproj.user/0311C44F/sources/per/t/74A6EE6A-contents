# Credit: https://www.r-bloggers.com/the-batmans-ecosystem/


library(ggplot2)

f1u <-
  function(x)
    ifelse ((abs(x) >  3 & abs(x) <= 7), 3 * sqrt(1 - (x / 7) ^ 2), 0)
f1d <-
  function(x)
    ifelse ((abs(x) >= 4 & abs(x) <= 7),-3 * sqrt(1 - (x / 7) ^ 2), 0)
f2u <-
  function(x)
    ifelse ((abs(x) > 0.50 & abs(x) < 0.75), 3 * abs(x) + 0.75, 0)
f2d <-
  function(x)
    ifelse ((abs(x) > -4 &
               abs(x) < 4),
            abs(x / 2) - (3 * sqrt(33) - 7) * x ^ 2 / 112 - 3 + sqrt(1 - (abs(abs(
              x
            ) - 2) - 1) ^ 2),
            0)
f3u <- function(x)
  ifelse ((x > -0.5 & x < 0.5), 2.25, 0)
f4u <-
  function(x)
    ifelse ((abs(x) >  1 &
               abs(x) <= 3),
            6 * sqrt(10) / 7 + (1.5 - 0.5 * abs(x)) * sqrt(abs(abs(x) - 1) / (abs(x) -
                                                                                1)) - 6 * sqrt(10) * sqrt(4 - (abs(x) - 1) ^ 2) / 14,
            0
    )
f5u <-
  function(x)
    ifelse ((abs(x) >= 0.75 & abs(x) <= 1), 9 - 8 * abs(x), 0)
fu <- function (x)
  f1u(x) + f2u(x) + f3u(x) + f4u(x) + f5u(x)
fd <- function (x)
  f1d(x) + f2d(x)
batman <- function(r, x)
  ifelse(r %% 2 == 0, fu(x), fd(x))

par(bg = "#02233f", mar = c(0, 0, 0, 0))
plot(
  x = seq(-7, 7, 0.065),
  y = Vectorize(batman)(1:216, seq(-7, 7, 0.065)),
  col = "#999900",
  type = "l"
)


