maxmin <- data.frame(
  total=c(5, 1),
  phys=c(15, 3),
  psycho=c(15, 3),
  social=c(5, 1),
  env=c(5, 1),
  total2=c(5, 1),
  phys2=c(15, 3),
  psycho2=c(5, 1),
  social2=c(5, 1),
  env2=c(5, 1))
# data for radarchart function version 1 series, minimum value must be omitted from above.
RNGkind("Mersenne-Twister")
set.seed(123)
n <- 3
dat <- data.frame(
  total=runif(n, 1, 5),
  phys=rnorm(n, 10, 2),
  psycho=rnorm(n, 10, 2),
  social=runif(n, 1, 5),
  env=runif(n, 1, 5),
  total2=runif(n, 1, 5),
  phys2=rnorm(n, 10, 2),
  psycho2=runif(n, 1, 5),
  social2=runif(n, 1, 5),
  env2=runif(n, 1, 5))
dat <- rbind(maxmin,dat)
radarchart(dat, axistype=0, seg=5,title='sampling',pcol=rgb(0.1,0.5,0.5,0.7),
           pfcol=rgb(0.1,0.5,0.5,1/n))


