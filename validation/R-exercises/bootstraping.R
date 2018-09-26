require(ISLR)
require(boot)

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X, Portfolio$Y)

# what is the standard error of alpha?
alpha_fn=function(data,index){
  with(data[index,],alpha(X,Y))
}
alpha_fn(Portfolio,1:100)

set.seed(1)
alpha_fn(Portfolio, sample(1:100, 100, replace = TRUE))

boot_out = boot(Portfolio, alpha_fn, R=1000)
boot_out
plot(boot_out)

# Exercises
install.packages('rstudioapi')
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}
set_wd()
load("../../data/5.R.RData")
ls()

# 5.R.R1
summary(lm(y~., data = Xy))

# 5.R.R2
?matplot
matplot(Xy,type = "l")

# 5.R.R3
?boot
names(Xy)
?coef
alpha_fn=function(data,index){
  coef(lm(y~., data = Xy , subset = index ))
}
boot_out = boot(Xy, alpha_fn, R=1000)
boot_out