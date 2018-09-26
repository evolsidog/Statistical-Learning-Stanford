require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower, data=Auto)

# LOOCV (Leave-one-out-cross-validation)
glm_fit=glm(mpg~horsepower, data=Auto)
cv.glm(Auto, glm_fit)$delta #pretty slow

# Lets write a simple function to use formula
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

# Now we try it out
loocv(glm_fit)

cv_error=rep(0,5)
cv_error
degree=1:5
degree

for(d in degree){
  glm_fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv_error[d]=loocv(glm_fit)
}

plot(degree, cv_error, type="b")

# 10-fold CV
cv_error10=rep(0,5)
for(d in degree){
  glm_fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv_error10[d]=cv.glm(Auto, glm_fit, K=10)$delta[1]
}
lines(degree, cv_error10, type = "b", col="red")
