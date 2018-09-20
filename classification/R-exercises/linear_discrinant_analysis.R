require(ISLR)
require(MASS)

# Linear Discrimanant Analysis
lda_fit = lda(Direction~Lag1+Lag2, data=Smarket, subset = Year<2005)
lda_fit
plot(lda_fit)
smarket_2005 = subset(Smarket, Year==2005)
lda_pred=predict(lda_fit, smarket_2005)
class(lda_pred)
data.frame(lda_pred)[1:5,]
table(lda_pred$class, smarket_2005$Direction)
mean(lda_pred$class==smarket_2005$Direction)
