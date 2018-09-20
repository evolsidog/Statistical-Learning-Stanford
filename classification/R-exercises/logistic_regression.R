require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)

# Logistic regression
glm_fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm_fit)
glm_probs=predict(glm_fit, type="response")
glm_probs[1:5]
glm_pred=ifelse(glm_probs>0.5, "Up", "Down")
attach(Smarket)
table(glm_pred, Direction)

# Maiking training and test set
train = Year<2005
glm_fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket, family = binomial, subset = train)
glm_probs=predict(glm_fit, newdata = Smarket[!train,], type="response")
glm_pred=ifelse(glm_probs>0.5, "Up", "Down")
direcction_2005=Smarket$Direction[!train]
table(glm_pred, direcction_2005)
mean(glm_pred==direcction_2005)

# Fit smaller model
glm_fit=glm(Direction~Lag1+Lag2,data=Smarket, family = binomial, subset = train)
glm_probs=predict(glm_fit, newdata = Smarket[!train,], type="response")
glm_pred=ifelse(glm_probs>0.5, "Up", "Down")
table(glm_pred, direcction_2005)
mean(glm_pred==direcction_2005)
summary(glm_fit)
