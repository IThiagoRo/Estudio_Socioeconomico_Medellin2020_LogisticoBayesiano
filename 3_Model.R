# Paquetes necesarios
library(R2jags)
library(rjags)
library(coda)
library(dplyr)
library(magrittr)
library(kableExtra)
library(knitr)
library(mcmcplots)
library(superdiag)
library(HDInterval)


# Lectura de la Base de datos
db <- read.csv("Data/db.csv")

# Removiendo NA y categorias sin informacion relevante de la encuesta de calidad de vida
db1 <- db[-which(db$P_235 > 1500000|db$P_235 == -99 | db$P_235 == -98| db$P_235 == -88, arr.ind=TRUE),]
db1 <- db1[-which(db1$P_243 == -88  | db1$P_243 == -98 | db1$P_243 == -99 | db1$P_243 >= 1000000, arr.ind=TRUE),]
db1 <- db1[-which(db1$P_73 ==-88  | db1$P_73 ==-98 | db1$P_73 ==-99, arr.ind=TRUE),]



y = db1$P_291 # Posibilidad que el hogar se quede sin alimentos
x1 = as.factor(db1$P_10) # Estrato
x2 = (db1$P_12 - mean(db1$P_12)) # Numero personas en el hogar
x3 = (db1$P_235 - mean(db1$P_235))/sd(db1$P_235) # Ingresos en el hogar a Alimentos
x4 = (db1$P_243 - mean(db1$P_243))/sd(db1$P_243) # Gastos Financieros del hogar
x5 = as.factor(db1$P_73) # Busca empleo


# Seleccionando el estrato 6 como nivel de referencia
x1 = relevel(x1, ref="6")

# Matriz de diseño
X = model.matrix(~x1+x2+x3+x4+x5)

head(X)
dim(X)

#--- modelo ---#
modelo1 <- function(){
  for (i in 1:N) {
    y[i] ~ dbern(p[i])   #variable respuesta
    
    logit(p[i]) <- inprod(b[], x[i,])  #predictor lineal
  }
  for (j in 1:K) {
    b[j] ~ dnorm(0,1.0E-12)  #apriori
  }
}



# Tamano de la muestra
N = dim(db1)[1]

# Input o informacion de entrada
data.input.jags <- list(y=y, x = X, N = N, K = ncol(X))


# Parametros a monitorear
bayes.mod.params <- c("b")


# Puntos iniciales de la cadena MCMC
bayes.mod.inits <- function(){
  list("b" = rnorm(10))  #cantidad de variables 
}


set.seed(123)
bayes.mod.fit <- jags(data = data.input.jags, inits = bayes.mod.inits,
                      parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 30000,
                      n.burnin = 5000, model.file = modelo1)


# HDI
hdi(bayes.mod.fit)


Beta.poste <- bayes.mod.fit$BUGSoutput$sims.list$b
dim(Beta.poste)

Py1.X = sapply(1:dim(X)[1], function(j){median(sapply(1:dim(Beta.poste)[1], function(i){exp(X[j,]%*%Beta.poste[i,])/ (1 + exp(X[j,]%*%Beta.poste[i,]))}))})


# Curva ROC 
library(ROCR)
ROCR.simple2 = list(predicciones = Py1.X, labels = as.numeric(y))
df <- data.frame(ROCR.simple2)
pred <- ROCR::prediction(df$predicciones, df$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE,type="l")
abline(a=0,b=1,col="blue")

# Punto de corte optimo
cost.perf <- performance(pred, measure ="cost")
opt.cut   <- pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# coordenadas del punto de corte optimo
xcoor <-perf@x.values[[1]][which.min(cost.perf@y.values[[1]])]
ycoor <-perf@y.values[[1]][which.min(cost.perf@y.values[[1]])]
points(xcoor, ycoor, pch=20, col="purple")

# Area bajo la curva
AUC       <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]]) 
cat("Punto de corte optimo:",opt.cut)


# Matriz de Confusion 
library(tibble)
library(cvms)
df <- ifelse(Py1.X >= 0.55, 1, 0)
m <- tibble("real" = y, "predicho" = Py1.X)


eval <- cvms::evaluate(m, target_col = "real", prediction_cols = "predicho", type="binomial")
conf_mat <- eval$`Confusion Matrix`[[1]]
cvms::plot_confusion_matrix(conf_mat)




saveRDS(bayes.mod.fit, 'Data/model_current.rds')

