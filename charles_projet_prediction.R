library(MASS)
#MASS sert a importer le data Boston
library(corrplot)
# le library corrplot sert a visualiser els variable corrélé 
require(ggplot2)
library(plotly)
require(Metrics)
#
Boston
attach(Boston)
#on observe les donné ainsi que les corrélation entre les differentes variable
summary(Boston)
corr_matrix<-cor(Boston)
#visualisation sur corrplot
corrplot(corr_matrix, type="upper")
#on verifie si les donner on une valeur manquant ou non ?
#La fonction any (is.na ()) retournera TRUE s'il manque une valeur dans notre jeu de données.
#dans ce cas, la fonction a renvoyé FALSE
any(is.na(Boston))
#Nous commençons par diviser l'ensemble de données en deux parties, ensemble d'entrainement et ensemble de test
#hasard 75% de ligne dans l'ensemble de données de Boston et le placerons dans l'ensemble d'apprentissage, et 25% de ligne de plus dans le test 
data(Boston)
#floor () est utilisé pour renvoyer la plus grande valeur entière qui n'est pas supérieure à un nombre individuel ou à une expression
smp_size<-floor(0.75*nrow(Boston))
#set.seed () est utilisé pour définir le germe du générateur de nombres aléatoires de R,
set.seed(12)
train_ind<-sample(seq_len(nrow(Boston)), size=smp_size)
train<-Boston[train_ind, ]
dim(train)
test<-Boston[-train_ind, ]
#
#Maintenant, nous avons notre ensemble d'entraînement et notre ensemble de tests, 
#jetons un coup d'œil à la corrélation entre les variables dans #lensemble dentraînement,
#nous pouvons remarquer que lstat est la variable qui a la plus forte influence sur notre medv, 
#cest pourquoi nous sélectionnons lstat comme variable pour notre régression linéaire simple.
#avec summary on peut observer tout c'est valeur quand on analyse les données


#creation de la linear regression model
lm.fit=lm(medv~lstat,data=train)
summary(lm.fit)
# on remarque que le residuel standart error est de 6.007 pour le model training

#predictiction residual standar error de notre teste
evaluate<-predict(lm.fit, test)

#on prend la 14em valeure qui est la plus pres du rems du model  training pour le model test celle strouve ligne 14
#La fonction rmse () dans la bibliothèque de métriques calculera l'erreur quadratique moyenne 
#entre les valeurs réelles et les valeurs prédites, selon ce que notre modèle a rmse environ 6,06
rmse(evaluate,test[,14 ])

# on a le rmse de predilection qui est de 6.83024

#le code en dessous permet d'aller recherche la varialbe et la ligne qui est le plus proche de nombre prediction dans
#lstat trainning plus medv pour le cout de l'arpartement 
dat <- data.frame(lstat = (1:35),
                  medv = predict(lm.fit, data.frame(lstat = (1:35))))
plot_ly() %>% 
  add_trace(x=~lstat, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~lstat, y=~medv, type="scatter", data = test, name = "Actual Value")
#Le premier graphique indique qu'il existe une corrélation non linéaire entre lstat et medv.
#Nous améliorons notre modèle en ajoutant un coefficient non linéaire à notre modèle
plot(lm.fit)
print(test$lstat[1:35])
#Hit <Return> to see next plot: lm(medv~lstat)
#Hit <Return> to see next plot: theoretical quantile(medv~lstat)
#Hit <Return> to see next plot: 

#Le premier graphique indique qu'il existe une corrélation non linéaire entre lsat et medv. 
#Nous améliorons notre modèle en ajoutant un coefficient non linéaire à notre modèle
#Transformations non linéaires des prédicteurs Étant donné un prédicteur X,
#nous pouvons créer un prédicteur X2 en utilisant I (X ^ 2).

lm.fit=lm(medv~lstat+I(lstat^2),data=train)
#print(lm.fit)
dat <- data.frame(lstat = (1:40),
                  medv = predict(lm.fit, data.frame(lstat = (1:40))))
plot_ly() %>% 
  add_trace(x=~lstat, y=~medv, type="scatter", mode="lines", data = dat, name = "Predicted Value") %>%
  add_trace(x=~lstat, y=~medv, type="scatter", data = test, name = "Actual Value")

# on re evalue le model

summary(lm.fit)
evaluate<-predict(lm.fit, test) 
rmse(evaluate,test[,14 ])
#En ajoutant simplement un coefficient non linéaire à notre modèle, 
#le rmse a chuté de manière significative et le modèle a mieux adapté les données


#creation de multiple lunear model
for ( i in range(1000)) { 
  
  lm.fit=lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+I(lstat^2),data=train)
  print(lm.fit) 
}

 
#lm.fit=lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+I(lstat^2),data=train)


summary(lm.fit)


#modèle sur un ensemble de données de test:
evaluate<-predict(lm.fit, test) 
#Use predict() to produce confidence intervals and prediction intervals for the prediction of medv for a given value lsat
#head(predict(lm.fit, test, interval = "confidence"), 5)
#head(predict(lm.fit, test, interval = "prediction"), 5)
rmse(evaluate,test[,14 ])

