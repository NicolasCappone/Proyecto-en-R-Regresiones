#MODELO DE REGRESION LINEAL SIMPLE (VELOCITY - CARGA DE DISCO )

plot(Samara$Load, Samara$Velocity, col= Samara$Tree)
plot(grupo8Data$Load, grupo8Data$Velocity)
modelo = lm(Samara$Velocity ~ Samara$Load , data= Samara)
summary(modelo)
abline(modelo, col = "blue")



modeloo = lm(grupo8Data$Velocity ~ grupo8Data$Load , data= grupo8Data)

summary(modeloo)
plot(modeloo$residuals)

step(modelo2, direction = "both")

modelo1.res = resid(modelo)

plot(Samara$Load, modelo1.res)
studres(modelo)

qf(0.95 , df1 = 3 , df2 = 31)

plot(modelo$fitted.values,studres(modelo)) 
abline(a = 800 ,b = 0 , ity= 2, col= "blue")
abline(a = -800 ,b = 0 , ity= 2, col= "blue")


#Intervalos de confianza 95%
qt(0.975 , 33)

#Test de hipotesis a nivel 0,05 
qt(0.95 , 33)

mean( Samara$Velocity)

#Estudio de los residuos 

plot (modelo$fitted.values, modelo$residuals)
abline(a = 0.00,b= 0 ,   col= "red" )


#MODELO VARIABLES DAMMIE 

set(Samara, Velocity!= 99.00)

Samara$Arbol1 <- ifelse(Samara$Tree == 1,1,0)

Samara$Arbol2 <- ifelse(Samara$Tree == 2,1,0)

modelodummies1 = lm(Samara$Velocity ~ Samara$Load + Samara$Arbol1 + Samara$Arbol2)

summary(modelodummies1)

Samara$intarb1 <- Samara$Arbol1*Samara$Velocity

Samara$intarb2 <- Samara$Arbol2*Samara$Velocity

modelodummies2 = lm(Samara$Velocity ~ Samara$Load + Samara$Arbol1 + Samara$Arbol2 + Samara$intarb1 + Samara$intarb2)

summary(modelodummies2)

modelodummies3 = lm(Samara$Velocity ~ Samara$Arbol1 + Samara$Arbol2 + Samara$intarb1 + Samara$intarb2 )

summary(modelodummies3)


step(modelodummies1, direction = "forward")


multi = cbind(Samara, Samara$Load*Samara$Arbol1*Samara$Arbol2)

names(multi) = c("Tree", "Load", "Velocity", "Arbol1", "Arbol2", "LoadxArbol1xArbol2")

head(multi)

modelo3 = lm(Velocity ~ Load  + Arbol1 + Arbol2 + LoadxArbol1xArbol2, data = multi)
summary(modelo3)

plot(modelodummies1$fitted.values, modelodummies1$residuals)
abline(a = 0.00,b= 0 ,   col= "red" )

#MODELO LINEAL SIMPLE PARA CADA ARBOL 

#ARBOL 1
arbolf = lm(Arbol1$Velocity ~ Arbol1$Load , data = Arbol1)
summary(arbolf)
plot(Arbol1$Load, Arbol1$Velocity)
abline(arbolf, col=" blue")
qt(0.95, 10)
mean(Arbol1$Velocity)
plot (arbolf$fitted.values, arbolf$residuals)
abline(a = 0.00,b= 0 ,   col= "red" )

arbolone = cbind( Arbol1, sqrt(Arbol1$Load))
names(arbolone) = c(names(Arbol1), "Arbolc")

modeloar2 = lm(Velocity ~ Load + Arbolc , data= arbolone)
summary(modeloar2)

plot(modeloar2$fitted.values, modeloar2$)

x= 0.27:1.35
lines(x ,5.206 , 23.217*x +  -19.409*sqrt(x)  , col = "blue", lwd= 3)


#ARBOL 2
arbols = lm(Arbol2$Velocity ~ Arbol2$Load , data = Arbol2)
summary(arbols)
plot(Arbol2$Load, Arbol2$Velocity)
abline(arbols, col=" blue")
qt(0.95, 9)
mean(Arbol2$Velocity)
plot (arbols$fitted.values, arbols$residuals)
abline(a = 0.00,b= 0 ,   col= "red" )

  #ARBOL 3
arbolt = lm(Arbol3$Velocity ~ Arbol3$Load , data = Arbol3)
summary(arbolt)
plot(Arbol3$Load, Arbol3$Velocity)
abline(arbolt, col=" blue")
qt(0.95, 10)
mean(Arbol3$Velocity)
plot (arbolt$fitted.values, arbolt$residuals)
abline(a = 0.00,b= 0 ,   col= "red" )



#Regresion logistica

Samara$Arbol1 <- ifelse(Samara$Tree == 1,1,0)

modelo9 = glm(Samara$Arbol1 ~ Samara$Load + Samara$Velocity, family = "binomial", data = Samara )
summary(modelo9)

step(modelo9 , direction = "both")

qchisq(0.95 , 1)

1- pchisq(9.175, df=1)
