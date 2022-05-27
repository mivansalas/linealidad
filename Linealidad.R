#****This script make a bias-Linearity measure study*******
#Author Ing. Mario Ivan Salas Dominguez
#
library(readxl)#import Excel data librarywd
Datos <- read_excel("Linealidad_r_dataset.xlsx",col_names = TRUE)#read the data in excel of the study.
names (Datos)[2] = "ref_values"#change the column 2 to ref_value
#References and average bias evaluation
  tol_var<-16.5368
  sesgo_i<-c(Datos$Respuesta-Datos$ref_values)#Bias by references group
  Datos<-cbind(Datos,sesgo_i)#Actualiza los datos con los sesgos.
  avge_bias<-tapply(Datos$sesgo_i,Datos$ref_values ,mean)#Mean by references group
  sd_bias<-tapply(Datos$sesgo_i,Datos$ref_values ,sd)#SD by references group
  n_bias<-tapply(Datos$sesgo_i,Datos$ref_values ,length)#number of values by references
  ref_values<-unique(Datos$ref_values)#Get the reference values
  t_values<-c(avge_bias/(sd_bias/sqrt(n_bias)))#Evaluar los valore del estadistico t
  p_values<-c(2*pt(abs(t_values),n_bias-1,lower.tail = FALSE))#Evalua los p-values
  bias_ref_report<-cbind(ref_values,avge_bias,p_values)#matrix references bias report 
  ldata<-merge(Datos,bias_ref_report)
  regresion<-lm(formula = sesgo_i~Datos$ref_values)#Establce las constantes de la regresión lineal
  summary(regresion)#Resumen del analisis de regresion
#bias and linearity percent report
lin<-abs(regresion[["coefficients"]][["Datos$ref_values"]]*tol_var)
percent_lin<-lin*100/tol_var)
percent_bias<-abs(mean(avge_bias)*100/tol_var)
percent_report<-cbind(percent_lin,percent_bias)
barplot(percent_report,col = "blue",ylab = "Percent")
#linearity regression plot
plot(ldata$ref_values,ldata$sesgo_i, col="blue",xlab = "Reference values",ylab = "Bias", main = "Linearity and bias report
     ")#plot bias values
points(ref_values,avge_bias,col="red",pch=16)#plot bias average
abline(h=0,lty=2)       
abline(regresion,col="red")
preds <- predict(regresion,interval = 'confidence')
lines(Datos$ref_values, preds[ ,3], lty = 'dashed', col = 'blue')
lines(Datos$ref_values, preds[ ,2], lty = 'dashed', col = 'blue')
