#****This script make a bias-Linearity measure study*******
#Author Ing. Mario Ivan Salas Dominguez
#
library(readxl)#import Excel data librarywd
library(ggplot2)#import ggplot library
Datos <- read_excel("Linealidad_r_dataset.xlsx",col_names = TRUE)#read the data in excel of the study.
names (Datos)[2] = "ref_values"#change the column 2 to ref_value
#References and average bias evaluation
  sesgo_i<-c(Datos$Respuesta-Datos$ref_values)#Bias by references group
  Datos<-cbind(Datos,sesgo_i)#Actualiza los datos con los sesgos.
  avge_bias<-tapply(Datos$sesgo_i,Datos$ref_values ,mean)#Mean by references group
  sd_bias<-tapply(Datos$sesgo_i,Datos$ref_values ,sd)#SD by references group
  n_bias<-tapply(Datos$sesgo_i,Datos$ref_values ,length)#number of values by references
  ref_values<-unique(Datos$ref_values)#Get the reference values
  t_values<-c(avge_bias/(sd_bias/sqrt(n_bias)))#Evaluar los valore del estadistico t
  p_values<-c(2*pt(abs(t_values),n_bias-1,lower.tail = FALSE))#Evalua los p-values
  bias_ref_report<-cbind(ref_values,avge_bias,p_values)#matrix references bias report 
  #Bias average report
 t.test(sesgo_i,alternative = "two.sided",mu=0)
#Análisis y gráfica de regresión linealidad
merge(Datos,bias_ref_report)
regresion<-lm(formula = sesgo_i~Datos$ref_values)#Establce las constantes de la regresión lineal
anova(regresion)# Analisis de varianza de la regresion
summary(regresion)#Resumen del analisis de regresion
ggplot(Datos, aes(x = ref_values, y = sesgo_i)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, level=0.95)
