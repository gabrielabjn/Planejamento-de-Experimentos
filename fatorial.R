setwd('C:/Users/gabriela.silva/Desktop/faculdade/pe')
library(readxl)
data<-read_excel('Fatorial.xlsx')

# 2 FATORES C/ DELINEAMENTO EM BLOCOS ------------------------------------------

I <- 5 # numero de niveis fator 1 (aparelho)
J <- 4 # numero de niveis fator 2 (operador)
R <- 10 # numero de blocos (arvores)

# GL ---------------
GL_1<- I - 1
GL_2<- J-1
GL_int<- GL_1*GL_2
GL_trat<-I*J-1
GL_b<-R-1
GL_tot<-I*J*R-1
GL_res<-GL_tot-GL_b-GL_trat

# SQ ----------------

C<-1/(I*J*R)*sum(data$obs)^2

# Aparelho (fator 1)
aparelho <- aggregate(obs ~ aparelho, data = data, FUN = sum)
SQ1<-(1/(J*R))* sum(aparelho$obs^2) - C

# Operador (fator 2)
op <- aggregate(obs ~ operador, data = data, FUN = sum)
SQ2<-(1/(I*R))* sum(op$obs^2) - C

# (Tratamento)
ap_op<-aggregate(obs~aparelho+operador,data=data,FUN=sum)
SQ4<-1/R*sum(ap_op$obs^2)-C

# Interacao
SQ3<-SQ4-SQ1-SQ2

# Blocos
blocos<-aggregate(obs~bloco,data=data,FUN=sum)
SQ5<-1/(I*J)*sum(blocos$obs^2)-C

# Total 
SQ7<-sum(data$obs^2)-C

# Residuos
SQ6<-SQ7-SQ4-SQ5

# QM ----------------

QM1<-SQ1/GL_1 
QM2<-SQ2/GL_2
QM3<-SQ3/GL_int
# QM4<-SQ4/GL_trat
# QM5<-SQ5/GL_b
QM6<-SQ6/GL_res
# QM7<-SQ7/GL_tot

F1<-QM1/QM6 # fator1
F2<-QM2/QM6 # fator2
F3<-QM3/QM6

# Valor quartilico da F a 95% (considerando QM_int/QM_res)
qf(0.95,GL_int,GL_res)

# COEFICIENTE DE VARIACAO
sqrt(QM6)/mean(data$obs,na.rm=TRUE)*100

# GRAFICO ----------------------------------------------------------------------

ap_op_media<-aggregate(obs~aparelho+operador,data=data,FUN=mean)

cores <- as.factor(ap_op_media$operador)
plot(y=ap_op_media$obs, x = ap_op_media$aparelho, col=cores,pch=19,
     xlab = 'aparelho', ylab='medias')


# DESDOBRAMENTO DE FATOR DENTRO DE FATOR --------------- -----------------------

ap_op

# soma dos fatores 1 (ap) dentro de cada fator 2 (op)
tot_op <-aggregate(obs ~ operador,data=ap_op,FUN=sum)$obs # total por operador
# sum(tot_op) # conferir com total geral

# soma dos fatores 2 (op) dentro de cada fator 1 (ap)
tot_ap <-aggregate(obs ~ aparelho,data=ap_op,FUN=sum)$obs
# sum(tot_ap)

# Op. d. Ap ---------------------------------
k<-5
x<-ap_op[ap_op$aparelho==k,]
SQ<-(1/R)*sum(x$obs^2)-(sum(x$obs)^2)*1/(J*R)
SQ
QM<-SQ/GL_2
QM
QM/QM6

qf(0.95,GL_2,GL_res)

# Ap. d. Op ----------------------------------
k<-4
x<-ap_op[ap_op$operador==k,]
SQ<-(1/R)*sum(x$obs^2)-(sum(x$obs)^2)*1/(I*R)
SQ
QM<-SQ/GL_1
QM
QM/QM6

qf(0.95,GL_1,GL_res)

# --------------------------------------------

x<-ap_op_media[ap_op_media$operador==1,]
abs(outer(x$obs,x$obs,'-'))

3.92*sqrt(0.4129/10)
