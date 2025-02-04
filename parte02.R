# diferencas entre medias no teste tukey
#outer(as.numeric(medias),as.numeric(medias), FUN = '-')

# ANOVA DE UM CONJUNTO DE DADOS COM BLOCOS -------------------------------------

dados<-data.frame(
  `1` = c(145, 155, 166),
  `2` = c(200, 190, 190),
  `3` = c(183, 186, 208),
  `4` = c(190, 175, 186),
  `5` = c(180, 160, 156),
  `6` = c(130, 160, 130),
  `7` = c(206,NA, 170),
  `8` = c(250 ,271, 230),
  `9` = c(164 ,190 ,193)
  
)
dados<-t(dados)

# usar o chunk abaixo se tiver PP ----------------------------------------------

I<-k

I<-nrow(dados) # numero de tratamentos
T_PP <- sum(dados[7,], na.rm=TRUE)
J<-ncol(dados)
B<-sum(dados[,2], na.rm=TRUE)
G<-sum(dados,na.rm=TRUE)

PP<-(I*T_PP + J*B - G)/((I-1)*(J-1))
PP<-ceiling(PP)

U<- ((I - 1)/I)*(193-B/(I-1))^2

dados[7,2]<-193

# ANOVA (BALANCEADO) -----------------------------------------------------------

k<-9 # numero de tratamentos
n<-27 # numero total de obs
r<-3 # numero de repeticoes por tratamento

# fator correcao
C<-sum(dados)^2/n

# soma quadrados total
SQT<-sum(dados^2)-C

# soma de quadrados de tratamentos
T_<-as.numeric(apply(dados,1,sum)) # ATENCAO: deve somar para cada tratamento (use 2 apenas se os tratamentos estiverem nas colunas)
SQTrat<-sum(T_^2)/r-C

# soma de quadrados dos blocos
T_B<-as.numeric(apply(dados,2,sum)) # ATENCAO: deve somar para cada tratamento (use 2 apenas se os tratamentos estiverem nas colunas)
r_B<-9
SQBlocos<-sum(T_B^2)/r_B-C

# soma de quadrados de residuo
SQR<-SQT-SQTrat-SQBlocos

# quadrado medio de tratamentos
QMTrat<-SQTrat/(k-1)

# quadrado medio dos blocos
QMBlocos<-SQBlocos/(r-1) # numero de blocos menos 1

# quadrado medio de residuo
QMR<-SQR/(15) # GL dos residuos

# valor de F (estatistica de teste para igualdade de medias entre grupos)
F_<-QMTrat/QMR
F_B<-QMBlocos/QMR

# Valor critico da F a 5%
qf(0.95,k-1,15) # GL trat, GL res 

# p-valor da ANOVA
pf(f,k-1,n-k,lower.tail = FALSE) 

# lembrar do CV
(sqrt(sum(e^2,na.rm=TRUE)/(n-k-2)))/mean(dados,na.rm=TRUE)*100
# ou 
sqrt(QMR)/mean(dados,na.rm=TRUE)*100


# ANOVA COM PP -----------------------------------------------------------------

# SQTrat Ajustado
SQTrat_aj<-SQTrat-U

QMTrat <- SQTrat_aj/I-1

F_<-QMTrat/QMR
qf(0.95,I-1,15)

# CV (%)
sqrt(QMR)/mean(dados)*100


# TESTE TUKEY ------------------------------------------------------------------

m_trat<-apply(dados,1,mean)
m_trat<-as.numeric(m_trat)
M<-outer(m_trat,m_trat,FUN='-')
M[!upper.tri(M, diag = TRUE)] <- 0
M

q<-5.08
delta<-q*sqrt(QMR/r)

which(abs(M)>45.29)

# RESIDUOS CONDICIONAL (2 OU MAIS PPs) -----------------------------------------

# utilizar dataset sem a parcela estimada para a PP

C<-sum(dados,na.rm=TRUE)^2/26
SQT_1<-sum(dados^2,na.rm=TRUE)-C

T_B_1<-as.numeric(apply(dados,2,sum,na.rm=TRUE)) # ATENCAO: deve somar para cada tratamento (use 2 apenas se os tratamentos estiverem nas colunas)
r_B_1<-c(9,8,9)
SQBlocos_1<-sum((T_B_1^2)/r_B)-C

SQR_1<-SQT_1-SQBlocos_1

SQTrat_aj<-SQR_1-SQR
