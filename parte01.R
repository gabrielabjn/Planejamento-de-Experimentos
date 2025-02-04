# PLANEJAMENTO DE EXPERIMENTOS
# LISTA 01

# ANALISE DE RESIDUOS (BALANCEADO) ---------------------------------------------
# EXPERIMENTO BALANCEADO 

dados<-data.frame(
  A = c(110,157,194,178),
  B = c(1,2,4,18),
  C = c(880,1256,5276,4355),
  D = c(495,7040,5307,10050),
  E = c(7,5,29,2)
)

dados<-t(dados)

n<-20
m<-sum(dados)/n
J<-4

t_i<-as.numeric(apply(dados,1,sum))/4-m
  
e<-dados-m

for (i in seq_along(t_i)) e[i,]<-e[i,]-t_i[i]  
  
shapiro.test(e)  
lillie.test(e)
hist(e,freq=FALSE)  
plot(as.vector(dados),e,xlab='valores ajustados',ylab='residuos')

qqnorm(e)  
qqline(e)

trat<-factor(rep(c("A","B","C","D"), length.out = length(as.vector(e))))

bartlett.test(as.vector(e)~trat)

boxcox(as.vector(dados)~trat)

dados<-log(dados) # repetir todo o codigo acima considerando esta trasformacao
# ate o teste de bartlet


# ANOVA (BALANCEADO) -----------------------------------------------------------
# EXPERIMENTO BALANCEADO
# LIVRO SONIA 

k<-4 # numero de tratamentos
n<-20 # numero total de obs
r<-5 # numero de repeticoes por tratamento

# fator correcao
C<-sum(dados)^2/n

# soma quadrados total
SQT<-sum(dados^2)-C

# soma de quadrados de tratamentos
T_<-as.numeric(apply(dados,2,sum)) # ATENCAO: deve somar para cada tratamento (use 2 apenas se os tratamentos estiverem nas colunas)
SQTrat<-sum(T_^2)/r-C

# soma de quadrados de residuo
SQR<-SQT-SQTrat

# quadrado medio de tratamentos
QMTrat<-SQTrat/(k-1)

# quadrado medio de residuo
QMR<-SQR/(n-k)

# valor de F (estatistica de teste para igualdade de medias entre grupos)
F_<-QMTrat/QMR

# Valor critico da F a 5%
qf(0.95,k-1,n-k) 

# p-valor da ANOVA
pf(f,k-1,n-k,lower.tail = FALSE) # 

# lembrar do CV
(sqrt(sum(e^2,na.rm=TRUE)/(n-k)))/mean(data,na.rm=TRUE)*100
# ou 
# sqrt(QMR)/mean(data,na.rm=TRUE)*100


# ANALISE DE RESIDUOS (DESBALANCEADO) ------------------------------------------
# EXPERIMENTO DESBALANCEADO

data<-data.frame(
  A = c(58.2,57.2,58.4,55.8,54.9),
  B = c(56.3,54.5,57,55.3, NA),
  C = c(50.1,54.2,55.4, NA, NA),
  D = c(52.9,49.9,50,51.7, NA)
)

data<-t(data)

#yij = m + ti + eij

m<-mean(as.matrix(data),na.rm=TRUE)
I<-4 # numero de tratamentos

t_i<-rep(NA,4)
t_i[1]<-sum(data[1,],na.rm=TRUE)/5-m
t_i[2]<-sum(data[2,],na.rm=TRUE)/4-m
t_i[3]<-sum(data[3,],na.rm=TRUE)/3-m
t_i[4]<-sum(data[4,],na.rm=TRUE)/4-m

e<-matrix(nrow=4,ncol=5)
for (i in 1:I) e[i,]<-data[i,]-m-t_i[i]

hist(e,freq=FALSE)
qqnorm(e)
qqline(e)
shapiro.test(e) # nao rejeita H0 de normalidade

trat<-factor(rep(c("A","B","C","D"),5))
bartlett.test(as.vector(e)~trat) # nao rejeita hipotese de homocedasticidade dos erros


# pressupostos ANOVA |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# modelo aditivo
# erros independentes (casualizacao)
# E ~ N(0,sigma2) 

# ANOVA (DESBALANCEADO) ---------------------------------------------------------
# EXPERIMENTO DESBALANCEADO

k<-4 #tratamentos
n<-16 #tam amostral

C<-sum(data,na.rm=TRUE)^2/n # fator correcao
SQT<-sum(data^2,na.rm=TRUE)-C

T_<-as.numeric(apply(data,1,sum,na.rm=TRUE))
SQTrat<-sum(T_^2/c(5,4,3,4))-C

SQR<-SQT-SQTrat

QMTrat<-SQTrat/(k-1)

QMR<-SQR/(n-k)

F_<-QMTrat/QMR # 9,92

qf(0.95,k-1,n-k) # 3,49 (valor critico a 95% na F)

pf(9.92,k-1,n-k,lower.tail = FALSE) # p-valor

# a 5% de significância, rejeitamos a hipótese de que as médias entre os
#tratamentos sao iguais.

# COEFICIENTE DE VARIACAO ------------------------------------------------------

(sqrt(sum(e^2,na.rm=TRUE)/(n-k)))/mean(data,na.rm=TRUE)*100
# sqrt(QMR) # para encontrar s 

# [ADENDO] TESTES DE HOMOCEDASTICIDADE -----------------------------------------

# hartley.test{vartest} (experimentos balanceados)
# bartlett.test{stats} (pode ser usado em exp desbalanceados MAS normalidade deve ser atendida)
# levene.test{vartest} (dados nao precisam seguir normalidade)
