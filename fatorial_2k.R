# Fatorial 2^k -----------------------------------------------------------------

# Exemplo com k = 3 ------------------------------------------------------------

# NPK (Nitrogenio, Fosforo, Potassio)
# ABC

I<-2
J<-2
K<-2
R<-6 # blocos

# Graus de Liberdade -----------------------------------------------------------

GL_A<-I-1
GL_B<-J-1
GL_C<-K-1

GL_AB<-GL_A*GL_B
GL_AC<-GL_A*GL_C
GL_BC<-GL_B*GL_C
GL_ABC<-GL_A*GL_B*GL_C

GL_trat<-I*J*K-1
GL_bloco<-R-1

GL_tot<-I*J*K*R-1
GL_res<-GL_tot-GL_trat-GL_bloco # GL_res == GL_trat* GL_bloco

# Somas de Quadrados -----------------------------------------------------------

#C<-Tot_geral^2/IJKR
C<-1946.9^2/(I*J*K*R)

# Fator (A)
T_A0<-857.1
T_A1<-1089.8
SQA<-(1/(J*K*R))*(T_A0^2+T_A1^2) - C

# Fator (B)
T_B0<-957.4
T_B1<-989.5
SQB<-(1/(I*K*R))*(T_B0^2+T_B1^2) - C

# Fator (C)
T_C0<-882.3
T_C1<-1064.6
SQC<-(1/(I*J*R))*(T_C0^2+T_C1^2) - C

# Fator A vs B (A vs B)
SQAB<-1/(K*R)*(407^2+450.1^2+550.4^2+539.4^2)-C-SQA-SQB

# Fator A vs C (A vs C)
SQAC<-1/(J*R)*(436.7^2+420.4^2+445.6^2+644.2^2)-C-SQA-SQC

# Fator B vs C
SQBC<-1/(I*R)*(420.6^2+461.7^2+536.8^2+527.8^2)-C-SQB-SQC

# Tratamento e Fator A vs B vs C
SQTrat<-1/(R)*(206.2**2+200.8**2+230.5**2+219.6**2+214.4**2+336**2+231.2**2+308.2**2)-C 
SQABC<-SQTrat - SQA - SQB - SQC - SQAB - SQAC - SQBC
# SQTrat == SQA + SQB + SQC + SQAB + SQAC + SQBC + SQABC

# Blocos
SQBloco<-1/(I*J*K)*(308.3**2+351.4**2+324.5**2+338.8**2+298.2**2+325.7**2)-C

# Total
T_2<-c((31.8^2), (40.5^2), (25.7^2), (25.7^2), (37.2^2), (45.3^2),  
(25.6^2), (32.4^2), (39.6^2), (48.9^2), (20.6^2), (33.7^2),  
(36.2^2), (37.8^2), (40.9^2), (44.8^2), (32.4^2), (38.4^2),  
(37.1^2), (53.0^2), (36.4^2), (43.0^2), (19.7^2), (30.4^2),  
(35.3^2), (39.0^2), (36.0^2), (33.5^2), (28.2^2), (42.4^2),  
(51.5^2), (66.1^2), (51.7^2), (52.0^2), (56.5^2), (58.2^2),  
(43.8^2), (32.7^2), (43.3^2), (41.8^2), (31.9^2), (37.7^2),  
(47.0^2), (49.9^2), (50.9^2), (49.1^2), (71.7^2), (39.6^2)) 
SQTot<-sum(T_2)-C

# Residuos
SQRes = SQTot - SQTrat - SQBloco

# Quadrados Medios -------------------------------------------------------------

# QM = SQ/GL
# PARA OS FATORES E INTERACOES: 
# SQ IGUAL AO QM pois os niveis sao iguais a 02, logo GL = 1  (fatorial 2^k)

QMTrat<-SQTrat/GL_trat
QMBloco<-SQBloco/GL_bloco
QMRes<- SQRes/GL_res

# Estatistica F ----------------------------------------------------------------

f_A<-SQA/QMRes
f_B<-SQB/QMRes
f_C<-SQC/QMRes  
f_AB<-SQAB/QMRes  
f_AC<-SQAC/QMRes
f_BC<-SQBC/QMRes
f_ABC<-SQABC/QMRes

f_trat<-QMTrat/QMRes

qf(0.95,1,GL_res)
