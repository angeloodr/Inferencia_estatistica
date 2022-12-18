# Angelo Orletti Del Rey 2022
# Este codigo foi utilizado como auxiliar na disciplina de bioestatística da Universidade Federal do ABC
# Aqui desenvolvo principalmente os conceitos basicos de estatistica descritiva e inferencial

#calculo de probabilidade e quantil das dististribuições normal e t
norm(0.4, 225, 37.5)
pnorm(262.5, 225,37.5) - pnorm(187.5, 225,37.5)

qt(0.025, 9)*(21.7/(9^(1/2)))+116.9
qt(0.975, 9)*(21.7/(9^(1/2)))+116.9

pt(0.025, 50)
pt(c(0.025, 0.975), 5)
pf()

# bioestatistica semana 6: realização de teste t #### 
#questao 1-4
antes = c(635,704,662,560,603,745,698,575, 633,699)
depois = c(640,712,681,558,610,740,707,585,935,682)

t.test(antes, depois,
       alternative = "less",
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.99)
#questao 5-8
drogaA=c(7,7,6,6,9,6,7,8,5)
drogaB=c(9,7,6,8,10,8,7,8,7)

t.test(drogaA, drogaB, 
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)
#quetao 9-14
marcaA = c(860,850,750,870,940,410,410,820,890,890)
marcaB = c(540,640,600,640,300,610,430,280,300,610)
var(marcaA)
var(marcaB)
fratio = var(marcaA)/var(marcaB)

t.test(marcaA, marcaB, 
       alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

#questao 15-17: normalização dos dados em z-score
n=100
mu=120
sigma = 24
xteste = 115
zteste= (mu-xteste)/(sigma/(n)^(1/2))
pnorm(zteste)

amostras = mu + sigma*scale(rnorm(100))

zpoder = qnorm(0.05)+abs(mu-xteste)*((n)^(1/2)/sigma)
pnorm(zpoder)                                                    

# quest�o 18 e 19 teste bilateral
rm(list = ls())
dev.off()
p0 = 0.02
pa = 0.05
qa = 1-pa
n=500
pa*qa*n
errpad = (pa*qa/n)^(1/2)
sigma = errpad*(n^(1/2))

zpoder = qnorm(0.05)+abs(p0-pa)*((n)^(1/2)/sigma)
pnorm(zpoder) 

namostras= (sigma^2*(qnorm(0.90)+qnorm(0.975))^2)/((p0-pa)^2)
namostras

# bioestatistica trabalho 1 #### 
rm(list = ls())  
dev.off()
ori = c(0.13,0.18,0.16,0.15,0.23,0.31)
oci = c(0.22,0.09,0.13,0.24,0.16,0.17)

t.test(oci, ori, var.equal = TRUE)

n= (var(oci)+var(ori)*(qnorm(0.8)+qnorm(0.975))^2)/((mean(oci)-mean(ori))^2)
n
zb = ((12*(mean(oci)-mean(ori))^2)/(var(oci)+var(ori)))^(1/2) - qnorm(0.975)
pnorm(zb)
      
# bioestatistica semana 10 ####
renda = c(10,15,12,70,80,100,20,30,10,60)
poupa = c(4 , 7, 5,20,20, 30, 8, 8, 3,15)
filho = c(8 , 6, 5, 1, 2,  2, 3 ,2, 6, 1)
cor.test(poupa, renda)
cor.test(filho, renda)

leite = c(26,25,31,29,27,31,32,28)
chuva = c(23,21,28,27,23,28,27,22)
summary(lm(leite ~ chuva)) #realização de regressão linear com chua predizendo leite

x = 24
y= 0.8 * x + 8.9 

# bioestatistica Trabalho 2: realização de teste de chi2 e regressão linear ####
getwd()
rm(list = ls())
dev.off()
install.packages("plotrix")                           # Install plotrix R package
library("plotrix")                                    # Load plotrix package
dados = read.csv("C:/Users/angel/OneDrive/�rea de Trabalho/IIE/Bioest_dados - Planilha1.csv", header=TRUE)

adictive = (dados$IAT_total >= 70) #addictive
problematic = (dados$IAT_total < 70 & dados$IAT_total >= 40) #problematic
normal = (dados$IAT_total < 40) #normal

# idades
mean(dados$Age[adictive])
std.error(dados$Age[adictive])
length(dados$Age[adictive & dados$Age>=6 & dados$Age<=9])
length(dados$Age[adictive & dados$Age>=10 & dados$Age<=14])
length(dados$Age[adictive & dados$Age>=15 & dados$Age<=18])
chisq.test(c(length(dados$Age[adictive & dados$Age>=6 & dados$Age<=9]),
             length(dados$Age[adictive & dados$Age>=10 & dados$Age<=14]),
             length(dados$Age[adictive & dados$Age>=15 & dados$Age<=18])))
mean(dados$Age[problematic])
std.error(dados$Age[problematic])
length(dados$Age[problematic & dados$Age>=6 & dados$Age<=9])
length(dados$Age[problematic & dados$Age>=10 & dados$Age<=14])
length(dados$Age[problematic & dados$Age>=15 & dados$Age<=18])
chisq.test(c(length(dados$Age[problematic & dados$Age>=6 & dados$Age<=9]),
             length(dados$Age[problematic & dados$Age>=10 & dados$Age<=14]),
             length(dados$Age[problematic & dados$Age>=15 & dados$Age<=18])))
mean(dados$Age[normal])
std.error(dados$Age[normal])
length(dados$Age[normal & dados$Age>=6 & dados$Age<=9])
length(dados$Age[normal & dados$Age>=10 & dados$Age<=14])
length(dados$Age[normal & dados$Age>=15 & dados$Age<=18])
chisq.test(c(length(dados$Age[normal & dados$Age>=6 & dados$Age<=9]),
             length(dados$Age[normal & dados$Age>=10 & dados$Age<=14]),
             length(dados$Age[normal & dados$Age>=15 & dados$Age<=18])))

#sexo biol�gico
length(dados$Gender[adictive & dados$Gender == 1])
length(dados$Gender[problematic & dados$Gender == 1])
length(dados$Gender[normal & dados$Gender == 1])
chisq.test(c(length(dados$Gender[adictive & dados$Gender == 1]),
             length(dados$Gender[problematic & dados$Gender == 1]),
             length(dados$Gender[normal & dados$Gender == 1])))
length(dados$Gender[adictive & dados$Gender == 2])
length(dados$Gender[problematic & dados$Gender == 2])
length(dados$Gender[normal & dados$Gender == 2])
chisq.test(c(length(dados$Gender[adictive & dados$Gender == 2]),
             length(dados$Gender[problematic & dados$Gender == 2]),
             length(dados$Gender[normal & dados$Gender == 2])))

#filho unico
length(dados$The_only_child[adictive & dados$The_only_child == 1])
length(dados$The_only_child[problematic & dados$The_only_child == 1])
length(dados$The_only_child[normal & dados$The_only_child == 1])
chisq.test(c(length(dados$The_only_child[adictive & dados$The_only_child == 1]) ,
             length(dados$The_only_child[problematic & dados$The_only_child == 1]),
             length(dados$The_only_child[normal & dados$The_only_child == 1])))

length(dados$The_only_child[adictive & dados$The_only_child == 2])
length(dados$The_only_child[problematic & dados$The_only_child == 2])
length(dados$The_only_child[normal & dados$The_only_child == 2])
chisq.test(c(length(dados$The_only_child[adictive & dados$The_only_child == 2]) ,
             length(dados$The_only_child[problematic & dados$The_only_child == 2]),
             length(dados$The_only_child[normal & dados$The_only_child == 2])))

# Residencia
length(dados$Residence[adictive & dados$Residence == 1])
length(dados$Residence[problematic & dados$Residence == 1])
length(dados$Residence[normal & dados$Residence == 1])
chisq.test(c(length(dados$Residence[adictive & dados$Residence == 1]),
             length(dados$Residence[problematic & dados$Residence == 1]),
             length(dados$Residence[normal & dados$Residence == 1])))
length(dados$Residence[adictive & dados$Residence == 2])
length(dados$Residence[problematic & dados$Residence == 2])
length(dados$Residence[normal & dados$Residence == 2])
chisq.test(c(length(dados$Residence[adictive & dados$Residence == 2]),
             length(dados$Residence[problematic & dados$Residence == 2]),
             length(dados$Residence[normal & dados$Residence == 2])))
length(dados$Residence[adictive & dados$Residence == 3])
length(dados$Residence[problematic & dados$Residence == 3])
length(dados$Residence[normal & dados$Residence == 3])
chisq.test(c(length(dados$Residence[adictive & dados$Residence == 3]),
             length(dados$Residence[problematic & dados$Residence == 3]),
             length(dados$Residence[normal & dados$Residence == 3])))
length(dados$Residence[adictive & dados$Residence == 4])
length(dados$Residence[problematic & dados$Residence == 4])
length(dados$Residence[normal & dados$Residence == 4])
chisq.test(c(length(dados$Residence[adictive & dados$Residence == 4]),
             length(dados$Residence[problematic & dados$Residence == 4]),
             length(dados$Residence[normal & dados$Residence == 4])))

# Regress�o linear Femea Tabela 5 ###
summary(lm(dados$IAT_total[dados$Gender == 2] ~
     dados$Age[dados$Gender == 2] +
     dados$Anxiety_classification_3[dados$Gender == 2] + 
     dados$Depression_classification_3[dados$Gender == 2] +
     dados$Stress_classification_3[dados$Gender == 2]))

###Tabela 3
# Depress�o
length(dados$Depression_classification_3[adictive & dados$Depression_classification_3 == 1])
length(dados$Depression_classification_3[problematic & dados$Depression_classification_3 == 1])
length(dados$Depression_classification_3[normal & dados$Depression_classification_3 == 1])
chisq.test(c(length(dados$Depression_classification_3[adictive & dados$Depression_classification_3 == 1]),
           length(dados$Depression_classification_3[problematic & dados$Depression_classification_3 == 1]),
           length(dados$Depression_classification_3[normal & dados$Depression_classification_3 == 1])))

length(dados$Depression_classification_3[adictive & dados$Depression_classification_3 == 2])
length(dados$Depression_classification_3[problematic & dados$Depression_classification_3 == 2])
length(dados$Depression_classification_3[normal & dados$Depression_classification_3 == 2])
chisq.test(c(length(dados$Depression_classification_3[adictive & dados$Depression_classification_3 == 2]),
             length(dados$Depression_classification_3[problematic & dados$Depression_classification_3 == 2]),
             length(dados$Depression_classification_3[normal & dados$Depression_classification_3 == 2])))

length(dados$Depression_classification_3[adictive & dados$Depression_classification_3 == 3])
length(dados$Depression_classification_3[problematic & dados$Depression_classification_3 == 3])
length(dados$Depression_classification_3[normal & dados$Depression_classification_3 == 3])
chisq.test(c(length(dados$Depression_classification_3[adictive & dados$Depression_classification_3 == 3]),
             length(dados$Depression_classification_3[problematic & dados$Depression_classification_3 == 3]),
             length(dados$Depression_classification_3[normal & dados$Depression_classification_3 == 3])))

# Ansiedade
length(dados$Anxiety_classification_3[adictive & dados$Anxiety_classification_3 == 1])
length(dados$Anxiety_classification_3[problematic & dados$Anxiety_classification_3 == 1])
length(dados$Anxiety_classification_3[normal & dados$Anxiety_classification_3 == 1])
chisq.test(c(length(dados$Anxiety_classification_3[adictive & dados$Anxiety_classification_3 == 1]),
             length(dados$Anxiety_classification_3[problematic & dados$Anxiety_classification_3 == 1]),
             length(dados$Anxiety_classification_3[normal & dados$Anxiety_classification_3 == 1])))

length(dados$Anxiety_classification_3[adictive & dados$Anxiety_classification_3 == 2])
length(dados$Anxiety_classification_3[problematic & dados$Anxiety_classification_3 == 2])
length(dados$Anxiety_classification_3[normal & dados$Anxiety_classification_3 == 2])
chisq.test(c(length(dados$Anxiety_classification_3[adictive & dados$Anxiety_classification_3 == 2]),
             length(dados$Anxiety_classification_3[problematic & dados$Anxiety_classification_3 == 2]),
             length(dados$Anxiety_classification_3[normal & dados$Anxiety_classification_3 == 2])))

length(dados$Anxiety_classification_3[adictive & dados$Anxiety_classification_3 == 3])
length(dados$Anxiety_classification_3[problematic & dados$Anxiety_classification_3 == 3])
length(dados$Anxiety_classification_3[normal & dados$Anxiety_classification_3 == 3])
chisq.test(c(length(dados$Anxiety_classification_3[adictive & dados$Anxiety_classification_3 == 3]),
             length(dados$Anxiety_classification_3[problematic & dados$Anxiety_classification_3 == 3]),
             length(dados$Anxiety_classification_3[normal & dados$Anxiety_classification_3 == 3])))

# Stress
length(dados$Stress_classification_3[adictive & dados$Stress_classification_3 == 1])
length(dados$Stress_classification_3[problematic & dados$Stress_classification_3 == 1])
length(dados$Stress_classification_3[normal & dados$Stress_classification_3 == 1])
chisq.test(c(length(dados$Stress_classification_3[adictive & dados$Stress_classification_3 == 1]),
             length(dados$Stress_classification_3[problematic & dados$Stress_classification_3 == 1]),
             length(dados$Stress_classification_3[normal & dados$Stress_classification_3 == 1])))
length(dados$Stress_classification_3[adictive & dados$Stress_classification_3 == 2])
length(dados$Stress_classification_3[problematic & dados$Stress_classification_3 == 2])
length(dados$Stress_classification_3[normal & dados$Stress_classification_3 == 2])
chisq.test(c(length(dados$Stress_classification_3[adictive & dados$Stress_classification_3 == 2]),
             length(dados$Stress_classification_3[problematic & dados$Stress_classification_3 == 2]),
             length(dados$Stress_classification_3[normal & dados$Stress_classification_3 == 2])))
length(dados$Stress_classification_3[adictive & dados$Stress_classification_3 == 3])
length(dados$Stress_classification_3[problematic & dados$Stress_classification_3 == 3])
length(dados$Stress_classification_3[normal & dados$Stress_classification_3 == 3])
chisq.test(c(length(dados$Stress_classification_3[adictive & dados$Stress_classification_3 == 3]),
             length(dados$Stress_classification_3[problematic & dados$Stress_classification_3 == 3]),
             length(dados$Stress_classification_3[normal & dados$Stress_classification_3 == 3])))


