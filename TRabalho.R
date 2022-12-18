# este codigo foi utilizado para o trabalho final da disciplina de introdução a inferencia estatistica
# do bacharelado em neurociencia da UFABC. Aqui o objetivo era reproduzir os graficos da figura 5
# feitos no artigo de Blot, A. e Barbour, B.  2014. 

rm(list = ls())
dev.off()

#### PRÉ PROCESSAMENTO DOS DADOS ####
#### lendo os dados
afsoma = read.csv("C:/Users/angel/OneDrive/�rea de Trabalho/IIE/Figura5 - soma stm a-f.csv", header=TRUE)
afaxon = read.csv("C:/Users/angel/OneDrive/�rea de Trabalho/IIE/Figura5 - axon stm a-f.csv", header=TRUE)
gsoma = read.csv("C:/Users/angel/OneDrive/�rea de Trabalho/IIE/Figura5 - soma stm g.csv", header=TRUE)
gaxon = read.csv("C:/Users/angel/OneDrive/�rea de Trabalho/IIE/Figura5 - axon stm g.csv", header=TRUE)
himod = read.csv("C:/Users/angel/OneDrive/�rea de Trabalho/IIE/Figura5 - firing-modulation-(Hz-green-trace)-h-i.csv", header=TRUE)
hivolt = read.csv("C:/Users/angel/OneDrive/�rea de Trabalho/IIE/Figura5 - voltage-near-axon-(microV)-h-i.csv", header=TRUE)

#### colocando os dados como dados numéricos
# Esse pré processamento é necessário para primeiramente retirar os pontos de separação que 
# os dados tinham e transformar a sting do valor em numeros para que possamos fazer manipulações

#### fig a-f
voltsomaAF = c()
for (ii in c(1:length(afsoma$V.axon.soma.stm.a.f))) {
  kk = strsplit(afsoma$V.axon.soma.stm.a.f[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
         numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  voltsomaAF= c(voltsomaAF, numb)
}

modsomaAF = c()
for (ii in c(1:length(afsoma$Firing.modulation.soma.stm.a.f))) {
  kk = strsplit(afsoma$Firing.modulation.soma.stm.a.f[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  modsomaAF= c(modsomaAF, numb)
}

voltaxonAF = c()
for (ii in c(1:length(afaxon$V.axon.axon.stm.a.f))) {
  kk = strsplit(afaxon$V.axon.axon.stm.a.f[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  voltaxonAF= c(voltsomaAF, numb)
}

modaxonAF = c()
for (ii in c(1:length(afaxon$Firing.modulation.axon.stm.a.f))) {
  kk = strsplit(afaxon$Firing.modulation.axon.stm.a.f[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  modaxonAF= c(modaxonAF, numb)
}

##### fig G
voltaxonG = c()
for (ii in c(1:length(gaxon$V.axon.axon.stm.g))) {
  kk = strsplit(gaxon$V.axon.axon.stm.g[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  voltaxonG= c(voltaxonG, numb)
}


modaxonG = c()
for (ii in c(1:length(gaxon$Firing.modulation.axon.stm.g))) {
  kk = strsplit(gaxon$Firing.modulation.axon.stm.g[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  modaxonG= c(modaxonG, numb)
}

voltsomaG = c()
for (ii in c(1:length(gsoma$V.axon.soma.stm.g))) {
  kk = strsplit(gsoma$V.axon.soma.stm.g[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  voltsomaG= c(voltsomaG, numb)
}


modsomaG = c()
for (ii in c(1:length(gsoma$Firing.modulation.soma.stm.g))) {
  kk = strsplit(gsoma$Firing.modulation.soma.stm.g[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  modsomaG= c(modsomaG, numb)
}

#### fig h-i
modtimeHI = c()
for (ii in c(1:length(himod$time..ms..h.i))) {
  kk = strsplit(himod$time..ms..h.i[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  modtimeHI= c(modtimeHI, numb)
}

modgreenHI = c()
for (ii in c(1:length(himod$firing.modulation..Hz.green.trace..h.i))) {
  if (ii<=70){
    bruto = himod$firing.modulation..Hz.green.trace..h.i[ii]
    numero = gsub(",","",bruto)
    numero = as.numeric(numero)
    numero = numero*100 # isso readequa o dado em not. cientifica para not. usual
    modgreenHI= c(modgreenHI, numero)
  }
  if (ii>70){
    kk = strsplit(himod$firing.modulation..Hz.green.trace..h.i[ii],"")
    numb = ""
    naoo= "."
    for (st in kk) {
      for (nd in st) {
        if (nd != naoo) {
          numb = paste(numb,nd, sep="")
        }
      }
    }
    numb = as.numeric(numb)
    modgreenHI= c(modgreenHI, numb)
  } 
}

volttimeHI = c()
for (ii in c(1:length(hivolt$time..ms..h.i))) {
  kk = strsplit(himod$time..ms..h.i[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  volttimeHI= c(volttimeHI, numb)
}


voltvoltHI = c()
for (ii in c(1:length(hivolt$voltage.near.axon..microV..h.i))) {
  kk = strsplit(hivolt$voltage.near.axon..microV..h.i[ii],"")
  numb = ""
  naoo= "."
  for (st in kk) {
    for (nd in st) {
      if (nd != naoo) {
        numb = paste(numb,nd, sep="")
      }
    }
  }
  numb = as.numeric(numb)
  voltvoltHI= c(voltvoltHI, numb)
}

#### PROCESSAMENTO DOS DADOS E PLOTAGEM DOS GRAFICOS ###
# alguns dos dados que eu obtive não faziam sentido e portanto plotei somente aqueles que conseguia 
# extrair algo de util para minhas análises

#### fig a-f
plot(c(1:length(voltsomaAF)), voltsomaAF, "l", xlab = "Time (ms)", ylab = "Potencial pr�ximo ao ax�nion (microV)")
plot(c(1:length(modsomaAF)), modsomaAF, "l", xlab = "Time (ms)", ylab = "Modula��o de disparo (Hz)")

#### fig g
# adequação para valores de micro volts
voltaxonG = voltaxonG*(10^(-9))
modaxonG = modaxonG*(10^(-10))

voltsomaG = voltsomaG*(10^(-9))
modsomaG = modsomaG*(10^(-10))

allvoltG = c(voltaxonG, voltsomaG)
allmodG = c(modaxonG, modsomaG)

# realização do metodo de reamostragem de bootstrap para o calculo dos coeficientes da regressão
# linear, reproduzindo o grafico G
alfa_bs=vector()
beta_bs=vector()
for (b in 1:10000){
  idx_bs = sample( 1:length(allmodG) , length(allmodG) , replace=TRUE)
  volt_bs = allvoltG[idx_bs]
  mod_bs = allmodG[idx_bs]
  fit_bs = lm( mod_bs ~ volt_bs)
  alfa_bs[b] = fit_bs[["coefficients"]][1]
  beta_bs[b] = fit_bs[["coefficients"]][2]
}

#seleção do intervalo de confianca de 95%
a = quantile(alfa_bs, c(0.025,0.975))
b = quantile(beta_bs, c(0.025,0.975))

#plotagem dos gráficos
plot(voltaxonG, modaxonG, xlim = c(-600,900), ylim = c(-90,75), col="blue", xlab = "", ylab = "" )
par(new=TRUE)
plot(voltsomaG, modsomaG, xlim = c(-600,900), ylim = c(-90,75), col="black", 
     xlab = "Potencial pr�ximo ao ax�nion (microV)", ylab = "Modula��o de disparo (Hz)")
par(new=TRUE)
abline(lm(allmodG ~ allvoltG))
par(new=TRUE)
abline(a["2.5%"], b["2.5%"], col="grey")
par(new=TRUE)
abline(a["97.5%"], b["97.5%"],  col="grey")


#### fig h-i

plot(modtimeHI, modgreenHI,"l", xlab = "Tempo (ms)", ylab = "Modula��o de disparo (Hz)")
plot(volttimeHI,voltvoltHI,"l", xlab = "Tempo (ms)", ylab = "Potencial pr�ximo ao ax�nion (microV)")


