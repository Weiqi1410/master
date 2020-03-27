rm(list=ls())
#install.packages('RCurl')
#require(RCurl)
#install.packages('synchrony')
#require(synchrony)
#install.packages('quantmod')
#require(quantmod)
#install.packages('rMVP')
#require(xts)
#install.packages('IntroCompFinR')
#library(IntroCompFinR)
#install.packages("tidyverse")
#require(tidyverse)
#install.packages("tidyquant")
#library(tidyquant)
#library(dplyr)

dax.Kurs <- read.csv("F:/Desktop/SACHEN/Uni/Master/Seminar ML/DAX_Kurse.csv",sep = ";",header=T,dec = ",")
rownames(dax.Kurs) <- as.Date(dax.Kurs[,1])
summary(dax.Kurs)


dax.ret <- read.csv("F:/Desktop/SACHEN/Uni/Master/Seminar ML/DAX_ret.csv",sep = ";",header=T,dec = ",")
rownames(dax.ret) <- as.Date(dax.ret[,1])
dax.ret <- dax.ret[,-1]
summary(dax.ret)

par(mfrow=c(1,1))
matplot(c(dax.Kurs[,1]), dax.Kurs[,-1],type = "l",xlab = "Date", ylab = "Kurs",lwd = 2,col=c(1:30), main="Komponente von DAX im Zeitraum 01.10.2015 bis 01.03.2020 (monatlich)")
legend("topleft", legend=colnames(dax.Kurs[,-1]), col=c(1:30), lty=1:30, cex=0.6)
#,xlim = c("2015-10-01", "2020-03-01")
#ggplot(dax.Kurs, aes(x, y)) + geom_line(aes(dax.Kurs[,1], dax.Kurs[,-1])) + xlim(as.Date("2015-10-01"), as.Date("2020-03-01"))



###### Benchmarking
### original
weights.ori <- c(0.0058 , 0.0462 , 0.0689 , 0.0452 , 0.0594, 0.0273 , 0.0301 , 0.0138 , 0.0293 , 0.0141 ,
                 0.000138 , 0.0309 , 0.0694 , 0.022 , 0.0207 , 0.0191 , 0.008 , 0.0149 , 0.0173 , 0.0053 ,
                 0.0972 , 0.0133 , 0.0272 , 0.0152 , 0.01353 , 0.0687 , 0.0084 , 0.0275 , 0.0234 , 0.0129)

### MEF
weights.mef <- c(0.019651004, 0.007179677, -0.029499803, 0.010858059, -0.014084884, -0.008484779, -0.004935069, 
                 -0.007257772, -0.025827618, 0.010134729, 0.116159230, 0.037632726, 0.370867422, 0.375384443, 
                 -0.011979158, 0.007023623, -0.016027989, 0.010121070, 0.130208846, -0.007661192, 0.002154779, 
                 -0.032002981, 0.011099092, -0.049271375,  0.012525109, -0.019904976, 0.114930901, -0.001624795,
                 -0.002868131, -0.004500188)

### PCA
weights.pca <- c(0.0365011064,0.0087605257,0.0611358541,0.0697815171,0.0380174903,0.0064774259,0.0619043027,
                 0.0560854559,0.0666476844,0.0051680088,0.0269074623,0.0592113117,0.0167983894,0.0085083857, 
                 0.0300560148,0.0350420727,0.0605367138,0.0237580220,0.0504645111,0.0273590012,0.0002472525,
                 0.0239857157,0.0357214478,0.0014099898,0.0446791118,0.0536800302,0.0371257139,0.0021319912,
                 0.0502614688,0.0016360225)

### RPM
weights.rpa <- c(0.015751273,0.007868252,0.005735380,0.012175770,0.011239881,0.026743672,0.009484065,
                 0.004539139,0.011497239,0.026552312,0.068310719,0.027299532,0.148480588,0.172994520,
                 0.014964043,0.019987214,0.013775199,0.017343222,0.038958389,0.048029052,0.035032008,
                 0.012523273,0.007330502,0.086211625,0.011998862,0.008987812,0.048651101,0.068376091,
                 0.005588138,0.013571126)




### Summe der Fehler
mef.error <- sum(abs(weights.mef-weights.ori)); mef.error
pca.error <- sum(abs(weights.pca-weights.ori)); pca.error
rpa.error <- sum(abs(weights.rpa-weights.ori)); rpa.error


### Summe der quadrierten Fehler
mef.error2 <- sum((weights.mef-weights.ori)^2);mef.error2
pca.error2 <- sum((weights.pca-weights.ori)^2);pca.error2
rpa.error2 <- sum((weights.rpa-weights.ori)^2);rpa.error2
