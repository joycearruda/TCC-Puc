
##################### IMPORTAÇÕES DAS BASES ##########################

### IMPORTAÇÃO DA 1ª BASE DE DADOS 

escolas = read.csv('escolas19.csv')



### COLOCAR AS VARIÁVEIS NAS COLUNAS

### INSTALAÇÃO E CARREGAMENTO DO PACOTE

install.packages('readr')
library(readr)


### SEPARAÇÃO DAS COLUNAS

escolas = read_delim("escolas19.csv", "|", escape_double = FALSE, trim_ws = TRUE)



# Identificação do tipo de variável

head(escolas)


### FILTRO DAS ESCOLAS DE ENSINO MÉDIO


install.packages('dplyr')
library(dplyr)

escolas2 = escolas[escolas$IN_COMUM_MEDIO_MEDIO == 1,]



### CARACTERÍSTICAS QUE CONSIDERAM AS ESCOLAS "ADEQUADA/AVANÇADA"


escolas3 = escolas2 %>% dplyr :: select(CO_ENTIDADE, CO_REGIAO, IN_AGUA_POTAVEL, IN_BANHEIRO, IN_COZINHA, 
                                        IN_ESGOTO_REDE_PUBLICA, IN_ENERGIA_REDE_PUBLICA, IN_EQUIP_TV, 
                                        IN_EQUIP_DVD, IN_COMPUTADOR, IN_EQUIP_IMPRESSORA, IN_BIBLIOTECA, 
                                        IN_INTERNET, IN_SALA_PROFESSOR, IN_LABORATORIO_CIENCIAS, 
                                        IN_LABORATORIO_INFORMATICA, IN_QUADRA_ESPORTES)



### IDENTIFICAÇÃO DOS NA's DA BASE escolas3


is.na(NA)
colSums(is.na(escolas3))



### EXCLUSÃO DOS NA's DA BASE escola3

escolas3 = na.omit(escolas3)




### IMPORTAÇÃO DA 2ª BASE DE DADOS


### INSTALAÇÃO E CARREGAMENTO DE PACOTE PARA LER EXCEL


install.packages("readxl")
library("readxl")


### IMPORTAÇÃO DA 2ª BASE DE DADOS

notas = read_excel("notas do enem por escola.xlsx")


head(notas)


notas2 = read_excel("notas do enem por escola.xlsx", 
                    col_types = c("skip", "numeric", "text", 
                                  "text", "text", "text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric"))




### IDENTIFICAÇÃO DOS NA's DA BASE notas2

is.na(NA)
colSums(is.na(notas2))



notas2 = na.omit(notas2)




### CONSTRUÇÃO DO DATASET A SER UTILIZADO NO TCC


base = merge(escolas3, notas2, by.x = 'CO_ENTIDADE', by.y = 'COD_INEP')




################## ANÁLISE EXPLORATÓRIA DOS DADOS   #################



range(base$NOTA_ENEM)

nclass.Sturges(base$NOTA_ENEM)

table(cut(base$NOTA_ENEM, seq(408, 717, l = 17)))

summary(base$NOTA_ENEM)




install.packages('psych')
library(psych)



describe(base$NOTA_ENEM)


describeBy(base$NOTA_ENEM, group = base$'DEPENDENCIA ADMINISTRATIVA')

describeBy(base$NOTA_ENEM, group = base$'URBANA E RURAL')


### PROPORÇÃO DOS EQUIPAMENTOS NAS ESCOLAS


X = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
apply(X = base[,X], MARGIN = 2, FUN = mean)


### TABELA DE REFERÊNCIAS CRUZADAS


table(base$IN_LABORATORIO_CIENCIAS, base$IN_LABORATORIO_INFORMATICA)
prop.table(table(base$IN_LABORATORIO_CIENCIAS, base$IN_LABORATORIO_INFORMATICA))

table(base$IN_BIBLIOTECA, base$IN_LABORATORIO_INFORMATICA)
prop.table(table(base$IN_BIBLIOTECA, base$IN_LABORATORIO_INFORMATICA))

table(base$IN_BIBLIOTECA, base$IN_QUADRA_ESPORTES)
prop.table(table(base$IN_BIBLIOTECA, base$IN_QUADRA_ESPORTES))




### TABELA DOS EQUIPAMENTOS POR TIPO DE DEPENDÊNCIA ADMINISTRATIVA

d = as.data.frame(aggregate(base$IN_BIBLIOTECA ~ base$`DEPENDENCIA ADMINISTRATIVA`, base, FUN = mean, na.rm = TRUE))
d


d = as.data.frame(aggregate(base$IN_LABORATORIO_CIENCIAS ~ base$`DEPENDENCIA ADMINISTRATIVA`, 
                            base, FUN = mean, na.rm = TRUE))
d


d = as.data.frame(aggregate(base$IN_LABORATORIO_INFORMATICA ~ base$`DEPENDENCIA ADMINISTRATIVA`, 
                            base, FUN = mean, na.rm = TRUE))
d


d = as.data.frame(aggregate(base$IN_QUADRA_ESPORTES ~ base$`DEPENDENCIA ADMINISTRATIVA`, 
                            base, FUN = mean, na.rm = TRUE))
d


### POR TIPO DE LOCALIZAÇÃO URBANA E RURAL


d = as.data.frame(aggregate(base$IN_BIBLIOTECA ~ base$`URBANA E RURAL`, base, FUN = mean, na.rm = TRUE))
d



d = as.data.frame(aggregate(base$IN_LABORATORIO_CIENCIAS ~ base$`URBANA E RURAL`, base, FUN = mean, na.rm = TRUE))
d


d = as.data.frame(aggregate(base$IN_LABORATORIO_INFORMATICA ~ base$`URBANA E RURAL`, base, FUN = mean, na.rm = TRUE))
d



d = as.data.frame(aggregate(base$IN_QUADRA_ESPORTES ~ base$`URBANA E RURAL`, base, FUN = mean, na.rm = TRUE))
d



### POR REGIÃO GEOGRÁFICA



f = as.data.frame(aggregate(base$IN_BIBLIOTECA ~ base$CO_REGIAO, base, FUN = mean, na.rm = TRUE))
row.names(f) = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul')
f



f = as.data.frame(aggregate(base$IN_LABORATORIO_CIENCIAS ~ base$CO_REGIAO, base, FUN = mean, na.rm = TRUE))
row.names(f) = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul')
f




f = as.data.frame(aggregate(base$IN_LABORATORIO_INFORMATICA ~ base$CO_REGIAO, base, FUN = mean, na.rm = TRUE))
row.names(f) = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul')
f



f = as.data.frame(aggregate(base$IN_QUADRA_ESPORTES ~ base$CO_REGIAO, base, FUN = mean, na.rm = TRUE))
row.names(f) = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul')
f




boxplot(base$NOTA_ENEM ~ as.factor(base$`DEPENDENCIA ADMINISTRATIVA`), 
        main = 'Distribuição das notas das escolas por tipo de dependência administrativa', 
        xlab = 'Dependência Administrativa', ylab = 'Notas do Enem')




boxplot(base$NOTA_ENEM ~ as.factor(base$`URBANA E RURAL`), 
        main = 'Distribuição das notas das escolas por localização (Urbana x Rural)', 
        xlab = 'Urbana e Rural', ylab = 'Notas do Enem')




boxplot(base$NOTA_ENEM ~ as.factor(base$`CO_REGIAO`), 
        main = 'Distribuição das notas das escolas por região)', 
        xlab = 'Região', ylab = 'Notas do Enem')





### ANOVA - MODELO1  

install.packages("dplyr")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("psych")
install.packages("rstatix")
install.packages("DescTools")
install.packages("nortest")
library(dplyr)
library(car)
library(psych)
library(rstatix)
library(DescTools)
library(RVAideMemoire)
library(nortest)



base2 = base %>% dplyr :: select(CO_ENTIDADE, IN_BIBLIOTECA, 
                                 IN_LABORATORIO_CIENCIAS, IN_LABORATORIO_INFORMATICA, IN_QUADRA_ESPORTES, NOTA_ENEM)



glimpse(base2)


base2 = transform(base2,IN_BIBLIOTECA=as.factor(IN_BIBLIOTECA), 
                  IN_LABORATORIO_CIENCIAS=as.factor(IN_LABORATORIO_CIENCIAS),
                  IN_LABORATORIO_INFORMATICA=as.factor(IN_LABORATORIO_INFORMATICA), 
                  IN_QUADRA_ESPORTES=as.factor(IN_QUADRA_ESPORTES))

glimpse(base2)


### ANÁLISE DE OUTLIERS 

hist(sqrt(base2$NOTA_ENEM))
summary(sqrt(base2$NOTA_ENEM))



### COEFICIENTE DE ASSIMETRIA 

pondera = (mean(base2$NOTA_ENEM)-median(base2$NOTA_ENEM))/sd(base2$NOTA_ENEM)
pondera


#### COEFICIENTE DE ASSIMETRIA - QUARTIS 

quantil = (quantile(base2$NOTA_ENEM,0.75)+quantile(base2$NOTA_ENEM,0.25)-2*median(base2$NOTA_ENEM))/
        (quantile(base2$NOTA_ENEM,0.75)-quantile(base2$NOTA_ENEM,0.25))
quantil


q1 = (1.5+quantil)*quantile(base2$NOTA_ENEM,0.25)
q2 = (1.5-quantil)*quantile(base2$NOTA_ENEM,0.75)



base2 = base2[base2$NOTA_ENEM<q2,]



anova1 = aov(NOTA_ENEM ~.,base2[,-1])
summary(anova1)


modelo1 = (lm(NOTA_ENEM~.,base2[,-1]))
summary(modelo1)




#### ANÁLISE DOS PRESSUPOSTOS DO MODELO####


##### TESTAR A HOMOCEDASTICIDADE 


install.packages("lmtest")
library(lmtest)


bptest(modelo1)



#### TESTAR A NORMALIDADE DOS RESÍDUOS
install.packages("nortest")
library(nortest)


ad.test(residuals(modelo1))



leveneTest(NOTA_ENEM ~ IN_BIBLIOTECA, base2, center=mean)
leveneTest(NOTA_ENEM ~ IN_LABORATORIO_CIENCIAS, base2, center=mean)
leveneTest(NOTA_ENEM ~ IN_LABORATORIO_INFORMATICA, base2, center=mean)
leveneTest(NOTA_ENEM ~ IN_QUADRA_ESPORTES, base2, center=mean)


### AJUSTE DO MODELO ANOVA


anova_media2 = aov(log(NOTA_ENEM) ~.,base2[,-1])
summary(anova_media2)


modelo2 = (lm(log(NOTA_ENEM)~.,base2[,-1]))
summary(modelo2)


#### TESTAR A NORMALIDADE DOS RESÍDUOS

ad.test(residuals(modelo2))


##### TESTAR A HOMOCEDASTICIDADE 


bptest(modelo2)

### TESTAR A ESFERACIDADE DOS ERROS


leveneTest(log(NOTA_ENEM) ~ IN_BIBLIOTECA, base2, center=mean)
leveneTest(log(NOTA_ENEM) ~ IN_LABORATORIO_CIENCIAS, base2, center=mean)
leveneTest(log(NOTA_ENEM) ~ IN_LABORATORIO_INFORMATICA, base2, center=mean)
leveneTest(log(NOTA_ENEM) ~ IN_QUADRA_ESPORTES, base2, center=mean)



### KMEANS - KMODES - MODELO2


install.packages (c("klaR", "MASS", "DescTools"))
library(klaR)
library(MASS)
library(DescTools)



set.seed(2021)



m = kmeans(base2[,c(2:5)],5)

m$centers



### K-MODES PARA AS VARIÁVEIS CATEGÓRICAS

m1 = kmodes(base2[,c(2:5)],5) 

m1$modes ## clusters

m1

### K-MEANS PARA A VARIÁVEL CONTÍNUA - NOTA_ENEM

m2 = kmeans(base2$NOTA_ENEM, 5) 

m2$centers

m2


aux = cbind.data.frame(m1$cluster, m2$cluster)

colnames(aux) = c('cluster de infraestrutura', 'cluster de nota do enem')

x = as.matrix.data.frame(table(aux$`cluster de nota do enem`,aux$`cluster de infraestrutura`))
x = as.data.frame(x)



x = x %>% dplyr :: select(V4, V2, V5, V1, V3)



x1 = as.data.frame(t(x)) %>%  dplyr :: select(V2, V5, V4, V3, V1)
x1 = t(x1)

rownames(x1) = c('cluster de nota 1', 'cluster de nota 2', 'cluster de nota 3', 
                 'cluster de nota 4', 'cluster de nota 5')
colnames(x1) = c('cluster de infraestrutura 1', 'cluster de infraestrutura 2', 'cluster de infraestrutura 3', 
                 'cluster de infraestrutura 4', 'cluster de infraestrutura 5')

View(x1)


### TESTE QUI-QUADRADO


chisq.test(x1)


