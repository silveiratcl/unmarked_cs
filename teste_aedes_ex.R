
library("unmarked")

load("unmarked_data.RData")

effort
predictors






esforco <- sqrt(aegypti_esf)# transformar
range(esforco)
esforco_st <- (esforco - mean(as.matrix(esforco)))/sd(as.matrix(esforco))# padronizar esforco
range(esforco_st)

## formatar os dados de acordo com os requerimentos do pacote
## y eh uma matriz, com os sítios nas linhas, dias de amostragem nas colunas
## siteCovs deve ser um dataframe, onde cada linha eh um si?tio e cada coluna uma covariavel
## obsCovs deve ser uma lista, onde cada linha eh um si?tio e cada elemento equivale a uma covariavel

unmarked_data <- unmarkedFrameOccu(y=aegypti_det,
                                   siteCovs = cbind(data.frame (floresta=floresta_st, populacao=populacao_st, #covs de site
                                                                temp_ac=temp_ac_st, prec_ac=prec_ac_st)),
                                   obsCovs = list(temp_sem=temp_sem_st,temp_ant=temp_ant_st, # requer uma lista
                                                  prec_sem=prec_sem_st, prec_ant=prec_ant_st,
                                                  esforco=esforco_st))

## Com base em algum conhecimento que você tenha sobre a biologia de mosquitos,
## construa diferentes modelos representando hipoteses alternativas
## para definir os fatores explicando a deteccao e ocupacao de Aedes aegypti.
## Veja os exemplos abaixo
## Ajuste os modelos seguindo este formato: occu (~ deteccao ~ocupacao, dados)
mod1 <- occu(~1 ~1, unmarked_data)

mod2 <- occu(~ esforco  ~floresta, unmarked_data)
#esforço e gente
mod3 <- occu(~ esforco + populacao  ~ floresta, unmarked_data) 

#esforço e gente + forest e precipitacao
mod4 <- occu(~ esforco + populacao  ~ floresta * prec_ac, unmarked_data) 

#detectabildiade é uma combinação do esforco com a temp acumulada
mod5 <- occu(~ esforco   ~ floresta + prec_ac + temp_ac , unmarked_data)




## fazer uma lista de modelos
## nomeie cada modelo de acordo com cada covariavel de cada componente dos seus modelos hierarquicos
lista_modelos <- fitList('p(.)psi(.) null'= mod1, 
                         'p(esf)psi(forest) M2'= mod2,
                         'p(esf)p(pop)psi(forest) M3'= mod3, 
                         'p(esf)p(pop)psi(forest)*(prec) M4'= mod4,  
                         'p(esf)psi(forest)psi(p_ac)psi(t_ac) M5'= mod5)  #fazer com o mod5 

## rankear os modelos de acordo com AIC (quanto menor o valor de AIC, melhor o modelo se ajusta aos dados)
## Mostrar tabela de selecao de modelos
selecao_modelos <- modSel(lista_modelos,nullmod='p(.)psi(.) null')
selecao_modelos

## segundo o melhor modelo, qual a probabilidade ocupacao quando a variavel de sitio estao na media
backTransform(linearComb(mod5, c(1,0,0,0), type="state"))

## qual eh a probabilidade de deteccao quando o esforco amostral esta na media
backTransform(linearComb(mod5, c(1,0), type="det"))


# Fazer com o melhor modelo
## construa graficos com valores preditos de ocupacao e deteccao de acordo com os fatores de cada componente do modelo
## obter dados preditos de acordo com o range de valores das covariaveis padronizadas
## 

dados_novos<- data.frame (floresta=seq(range(floresta_st)[1],range(floresta_st)[2],0.01)) # colocar isso  floresta + prec_ac + temp_ac


prec_ac_st_cut=prec_ac_st[1:496, 1]
temp_ac_st_cut=temp_ac_st[1:496, 1]
str(floresta_st)
str(prec_ac_st_cut)
str(temp_ac_st_cut)

dados_novos<- data.frame (floresta=seq(range(floresta_st)[1],range(floresta_st)[2],length.out=496),
                          prec_ac=seq(range(prec_ac_st_cut)[1],range(prec_ac_st_cut)[2],length.out=496),
                          temp_ac=seq(range(temp_ac_st_cut)[1], range(temp_ac_st_cut)[2], length.out=496))



pred_occur<- predict (mod5,type="state", dados_novos,append=T)

dados_novos<- data.frame (esforco=seq(range(esforco_st)[1],range(esforco_st)[2],0.01))
pred_detec<- predict(mod2,type="det", dados_novos,append=T)

## construa os plots

### Floresta
## ocupacao
par(mfrow=c(3,2))
plot(pred_occur$floresta,pred_occur$Predicted,type="l",lwd=3,xlab="Forest cover",
     ylab=expression (paste (italic("Aedes aegypti "), "occupancy (",Psi,")",sep=" ",
                             ylim=c(0,1))))
lines(pred_occur$floresta,pred_occur$lower)
lines(pred_occur$floresta,pred_occur$upper)

## deteccao
plot(pred_detec$esforco,pred_detec$Predicted,type="l",lwd=3,xlab="Sampling effort (number of eppendorfs)",
     ylab=expression (paste (italic("Aedes aegypti "), "detection (p)",sep=" ")))
lines(pred_detec$esforco,pred_detec$lower)
lines(pred_detec$esforco,pred_detec$upper)

### Precipitacao acumulada

plot(pred_occur$prec_ac,pred_occur$Predicted,type="l",lwd=3,xlab="Precipitação Acumulada",
     ylab=expression (paste (italic("Aedes aegypti "), "occupancy (",Psi,")",sep=" ",
                             ylim=c(0,1))))
lines(pred_occur$prec_ac,pred_occur$lower)
lines(pred_occur$prec_ac,pred_occur$upper)

## deteccao
plot(pred_detec$esforco,pred_detec$Predicted,type="l",lwd=3,xlab="Sampling effort (number of eppendorfs)",
     ylab=expression (paste (italic("Aedes aegypti "), "detection (p)",sep=" ")))
lines(pred_detec$esforco,pred_detec$lower)
lines(pred_detec$esforco,pred_detec$upper)

### temperatura
### Precipitacao acumulada

plot(pred_occur$temp_ac,pred_occur$Predicted,type="l",lwd=3,xlab="Temperatura Acumulada",
     ylab=expression (paste (italic("Aedes aegypti "), "occupancy (",Psi,")",sep=" ",
                             ylim=c(0,1))))
lines(pred_occur$temp_ac,pred_occur$lower)
lines(pred_occur$temp_ac,pred_occur$upper)

## deteccao
plot(pred_detec$esforco,pred_detec$Predicted,type="l",lwd=3,xlab="Sampling effort (number of eppendorfs)",
     ylab=expression (paste (italic("Aedes aegypti "), "detection (p)",sep=" ")))
lines(pred_detec$esforco,pred_detec$lower)
lines(pred_detec$esforco,pred_detec$upper)









######################################################
## codigo para construirmos um mapa da probabilidade de ocorrencia de Aedes aegypti no RS
######################################################

require(rgdal)## carregar os pacotes (instale se necessario com install.packages('rgdal',dependencies=T))
require(raster) ## (instale se necessario com install.packages('raster',dependencies=T))
require (maptools) # install.packages("maptools", dependencies=T)
require(gpclib) # install.packages("gpclib", type = "source",dependencies=T)
require(ggsn) ## install.packages("ggsn", dependencies=T)

## abrir o RData com dados espaciais (shapefiles)
load("dados_espaciais.RData")
## descricao dos dados
#### munRS = shapefile dos municipios do Rio Grande do Sul
#### lagoas = shapefile dos lagos do RS
#### southAme = shapefile da america do Sul (para o fundo)

## cortar o shapefile southAme para pegar os paises vizinhos ao Brasil
BR_AR_URU<- southAme [southAme@data$COUNTRY == "Paraguay" | southAme@data$COUNTRY == "Brazil" | southAme@data$COUNTRY == "Argentina" | southAme@data$COUNTRY == "Uruguay", ]
crs(munRS)<-crs(BR_AR_URU) ## definir o sistema de coordenada refencia (crs) para o shapefile do RS,
## com base no crs do shapefile da ASul

## para fazer o mapa, utilizaremos o pacote ggplot2
require(ggplot2)

## construir um dataframe com os valores que desejamos colocar no mapa, que representarao as 'cores'
## pegue o output do melhor modelo
cores <- data.frame (cores=predict (mod5,type="state")$Predicted,NM_MUNICIP=munRS@data$NM_MUNICIP)

## fortify "destrincha" um dataframe de acordo com um fator especificado (nesse caso "NM_MUNICIP", que sao os nomes do municipios do RS)
## este eh o formato requerido pelo ggplot
f.mun<-fortify(munRS, region="NM_MUNICIP")

## fazer a correspondencia entre as cores e os municipios
f.mun<- cbind (f.mun, Namostral= cores [match (f.mun$id, cores$NM_MUNICIP),]$cores)

## fazer um mapa inicial do fundo do mapa
a <- ggplot() + geom_polygon (data=BR_AR_URU, aes(x=long, y=lat, group=group),size = 0.1, fill="gray90", colour="gray75",alpha=1) +
  coord_fixed (xlim = c(-57.5, -49),  ylim = c(-34, -27), ratio = 1) 

## inserir nestes mapas os lagos
b <- a + geom_polygon(data=lagos, aes(x=long,y=lat, group=group),size=0.1, fill="lightcyan",colour="lightcyan",alpha=1)

## plotar a probabilidade de ocorrencia em cada municipio
c<- b+  geom_polygon(data=f.mun, aes(x=long, y=lat, group=group, color=Namostral, fill=Namostral), colour = NA, size=1) + 
  labs (title= expression (paste ("Ocorrencia de ",italic ("Aedes aegypti"))),size=1)+
  scale_fill_gradient2 (low='white', high='darkred', midpoint=0.2, limits=c(0,0.7), 
                        name="Probabilidade\nde ocupacao") ## para continuo

## colocar anotacoes no mapa, de modo a descrever os paises/estado limitrofes
d<-c + annotate(geom="text", x=-56, y=-32, label="URUGUAI",color="black",size=3) +
  annotate(geom="text", x=-56.5, y=-27.8, label="ARGENTINA",color="black",size=3)+
  annotate(geom="text", x=-56.5, y=-27, label="PARAGUAI",color="black",size=3)+
  annotate(geom="text", x=-51.8, y=-27, label="Santa Catarina",color="black",size=3)+
  annotate(geom="text", x=-50.5, y=-32.5, label="OCEANO ATLANTICO",color="black",size=3)

## inserir uma escala no mapa - pulei este passo por causa do erro
e<- d + ggsn::scalebar(f.mun, dist = 100, st.dist=0.02,st.size=3, height=0.01, dd2km = TRUE, 
                       model = 'WGS84', location = "bottomright")
str(f.mun)
## colorir o fundo de azul, para representar o oceano; colocar os nomes dos eixos
f<- d + theme(panel.background = element_rect(fill = "lightcyan", colour = "lightcyan", 
                                              size = 0.5, linetype = "solid")) + 
  xlab("Longitude (graus decimais)") + ylab("Latitude (graus decimais)") 

## ajustar o tamanho dos nomes nos eixos, titulo e legenda
g<- f+  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size=13),axis.text=element_text(size=10),
        axis.text.x = element_text(size=6),
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size=6),
        axis.title.y = element_text(size = 10)) +
  theme(legend.text=element_text(size=8)) +
  theme (legend.title=element_text(size=10)) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines")) # ajuste da margem

## indicar o norte
g_north <- g + ggsn::north(f.mun, symbol=1) 

## temos que apresentar o plot para salvar utilizando a funcao 'ggsave'  
g_north

## salvar o mapa em formato PNG
ggsave(f_north = "mapa_aedes_aegypti.png", width = 6,height=6,dpi =300)


## Exercicio 4:
##
## Escolha um dos cenarios abaixo e use o R para simular dados de detecao/nao-detecao
## de uma especie na natureza. Em seguida, use o pacote unmarked para ajustar aos
## dados um conjunto de modelos a sua escolha. O seu conjunto de modelos precisa incluir
## o modelo que descreve o cenario que voce escolheu. Com base em uma comparacao do AIC dos
## diferentes modelos que voce ajustar, mostre que o modelo que descreve o cenario escolhido
## eh realmente o modelo que melhor representa os seus dados. Este eh um exercicio circular:
## primeiro, voce vai simular dados de acordo com um modelo (ou cenario escolhido), depois
## voce ajusta os modelos aos dados, e finalmente mostra que o modelo usado na simulacao eh o
## que melhor se ajusta aos dados que voce simulou.
##     Para demonstrar que seus dados foram simulados corretamente, voce deve mostrar nao
## so a tabela de AIC, mas tambem as estimativas dos parametros do melhor modelo.
##     Para entender melhor os cenarios abaixo, pense numa especie de lagarto que se distribui
## por um conjunto de sitios que apresentam diferentes altitudes (CovS1) e inclinacoes do
## terreno (CovS2). Voce amostra esta especie em um conjunto de sitios a sua escolha e
## em um numero de visitas tambem a sua escolha. No entanto, em diferentes visitas a diferentes
## sitios voce pode ter mais ou menos luz solar direta (CovA1), o que pode ou nao influenciar
## seu sucesso de amostragem. Os quatro cenarios abaixo representam diferentes relacoes entre
## as tres covariaveis listadas acima e os dois parametros centrais dos seus modelos, a
## probabilidade de ocupacao de sitios (psi) e a probabilidade de deteccao da especie naqueles
## sitios que ela realmente ocupa (p). Escolha apenas um dos quatro cenarios abaixo e identifique
## claramente qual cenario voce escolheu.
##
#### SOLUCAO ####

## Cenario 1: Psi diminui com o aumento da altitude, e p aumenta com a quantidade de luz solar.
## CovS1 efeito negativo sobre psi (CovS1=altitude)
## CovA1 efeito positivo sobre p (CovA1=luz)

## Cenario 2: Psi aumenta com o aumento da altitude, e p diminui com o aumento da altitude.
## Parte 1: Simulacao dos dados

## Cenario 3: Psi aumenta com o aumento da altitude e diminui com o aumento da inclinacao do
##            terreno; p eh fixo.

## Cenario 4: Psi aumenta com o aumento da altitude e com o aumento da inclinacao. Mas o efeito da
## altitude eh mais forte que o efeito da inclinacao; p eh fixo.



