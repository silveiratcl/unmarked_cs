
library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)


# monitoramento 

df_monit <- read_delim("data/dados_monitoramento_cs_2024-03-22.csv",
                       col_types = list(localidade = col_character(),
                                        data = col_date(format = "%d/%m/%Y"),
                                        visib_horiz = col_double(),
                                        faixa_bat = col_character(),
                                        prof_interface_min = col_double(),
                                        prof_interface_max = col_double(),
                                        metodo = col_character(),
                                        observer = col_character(),
                                        n_divers = col_double(),
                                        tempo_censo = col_double(),
                                        dafor = col_double(),
                                        iar_medio = col_double(),
                                        n_trans_vis = col_double(),
                                        n_trans_pres = col_double(),
                                        dafor_id = col_double(),
                                        geo_id = col_character(),
                                        obs = col_character()
                       ))



spec(df_monit)
df_monit
print(df_monit, n = 100)
df_monit[, 1:17]


## aplicando os filtros 

dfmonit_filt <- df_monit %>% 
  filter(data > "2022-01-01" &
           !(obs %in% c("Sem geo", "estimado dos dados do ICMBio"))) %>%
  arrange(data)
dfmonit_filt

## selecionando a localidade por data para filtrar o numero de transectos

monit <- dfmonit_filt[ ,c("localidade", "data", "n_trans_vis", "n_trans_pres")] %>%
  group_by(localidade, data) %>%
  reframe(visuals = max(n_trans_vis),
          detec = max(n_trans_pres)) %>%
  arrange(data)

print(monit, n = 44)
###número maximo de transectos por data e localidade

## obtendo o total de transectos vistos e detecções para cada localidade

monit2 <- monit %>%
  group_by(localidade) %>%
  summarise(vis = sum(visuals),
            det = sum(detec)) %>%
  arrange(desc(vis))

print(monit2, n = 35)



# geomorfologia

df_geo = read_delim("data/dados_geo_cs_2024-03-22.csv", 
                    col_types = list(localidade = col_character(),
                                     data = col_date(format = "%d/%m/%Y"),
                                     visibilidade = col_double(),
                                     faixa_bat = col_character(),
                                     prof_interface_min = col_double(),
                                     prof_interface_max = col_double(),
                                     metodo = col_character(),
                                     observador = col_character(),
                                     tempo_geo = col_double(),
                                     geo_cat = col_character(),
                                     iar_geo = col_double(),
                                     n_trans_vis = col_double(),
                                     geo_id = col_double())
)
spec(df_geo)
df_geo = df_geo[, 1:14]
df_geo

## agrupando e obtendo o total do IAR para cada geomorfologia em cada localidade

geo <- df_geo[ ,c("localidade", "data", "geo_cat", "iar_geo")] %>%
  group_by(localidade, data, geo_cat) %>%
  reframe(iar_geo = mean(iar_geo)) %>%
  arrange(data)

print(geo, n = 185)

## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

geo2 <- geo[ ,c("localidade", "geo_cat", "iar_geo")] %>%
  group_by(localidade, geo_cat) %>%
  summarise(iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)


print(geo2, n = 34)

## padronizando as geos
###média = 0
###desvio padrão = 1

gc_pad <- as.matrix(geo2$gc - mean(as.matrix(geo2$gc)))/sd(as.matrix(geo2$gc))
lg_pad <- as.matrix(geo2$lg - mean(as.matrix(geo2$lg)))/sd(as.matrix(geo2$lg))
mp_pad <- as.matrix(geo2$mp - mean(as.matrix(geo2$mp)))/sd(as.matrix(geo2$mp))
rpm_pad <- as.matrix(geo2$rpm - mean(as.matrix(geo2$rpm)))/sd(as.matrix(geo2$rpm))
tf_pad <- as.matrix(geo2$tf - mean(as.matrix(geo2$tf)))/sd(as.matrix(geo2$tf))

### padronizando por mínimo e máximo pra tirar os valores negativos

gc_pad <- (geo2$gc - min(geo2$gc)) / (max(geo2$gc) - min(geo2$gc))
lg_pad <- (geo2$lg - min(geo2$lg)) / (max(geo2$lg) - min(geo2$lg))
mp_pad <- (geo2$mp - min(geo2$mp)) / (max(geo2$mp) - min(geo2$mp))
rpm_pad <- (geo2$rpm - min(geo2$rpm)) / (max(geo2$rpm) - min(geo2$rpm))
tf_pad <- (geo2$tf - min(geo2$tf)) / (max(geo2$tf) - min(geo2$tf))


geo2$gc_pad <- gc_pad
geo2$lg_pad <- lg_pad
geo2$mp_pad <- mp_pad
geo2$rpm_pad <- rpm_pad
geo2$tf_pad <- tf_pad

geo_pad <- geo2[, c(1, 7:11)]
print(geo_pad, n = 34)


# matriz de similaridade das geos entre as localidades
##pcoa a partir da matriz gerada pela distancia de Chord
##medida baseada na distância euclidiana

library(vegan)
library(labdsv)

geo_euc <- vegdist(decostand(geo2[, 2:6], "normalize"), "euc")
#a função ja normaliza os dados? se sim, usar geo2, senão geo_pad
geo_euc.pcoa <- cmdscale(d=geo_euc,k=(nrow(geo2)-1),eig=T,add=T)


windows(6,6)
#par(mfrow=c(1,3))
ordiplot(prcomp(geo_euc.pcoa$points[,c(1,2)]),type="t",
         main="PCoA - distância de Chord")
rownames(geo_euc.pcoa$points) <- (geo2$localidade)
head(geo_euc.pcoa$points)
abline(h=0, lty=3)
abline(v=0, lty=3)
gpcoa.wa <- wascores(x=geo_euc.pcoa$points[,1:2],w=geo_pad[, 2:6])
text(gpcoa.wa,rownames(gpcoa.wa),cex=0.7,col="red")
#text(-0.45,-0.45,labels="reduz o peso das spp \n muito abundantes",
     #pos=4,col="blue")


#pcoa por hellinger

geo.hel <- decostand(geo2[, 2:6], "hellinger")
geo.dhel <- vegdist(geo.hel, "euc")
geo.dhel.pcoa <- cmdscale(d=geo.dhel,k=(nrow(geo2)-1),eig=T,add=T)
site.sc.pcoa <- as.data.frame(geo.dhel.pcoa$points[,1:2])
head(site.sc.pcoa)
colnames(site.sc.pcoa) <- c("PCoA1","PCoA2")
names(site.sc.pcoa)

geo.dhel.upgma <- hclust(geo.dhel,"average")
cor(geo.dhel,cophenetic(geo.dhel.upgma))

plot(geo.dhel.upgma,hang=-1)

gr <- cutree(tree=geo.dhel.upgma, k=6)
site.sc.pcoa$gr <- gr
head(site.sc.pcoa)

explic.pcoa <- geo.dhel.pcoa$eig/sum(geo.dhel.pcoa$eig)*100
explic.pcoa

geo.dhel.wa <- wascores(x=geo.dhel.pcoa$points[,1:2],w=geo2[, 2:6])
geo.dhel.wa <- as.data.frame(geo.dhel.wa)
colnames(geo.dhel.wa) <- c("PCoA1","PCoA2")
isa.hel <- indval(x=geo.hel,clustering=site.sc.pcoa$gr)
x <- isa.hel
tableISAhel <- data.frame(Group=x$maxcls,
                          IndVal=round((x$indcls)*100,2), p_value=x$pval)
tableISAhel$sig[tableISAhel$p_value<0.05] <- "*"
tableISAhel <- tableISAhel[order(tableISAhel$Group),]
tableISAhel <- tableISAhel[!is.na(tableISAhel$sig),]
tableISAhel

nrow(tableISAhel)
nrow(geo.dhel.wa)


geo.dhel.wa$maxcls <- as.numeric(isa.hel$maxcls)
geo.dhel.wa$pval <- as.numeric(isa.hel$pval)
head(geo.dhel.wa)
geo.dhel.wa <- geo.dhel.wa[geo.dhel.wa$pval<0.05,]
nrow(geo.dhel.wa)

x11()
par(mar=c(5,5,4,2))
plot(geo.dhel.pcoa$points[,1:2],type="n",
     xlab=paste("PCoA 1 - ",round(explic.pcoa[1],1),"%",sep=""),
     ylab=paste("PCoA 2 - ",round(explic.pcoa[2],1),"%",sep=""),
     cex.lab=1.4,font.lab=2,cex.axis=1.2,cex.main=1.6,
     main="Distância de Hellinger")
abline(h=0, lty=3, col="gray")
abline(v=0, lty=3, col="gray")
axis(1,lwd=2,labels=F)
axis(2,lwd=2,labels=F)
box(lwd=2)

x <- site.sc.pcoa
points(x$PCoA1[x$gr==1],x$PCoA2[x$gr==1],
       pch=1,col="blue",lwd=2,cex=2.4)
points(x$PCoA1[x$gr==2],x$PCoA2[x$gr==2],
       pch=1,col="forestgreen",lwd=2,cex=2.4)

points(x$PCoA1[x$gr==3],x$PCoA2[x$gr==3],
       pch=1,col="darkorange",lwd=2,cex=2.4)
points(x$PCoA1[x$gr==4],x$PCoA2[x$gr==4],
       pch=1,col="red",lwd=2,cex=2.4)
points(x$PCoA1[x$gr==5],x$PCoA2[x$gr==5],
       pch=1,col="purple",lwd=2,cex=2.4)
points(x$PCoA1[x$gr==6],x$PCoA2[x$gr==6],
       pch=1,col="salmon",lwd=2,cex=2.4)
text(x$PCoA1,x$PCoA2,labels=rownames(x),cex=0.6)
legend("bottomleft",legend=c("Grupo 1","Grupo 2","Grupo 3",
                             "Grupo 4","Grupo 5","Grupo 6"),
       col=c("blue","forestgreen","darkorange","red","purple",
             "salmon"),pch=1,bty="n",pt.lwd=2,pt.cex=1.2)

x <- geo.dhel.wa
text(x$PCoA1[x$maxcls==1],x$PCoA2[x$maxcls==1],
     labels=rownames(x)[x$maxcls==1],col="blue")
text(x$PCoA1[x$maxcls==2],x$PCoA2[x$maxcls==2],
     labels=rownames(x)[x$maxcls==2],col="forestgreen")
text(x$PCoA1[x$maxcls==3],x$PCoA2[x$maxcls==3],
     labels=rownames(x)[x$maxcls==3],col="darkorange")
text(x$PCoA1[x$maxcls==4],x$PCoA2[x$maxcls==4],
     labels=rownames(x)[x$maxcls==4],col="red")
text(x$PCoA1[x$maxcls==5],x$PCoA2[x$maxcls==5],
     labels=rownames(x)[x$maxcls==5],col="purple")
text(x$PCoA1[x$maxcls==6],x$PCoA2[x$maxcls==6],
     labels=rownames(x)[x$maxcls==6],col="salmon")

# unindo os data frames

geomonit <- left_join(monit2, geo_pad) %>%
  arrange(desc(det)) %>%
  drop_na()

print(geomonit, n = 182)



