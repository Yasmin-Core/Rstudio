library(h2o)
library(plotrr)
library (cluster)
library (gridExtra)
library (grid)
library(tidyr)
library(dplyr)
library(purrr)
library(plotrix)
library(ggplot2)
library(factoextra)
install.packages("ggplot2")
install.packages("factoextra")
#-Essas são as dependências que foram instaladas , e ao instalar executamos a 
#biblioteca delas para que as funções pudessem ser acessadas.
#- Direcionamos o software para a leitura do conjunto de dados no diretório adequado desta máquina.


4#leitura da base de dados
df <- read.csv(file = "/home/michele/Área de Trabalho/TCC/script final/Wholesale customers data.csv", header = TRUE, sep = ",")
str  


#apresenta o rótulo de cada variável
names0214141


#apresenta as seis primeiras linhas da base da dados
head(df)
#Resultado :- Essa função nos mostra as 6 primeiras linhas do conjunto de dados.
#Chanel – Canais – Tipos de serviços
#1- HORECA
#2- VAREJO
#Region- Regiões
#1- Lisboa
#2-Porto
#3-Outras regiões


#apresenta um resumo estatístico dos dados
summary(df)




#apresenta um resumo estatístico por característica
summary(df$Fresh)
summary(df$Milk)
summary(df$Grocery)
summary(df$Frozen)
summary(df$Detergents_Paper)
summary(df$Delicassen)
#Resultado :  A partir destes resultados , podemos visualizar os preços de compras de cada produto ,
#com isso , o diretor de vendas pode indicar em qual produto o comerciante pode 
#investir para obter mais lucro. Neste caso ,
#os produtos frescos , o leite e os vegetais são os mais indicados a serem investidos.


#Gráfico de barras que apresenta a distribuição das regiões
a=table(df$Region)

barplot(a,main="Distribuição das Regiões",
        ylab="Quantidade",
        xlab="Região",
        col=rainbow(3),
        legend=rownames(a))

# Gráfico de pizza para visualizar a proporção de distribuição das regiões

pct=round(a/sum(a)*100)

lbs=paste(c("Lisboa","Porto","Outras Regiões")," ",pct,"%",sep=" ")

pie3D(a,labels=lbs,
      main="Proporção das Regiões de Compra")
#Resultado :Neste gráfico podemos visualizar que entre a região de Porto e Lisboa entre as 
#outras regiões suas porcentagens são menores , com isso o comerciante consegue perceber 
#em quais regiões está vendendo pouco e a partir desta informação pode-se criar uma 
#estratégia para que consiga mais clientes dessas regiões .


#Gráfico de barras que apresenta a distribuição dos tipos de serviços
a=table(df$Channel)

barplot(a,main="Distribuição dos Tipos de Serviços",
        ylab="Quantidade",
        xlab="Serviços",
        col=rainbow(3),
        legend=rownames(a))


# Gráfico de pizza para vizualizar a proporção de distribuição dos serviços

pct=round(a/sum(a)*100)

lbs=paste(c("Hotéis/Restaurantes/Cafés","Varejo")," ",pct,"%",sep=" ")



pie3D(a,labels=lbs,
      main="Proporção de distribuição dos serviços")

#Resultado:No gráfico de tipos de serviços , pode ser usado para identificar o público alvo do comércio
#, neste atacadista pode-se ver que os clientes HORECA são o público que mais compra neste 
#atacadista .


#Histogramas para cada um dos produtos
#O Histograma é uma representação gráfica da distribuição de frequências dos produtos.

#frescos
summary(df$Fresh)

hist(df$Fresh,
     col="#660033",
     main="Histograma para Produtos Frescos",
     xlab="Valores de Compras",
     ylab="Frequência",
     labels=TRUE)

#leite
summary(df$Milk)
hist(df$Milk,
     col="#CDC5BF",
     main="Histograma para Leite",
     xlab="Valores de Compras",
     ylab="Frequência",
     labels=TRUE)

#vegetais
summary(df$Grocery)
hist(df$Grocery,
     col="#EEE685",
     main="Histograma para Vegetais",
     xlab="Valores de Compras",
     ylab="Frequência",
     labels=TRUE)


#congelados
summary(df$Frozen)
hist(df$Frozen,
     col="#CDAA7D",
     main="Histograma para Congelados",
     xlab="Valores de Compras",
     ylab="Frequência",
     labels=TRUE)



#limpeza
summary(df$Detergents_Paper)

hist(df$Detergents_Paper,
     col="#C1CDC1",
     main="Histograma para Limpeza",
     xlab="Valores de Compras",
     ylab="Frequência",
     labels=TRUE)

#PADARIA
summary(df$Delicassen)

hist(df$Delicassen,
     col="#A4D3EE",
     main="Histograma para Padaria",
     xlab="Valores de Compras",
     ylab="Frequência",
     labels=TRUE)

#Resultado: Com isso, podemos ver que os produtos de padaria são os que 
#têm mais frequências de compras neste atacadista.

##############################
#UTILIZANDO O KMEANS

#encontrado o melhor valor de K pelo método do cotovelo
#a princípio 3 é o valor ideal para k.
set.seed(200)
k.max <- 10

wss<- sapply(1:k.max,function(k){kmeans(df[,3:8],k,nstart = 20,iter.max = 20)$tot.withinss})
wss


plot(1:k.max,wss, type= "b", xlab = "Número de clusters(k)", ylab = "Soma dos quadrados dentro do cluster")

#É um algoritmo que agrupa dados por afinidades.
#Para saber a quantidade de grupos o k-means consegue agrupar a partir dos dados que estamos trabalhando.
#Neste gráfico utilizamos o método do cotovelo para identificar o número de k ideal.
#A partir da análise deste método, o valor ideal de k é o 3.


#assumindo k = 3 como valor ideal

k2<-kmeans(df[,3:8],3,iter.max=100,nstart=50,algorithm="Lloyd")

k2


#A interpretação do coeficiente de silhueta é a seguinte:
# >  0 significa que a observação está bem agrupada. Quanto mais próximo o coeficiente estiver de 1, melhor será o agrupamento da observação.
# <  0 significa que a observação foi colocada no cluster errado.
# =  0 significa que a observação está entre dois clusters.
#
set.seed(42)


sil <- silhouette(k2$cluster, dist(df))


fviz_silhouette(sil)



###################################################################################
#Gráficos apresentando a districuição dos dados antes do agrupamento e depois do agrupamento

make_hist <- function(x){
  df %>% 
    ggplot(aes(x)) +
    geom_histogram() 
}


plots <- df %>%
  map(make_hist) 


title <- grid::textGrob("Distribuição Pré-Cluster")

grid.arrange(plots[[3]],
             plots[[4]],
             plots[[5]],
             plots[[6]],
             plots[[7]],
             plots[[8]],
             top = title)


clustered_model_data <- df %>% 
  mutate(cluster = as.factor(kmeans(df, 3)$cluster)) %>%
  map(unlist) %>% 
  as_data_frame()

make_hist <- function(x){
  clustered_model_data %>% 
    ggplot(aes(x, fill = cluster)) +
    geom_density(alpha = 0.3) 
}

plots <- clustered_model_data %>%
  map(make_hist) 




title <- grid::textGrob("Distribuição Pós-Cluster")


grid.arrange(plots[[3]],
             plots[[4]],
             plots[[5]],
             plots[[6]],
             plots[[7]],
             plots[[8]],
             top = title)
#As vendas dos produtos frescos acima de 30 mil euros , ficaram concentradas no clusters 1 , 
#enquanto as vendas dos vegetais acima de 20 mil euros se encontram no clusters 2.


########################################
#Distribuição dos produtos por cluster
a=table(k2$cluster)


barplot(k2$centers,main="Distribuição dos Produtos por Cluster",
        ylab="Quantidade",
        xlab="Produto",
        col=rainbow(3),
        legend=rownames(a))
################################################################################
# RESULTADO FINAL DO AGRUPAMENTO

par(mfrow=c(1, 1), mar=c(4, 4, 4, 2))

myColors <- c("darkblue", "red", "green", "pink", "purple", "yellow")
barplot(t(k2$centers), beside = TRUE, xlab="cluster", 
        ylab="value", col = myColors)
legend("topleft", ncol=2, legend = c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen"), fill = myColors)



