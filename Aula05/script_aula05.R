#########################################
#########4. Tutoriais de An·lise ExploratÛria de Dados#########
##########Dispon√?vel em http://ecologia.ib.usp.br/bie5782/doku.php?id=bie5782:02_tutoriais:tutorial4:start##

##Conferindo data.frame####
aves.c = read.table("aves_cerrado.csv", row.names=1, header=T, sep=";", dec=",", as.is=T) #le o arquvio 

#Verifica√ß√£o inicial####
head(aves.c) #retorna as 6 primeiras linhas
tail(aves.c) #retorna as 6 ultimas linhas
str(aves.c) #retorna todos os vetores com sua estrutura
summary(aves.c) #retorna dados estat√?sticos dos dados

aves.c[aves.c$urubu==NA,] #funcao errada, nao funciona para NA pois ele nao e numerico
is.na(aves.c) #verifica os NA do data.frame, onde aparece TRUE e onde esta o NA
is.na(aves.c$urubu) #verifica os NA da coluna urubu
aves.c[is.na(aves.c$urubu),] #mostra as linhas que contem NA da coluna urubu
aves.c[is.na(aves.c$urubu)|is.na(aves.c$carcara)|is.na(aves.c$seriema),] #mostra as linhas que tem NA na coluna urubu, ou na coluna carcara, ou na coluna seriema

temp1 <- aves.c[is.na(aves.c$urubu)|is.na(aves.c$carcara)|is.na(aves.c$seriema),] #joga todas as linhas da tabela que contem NA e joga para dentro de um vetor

aves.c$urubu[is.na(aves.c$urubu)] <- 0 #substitui todos os NA da coluna urubu, por zero
aves.c$carcara[is.na(aves.c$carcara)] <- 0 #substitui todos os NA da coluna carcara, por zero
aves.c$seriema[is.na(aves.c$seriema)] <- 0 #substitui todos os NA da coluna seriema, por zero
aves.c[aves.c$urubu==0|aves.c$carcara==0|aves.c$seriema==0,] #mostra as linhas que tem zero na coluna urubu, ou na coluna carcara, ou na coluna seriema
temp1 #mostra o objeto temp1
#podemos comparar as duas tabelas, e vemos que os NA localizados no objeto temp1 foram substituidos por zero com sucesso
table(aves.c$fisionomia) #retorna os caracteres e quantidade de cada um dele
aves.c$fisionomia[aves.c$fisionomia=="ce"] <- "Ce" #correge(kkkkk) o erro de digitacao do caractere "ce" 
table(aves.c$fisionomia)

aves.c$fisionomia <- factor(aves.c$fisionomia, levels=c("CL","CC","Ce")) #converte o vetor fisionomia para factor, com seus respectivos niveis
str(aves.c) #verificando
summary(aves.c) #verificando


#M√©dia, Mediana e Quantis####
mean(aves.c[,2:4]) #nao funciona
sapply(aves.c[,2:4],mean) #calcula a media de cada coluna das especies
sapply(aves.c[,2:4],median) #calcula a mediana de cada coluna das especies

#Funcao apply####
apply(aves.c[,2:4] #na tabela aves.c, todas as linhas das colunas de 2 at√© 4
      ,2 #para as colunas (1 para linhas)
      ,median #caulcule mediana
      )

apply(aves.c[,2:4] #na tabela aves.c, todas as linhas das colunas de 2 at√© 4
      ,2 #para as colunas (1 para linhas)
      ,mean #calcule a media
      ,trim=0.1 #tirando 10% dos dados
      )

#os valores de media e mediana sao proximos, provavelmente os dados seguem uma distribuicao normal

quantile(aves.c$urubu) #retorna os quartis da coluna urubu
summary(aves.c$urubu) #retorna o resumo estatistico da coluna urubu. O engracado e que os valores sao equivalentes aos quartis.
quantile(aves.c$urubu #calcule os quartis da coluna urubu do objeto aves.c
         ,probs= seq(from=0,to=1,by=0.1) #indicando que eu quero os quartis de 10% em 10%
         )
summary(aves.c[,2:4]) #retorna todos quartis de todas as colunas

#Exploracao de uma Variavel Categorica####
caixeta = read.csv("caixeta.csv", as.is=T) #carrega o arquivo caixeta e joga em um vetor
names(caixeta) #retorna os nomes das colunas do data.frame
table(caixeta$especie) #retorna todos os objetos do vetor especie e seus respectivos tamanhos
sort(table(caixeta$especie), decreasing=T) #retorna os objetos seguindo a ordem decrescente de seus tamanhos - do objeto maior para o menor

barplot(sort(table(caixeta$especie), decreasing=T)) #retorna o comando a cima em um grafico de barras
barplot(table(caixeta$local)) #retorna um grafico de barras dos objetos do vetor local do data.frame caixeta, com suas respectivas quantidades
dev.off()
##Gr·ficos para uma Vari·vel####
x11()
par(mfrow=c(2,2)) #permite plotar 4 graficos em uma imagem, de duas linhas por duas colunas
boxplot(aves.c$urubu)
hist(aves.c$urubu)
plot(density(aves.c$urubu))
stripchart(aves.c$urubu, method="stack") #produz scatter plot de uma dimensao para um dado 
par(mfrow=c(1,1))

#O que acontece se vocÍ omite a primeira linha? E a ˙ltima?

##Variacoes do Histograma####
## Histograma com os valores (funcao rug)
hist(aves.c$urubu) #retorna um histograma do objeto urubu
rug(jitter(aves.c$urubu)) #acrescenta marcas indicando a posicao de cada observacao no eixo x; jitter retorna o numerico de mesmo comprimento de x, mas com ruido, ou seja, retona todos os pontos que tem mesmo valor, sem sobrepor

hist(aves.c$urubu, prob=T) #produz um histograma reescalonando de modo que a area das barras somem um para que possa plotar uma ajuste nao parametrico de densidade probabilistico (curva empirica); curva baseada nos dados observados
lines(density(aves.c$urubu),col="blue" ) #retorna uma linha de densidade (curva empirica)

hist(aves.c$urubu, prob=T) #produz um histograma reescalonando de modo que a area das barras somem um para que possa plotar uma curva normal 
curve(expr = dnorm(x,mean=mean(aves.c$urubu),sd=sd(aves.c$urubu)),add=T, col="red") #parametros de uma curva normal colocando media e desvio padrao

plot(density(aves.c$urubu),col="blue", ylim=c(0,0.08)) #produz um grafico com ajuste nao parametrico de densidade probabilistico
curve(expr = dnorm(x,mean=mean(aves.c$urubu),sd=sd(aves.c$urubu)),add=T, col="red") #sobrepoe uma curva normal

#O que estes graficos revelam sobre a distribuicao do numero de avistamentos de urubus neste estudo ficticio?
#revelam que a curva empirica e bimodal, os dados nao obedecem uma distribuicao normal

#table e aggregate ####
table(caixeta$especie,caixeta$local) #retorna os objetos dentro do vetor especie, pelo vetor local, e suas respectivas quantidades

#Quando temos uma variavel categorica (fator) e uma numerica, as funcoes aggregate e tapply sao muito uteis. A funcao aggregate eh o equivalente das tabelas dinamicas das planilhas eletronicas. Por exemplo, para obter do objeto caixeta um data frame com a altura media dos fustes de cada especie de arvore por local voce executa o comando:
caixeta.alt = aggregate(caixeta$h, by=list(local=caixeta$local,especie=caixeta$especie),FUN=mean) #retorna a media da coluna h da tabela caixeta pelo local e especie


##xtabs####
Titanic.df = read.csv("titanic.csv", as.is=T) #carrega o arquivo caixeta e joga em um vetor

xtabs(Freq~Sex+Survived, data=Titanic.df) #retorna uma tabela de contigencia contendo freq por sexo entre os sobreviventes

prop.table(xtabs(Freq~Sex+Survived, data=Titanic.df), margin=1) #retorna a proporcao da tabela criada acima

xtabs(Freq~Class+Survived, data=Titanic.df) #retorna uma tabela de contigencia contendo freq por classe entre os sobreviventes

prop.table(xtabs(Freq~Class+Survived, data=Titanic.df), margin=1)#retorna a proporcao da tabela criada acima
#Por que usamos a funÁ„o xtabs neste caso e n„o a funÁ„o table?
########## O xtabs() permite que a gente crie a tabela de contigencia, cruzando objetos do vetor. Ou seja, È baseada em uma formula, coisa que o table n permite.

##Formula estatistica em graficos####
#A funÁ„o xtabs (tutorial anterior) usa a notaÁ„o de fÛrmula estatÌstica do R, que È:
#####vari·vel dependente ~ vari·veis preditoras
boxplot(urubu~fisionomia, data=aves.c) #plota um "grafico em caixa" boxplot, de urubu por fisionomia
plot(seriema~urubu, data=aves.c, subset=fisionomia=="Ce") #plota siriema por urubu, apenas para o subgrupo de nivel Ce na fisionomia
plot(seriema~urubu, data=aves.c, subset=fisionomia=="CC")#plota siriema por urubu, apenas para o subgrupo de nivel CC na fisionomia
plot(seriema~urubu, data=aves.c, subset=fisionomia!="CL") #plota siriema por urubu, apenas para o subgrupo de nivel CL na fisionomia


library(lattice)
xyplot(seriema~urubu|fisionomia, data= aves.c) #plota siriema em funÁ„o de urubu, para todos os niveis de fisionomia - mostra um grafico com subgrupos

##O quarteto de Anscombe####
data(anscombe)#carrega para a area de trabalho
ls() #agora o objeto esta no workspace
names(anscombe) #retorna os nomes das colunas da tabela anscombe

apply(anscombe[1:4], 2, mean) #calcula a media de cada coluna, da 1 ate a 4
apply(anscombe[5:8], 2, mean)

apply(anscombe[1:4], 2, var) #calcula a variancia de cada coluna, da 1 ate a 4
apply(anscombe[5:8], 2, var)

with(anscombe,cor(x1,y1)) #testa correlacao de pearson entre vada variavel
with(anscombe,cor(x2,y2))
with(anscombe,cor(x3,y3))
with(anscombe,cor(x4,y4))

#O que se pode concluir ate aqui?
#### que as variaveis possuem uma certa correlacao entre elas
x11()
par(mfrow=c(2,2)) # 4 graficos em uma janela
plot(y1~x1, data=anscombe) #plota uma variavel em funcao da outra
plot(y2~x2, data=anscombe)
plot(y3~x3, data=anscombe)
plot(y4~x4, data=anscombe)
par(mfrow=c(1,1))
par(mfrow=c(2,2)) # 4 graficos em uma janela

x11()
par(mfrow=c(2,2))
plot(y1~x1, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8])) #cria um plot
abline(lm(y1~x1, data=anscombe)) #insere uma reta de regressao linear
plot(y2~x2, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y2~x2, data=anscombe))
plot(y3~x3, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y3~x3, data=anscombe))
plot(y4~x4, data=anscombe,
     xlim=range(anscombe[,1:4]),ylim=range(anscombe[,5:8]))
abline(lm(y4~x4, data=anscombe))
dev.off()


#########################################
#########4. An·lise ExploratÛria de Dados#########
##########Segundo script - disponivel em http://ecologia.ib.usp.br/bie5782/doku.php?id=bie5782:03_apostila:05-exploratoria####

#Estatisticas Descritivas####
cax = read.csv("caixeta.csv", header=TRUE, as.is=TRUE) #carrega o arquivo caixeta e joga no objeto cx
summary(cax) #retorna o sumario estatistico dos dados

resumo1 = aggregate( cax[ , c("cap","h")], list(local=cax$local), mean ) #aplica a funcao media para todas as linhas das colunas "cap" e "h" do objeto cx, agrupando pelo fator local do data.frame e jogando no objeto resumo1
resumo2 = aggregate( cax[ , c("cap","h")], list(local=cax$local), sd )#aplica a funcao desvio padrao para todas as linhas das colunas "cap" e "h" do objeto cx, agrupando pelo fator local do data.frame e jogando no objeto resumo2
resumo = merge( resumo1, resumo2, by="local", suffixes=c(".mean",".sd") ) #agrupa as tabelas de media e desvio padrao, por local. Gera um data.frame com as medias e desvios padroes das variaves cap e h, por local (3 locais)
resumo


##Exercicio: Estatisticas do Caixetal

cax.area.basal = data.frame (x=cax$cap, y=cax$local, z=cax$parcela)
colnames (cax.area.basal)= c(x="cap", y="local", z="parcela")

str(cax.area.basal)
cax.mean <- aggregate(cax.area.basal$cap, by=list(local=cax.area.basal$local, parcela=cax.area.basal$parcela), FUN=mean)

cax.sd <- aggregate(cax.area.basal$cap, by=list(local=cax.area.basal$local, parcela=cax.area.basal$parcela), FUN=sd)

## Analisando a Distribuicao das Variaveis: Graficos Univariados ####
## Histogramas
cax$dap = (pi/4)* (cax$cap/10)

hist( cax$dap )
hist( cax$dap[ cax$local == "chauas" ] ) #cria um histograma do dap selecionando apenas o local "chauas"
hist( cax$dap[ cax$local == "jureia" ] )
hist( cax$dap[ cax$local == "retiro" ] )

hist( cax$dap, plot=FALSE ) #fornece os dados do histograma sem cria um histograma

dap.hist = hist( cax$dap, plot=FALSE) #salva os dados do histograma dentro do objeto dap.hist
class(dap.hist) #retorna a classe do objeto 
plot(dap.hist) #plota o objeto criado que eh um histograma

hist( cax$dap[ cax$local=="chauas" ],
      xlab="Di√¢metro √† Altura do Peito - DAP (cm)",
      ylab="Freq√º√™ncia",
      main="Histograma DAP - Chau√°s",
      col = "blue" ) #adiciona argumentos que add nome aos eixos, titulo e cor ao grafico.

hist( cax$dap[ cax$local=="chauas" ] , main="Chau√°s" ) #gera um histograma do dap selecionando apenas o local "Chauas" com titulo "Chauas"
X11() #abre uma janela para plotar o grafico
hist( cax$dap[ cax$local=="jureia" ] , main="Jur√©ia" )
X11()
hist( cax$dap[ cax$local=="retiro" ] , main="Retiro" )

dev.off () #fecha uma janela grafica
dev.cur () #diz que a janela grafica esta ativa
dev.set(which=dev.cur())#define qual janela dever√° ficar ativa, o argumento 'which' deve ser o n√∫mero da janela;
dev.next(which=dev.cur()) # informa o n√∫mero da pr√≥xima janela gr√°fica;
dev.prev(which=dev.cur())# informa o n√∫mero da janela gr√°fica anterior;
graphics.off() # fecha todas as janelas gr√°ficas.

##Exercicio: Altura de arvores em Caixetais ####
##Construa um histograma da altura das arvores do caixetal.
caixeta = read.csv("caixeta.csv")
hist(caixeta$h)

#Construa histogramas da altura das arvores para os diferentes caixetais ('local').
table(caixeta$local)
hist(caixeta$h[caixeta$local=="chauas"])
hist(caixeta$h[caixeta$local=="jureia"])
hist(caixeta$h[caixeta$local=="retiro"])

#Ha diferencas entre as estruturas (distribuicao de tamanhos) dos caixetais?
#apresentam diferentes distribuicoes

#Exercicio: Diametros de arvores de Eucalipto ####
#Construa um histogram do DAP das arvores de E. saligna. 
#Procure interpretar o histograma.
esaligna = read.csv("esaligna.csv")
hist(esaligna$dap)

##Graficos de Densidade####
plot( density(cax$dap) ) #estima a densidade de kernel gaussiano de dap do objeto cx e plota

plot( density(
  cax$dap
  ,bw=0.5) #determina a amplitude da janela de observaÁ„o
  ,col="red" )
lines( density(cax$dap, bw=5), col="blue" )
lines( density(cax$dap, bw=1.5), col="green") 


##Exercicio: Distribuicao de DAP nos Caixetais####

plot(density(cax$dap[caixeta$local=="chauas"]))
lines(density(cax$dap[caixeta$local=="jureia"]), col= "blue")
lines(density(cax$dap[caixeta$local=="retiro"]), col= "red")

##Boxplot####
esa = read.csv("esaligna.csv",header=TRUE) #carrega o arquivo esaligna.csv e joga pra dentro do objeto esa
boxplot(esa$dap) #plota um boxplot do diametro a altura (dap) do peito do objeto esa
##########O boxplot È ˙til para analisar a simetria de uma distribuiÁ„o, o espalhamento das observaÁıes e a presenÁa de observaÁıes discrepantes.
boxplot(dap ~ local, data=cax ) #plota um boxplot de dap em funcao de local, do data.frame cax
x11()
boxplot( dap ~ local * parcela, data=cax) #plota um boxplot de dap em funcao de local, separado em parcelas, do objeto cax

boxname = paste(sort(rep(unique(cax$local),5)), rep(1:5,3) ) #cria um objeto contendo os nomes da coluna local em ordem alfabetica em repeticoes de 5, junto com uma sequencia de repeticoes de 3 vezes da sequencia de 1 a 5
boxcor = sort(rep(c("navy","darkgreen","salmon1"),5)) #cria um objeto contendo 5 repeticoes dos nomes citados 
boxplot( dap ~ local * parcela, data=cax , names=boxname, col=boxcor, horizontal=T, las=1, xlab="DAP (cm)")
######FICOU BONITAO

##Exercicio: Altura de arvores em Caixetais II####
boxplot(h~local 
        *parcela #nao pediu, fiz pq quis
        ,data= cax 
        )
##Exercicio: Estrutura de Eucaliptais####
egrandis = read.csv("egrandis.csv", sep= ";")
x11()
boxplot(dap ~ regiao
        * rotacao,
        data = egrandis)
boxname2= paste(sort(rep(unique(egrandis$regiao), 2)), rep(1:2, 2))
boxplot(dap ~ regiao
        * rotacao
        ,data = egrandis
        ,names=boxname2
        ,xlab = "DAP (cm)"
        ,main = "Nao pediu, mas fiz"
        )

##Graficos Quantil-Quantil####
####Gr·ficos Quantil-Quantil tambÈm s„o uma forma de estudar o comportamento de vari·veis
qqnorm( cax$dap ) #plota todos os quantis da minha distribuicao, com a mesma media e mesma variancia
qqline( cax$dap )#plota a linha da distribuicao normal
#teste para ver se meus dados segue uma distribuicao normal, a linha indica o seguimento de uma distribuicao normal. Quanto mais proximo dessa linha, mas perto de uma distribuicao normal os meus dados estao

vn1 = rnorm( 10000 ) #cria uma distribuicao normal com 10000 valores
qqnorm( vn1 )
qqline( vn1 )
# como o objeto vn1 eh uma distribuicao normal, pq foi criado para ter essa distribuicao com a funcao rnorm, os pontos caem qause que exatamente na linha teorica da distribuicao normal dos dados

ve1 = rexp( 100000 )
qqnorm( ve1 )
qqline( ve1 )
#algo de errado nao esta certo com o final da distribuicao dos pontos. teoricamente era pra seguir a linha plotada

ve2 = apply( matrix(ve1, ncol=100), 1, mean)
qqnorm( ve2 )
qqline( ve2 )
#ocorre exatamente igual ao que ocorreu com vn1 - segue distribuicao normal

qqplot( cax$dap[ cax$local=="retiro" ], cax$dap[ cax$local=="jureia" ] ) #plota os quantis dos dados de local jureia em funcao de local retiro
abline( 0, 1, col="red" ) #insere uma curva de distribuicao normal teorica
a = min( cax$dap[ cax$local=="jureia" ] ) #cria um objeto a com o valor minimo do local jureia
abline( a, 1, col="navy" ) #insere uma linha de distribuicao normal, com media igual ao valor minimo contido no objeto a e com inclinacao da reta de 1 (b=1), com a cor especifica
##os dados se aproximam da linha baseada no valor minimo

##Exercicio: Inventario em Floresta Plantada####
#dap
qqnorm(egrandis$dap)
qqline(egrandis$dap)
#aparentemente segue uma distribuicao quase normal

#ht
qqnorm(egrandis$ht)
qqline(egrandis$ht)
#aparentemente segue uma distribuicao quase normal

#hdom - n sei que variavel eh essa

#Exercicio: Altura de arvores em Caixetais III####
unique(egrandis$regiao) #vendo os nomes dentro da coluna regiao 
qqnorm(egrandis$ht[egrandis$regiao == "Bofete" ])
qqline(egrandis$ht[egrandis$regiao == "Bofete" ])
#tende a normalidade

qqnorm(egrandis$ht[egrandis$regiao == "Botucatu" ])
qqline(egrandis$ht[egrandis$regiao == "Botucatu" ])
#parece fugir da normalidade

qqnorm(egrandis$ht[egrandis$regiao == "Itatinga" ])
qqline(egrandis$ht[egrandis$regiao == "Itatinga" ])
#parece fugir da normalidade

qqnorm(egrandis$ht[egrandis$regiao == "Salto" ])
qqline(egrandis$ht[egrandis$regiao == "Salto" ])
#tende a normalidade

##Grafico de Variavel Quantitativa por Classes####
da = table( cax$especie[ cax$local=="jureia" ] ) #cria uma tabela dos valores do local Jureia por especie
da = sort(da, decreasing=TRUE )#ordena em ordem decerscente
dr = da/sum(da) * 100 #converte em porcentagem
barplot( dr )#plota
x11()
barplot( dr #plote um grafico de barras do objeto dr
         ,xlab="Densidade Relativa (%)" #com eixo x de nome tal...
         ,horiz=T #com orientacao horizontal das barras
         ,las=1) #com orientacao horizontal do eixo x (2 para orientacao vertical)

par( omd=c(0.2,1,0,1) ) #omd define inicio e final da regiao de plotagem, que nesse caso diminuiu para caber o nome das especies
barplot( dr , xlab="Densidade Relativa (%)", horiz=T, las=1)

par( omd=c(0,1,0,1) )     # Primeiro È necess·rio re-estabelecer o par‚metro omd
dotchart( dr )

dotchart( log(dr), xlab="Logaritmo Natural da Densidade Relativa (%)")

##Exercicio: Dominancia em Caixetais#### BUGUEI AQUI
da2 = table( caixeta$especie,caixeta$local)
da2 = sort(da2, decreasing=TRUE )
dr2 = da2/sum(da2) * 100
par( omd=c(0.2,1,0,1) ) 
barplot(dr2, xlab="Densidade Relativa (%)", horiz = T, las=1) #tem que aumentar o limite do eixo x para o tamanho de dr2, nao sei como faz

##Exercicio: Inventario em Floresta Plantada####
da3 = table(egrandis$rotacao, egrandis$regiao)
da3= sort(da3, decreasing=TRUE )
dr3= da2/sum(da2) * 100
dotchart( log(dr2), xlab="Logaritmo Natural da Densidade Relativa (%)")
