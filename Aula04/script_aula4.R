###################################################
##########5. Criação e Edição de Gráficos no R
######Diponível em http://ecologia.ib.usp.br/bie5782/doku.php?id=bie5782:03_apostila:05a-graficos&rev=1459523589 & http://ecologia.ib.usp.br/bie5782/doku.php?id=bie5782:01_curso_atual:exercicios5

riqueza = c(15,18,22,24,25,30,31,34,37,39,41,45) #cria vetor riqueza
area = c(2,4.5,6,10,30,34,50,56,60,77.5,80,85) #cria vetor area
area.cate = rep(c("pequeno", "grande"), each=6) #cria vetor area.cat

plot(riqueza~area) #plota um gráfico de riqueza em função da área
plot(area,riqueza) # o mesmo que o anterior
boxplot(riqueza~area.cate) #plota os mesmos dados em formato boxplot
barplot(riqueza) #plota riqueza em formato de barra

#Exercicio 1: Fazendo os Primeiros Graficos####
#Construa "plot, boxplot e barplot usando as variáveis do conjunto de dados Conjunto de Dados: Dados de Biomassa de Árvores de Eucalyptus Saligna, para explorar relacoes entre:

dados_biomassa = read.csv (file.choose(),dec = ".", sep=",") #carreguei o arquivo "saligna" e informei para ler em csv 

#dap e ht 
plot(dados_biomassa$dap ~ dados_biomassa$ht) #grafico de distribuiao de pontos, onde varia diametro em funcao da altura
boxplot(dados_biomassa$dap ~ dados_biomassa$arvore) #cria um gráfico boxplot do diametro em funcao da arvore 
barplot(dados_biomassa$dap) #cria um grafico de barras com a variacao dos valores do diametro

#ht e tronco
plot(dados_biomassa$tronco ~ dados_biomassa$ht) #grafico de distribuicao de pontos, onde varia trono em funcao da altura
boxplot(dados_biomassa$tronco ~ dados_biomassa$arvore) #cria um grafico boxplot do tronco em funcao de cada arvore
barplot(dados_biomassa$tronco)#cria um grafico de barras com a variacao dos valores do tronco

#dap e classe
plot(dados_biomassa$dap ~ dados_biomassa$classe) #cria um grafico boxplot, ja que "classe" e uma categoria, do diametro em funcao de cada arvore
boxplot(dados_biomassa$dap ~ dados_biomassa$classe) #cria um grafico boxplot do diametro em funcao de cada arvore
barplot(dados_biomassa$dap) #cria um grafico de barras com a variacao dos valores do diametro

#dap e talhao
plot(dados_biomassa$dap ~ dados_biomassa$talhao) #cria uma distribuicao de pontos, dentro do que aparenta ser uma categoria (talvez local)
boxplot(dados_biomassa$dap ~ dados_biomassa$talhao) #categoriza talhao e plota o diametro em funcao dele
barplot(dados_biomassa$dap) #cria um grafico de barras com a variacao dos valores do diametro

#dap
plot(dados_biomassa$dap) #ele entende que diametro e uma serie temporal e plota uma distribuiacao de pontos
boxplot(dados_biomassa$dap) # cria um boxplot do diametro com a  mediana desses valores e com o valor maximo e minimo

#ht
plot(dados_biomassa$ht) #ele entende que altura e uma serie temporal e plota uma distribuicao de pontos
boxplot(dados_biomassa$ht) # cria um boxplot da altura com a representatividade da mediana desses valores com o valor maximo e minimo
barplot(dados_biomassa$ht) #cria um grafico de barras com a variacao dos valores da altura

par(las=1)
plot(riqueza~area, las=3) #cria um grafico de dispersao de pontos de riqueza em função de area, com as legendas lidas na vertical
par(cex=2)
plot(riqueza~area, cex=2) #aumenta as legendas para um tamanho 2 e os pontos para o tamanho 4
dev.off()
#Exercicio 2: Aprendendo a Editar Graficos####
?plot()
dev.off() #deleta os comandos do par()
riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45) #cria objeto riqueza
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85) #cria objeto area
plot(riqueza ~ area, xlab = "Tamanho da ilha", ylab= "Riqueza de espécies", main = "Aves das Ilhas Samoa") #retorna um grafico de dispersao de pontos de riqueza em função de area

?par() #define parametros para todos os graficos que serao plotados apos a execucao dessa funcao 
par(pch =8 #define o tipo do ponto
    ,cex= 1 #define tamanho dos pontos e legenda
    ,las= 1 #define a direcao da escala do grafico
    ,family="serif" ) #não vai
plot(riqueza ~ area, xlab = "Tamanho da ilha", ylab= "Riqueza de espécies", main = "Aves das Ilhas Samoa",family="serif") #plota o grafico com os parametros
dev.off()

#Sobre o par()####
par(mfrow=c(2,1)) #define quantas figuras serao expostas dentro de uma mesma figura total. Onde tem duas linhas e uma coluna
x11()
plot(riqueza~area)
boxplot(riqueza~area.cate)
dev.off()

par(mfrow=c(1,2)) #define quantas figuras serao expostas dentro de uma mesma figura total. Onde tem uma linha e duas colunas
plot(riqueza~area)
boxplot(riqueza~area.cate)
dev.off()

par(mfrow=c(2,1)) #define quantas figuras serao expostas dentro de uma mesma figura total. Onde tem uma linha e duas colunas
par(mar=c(4,14,2,6)) #controla o tamanho das margens - deixou os graficos pequenininhos
plot(riqueza~area) 
boxplot(riqueza~area.cate) 
dev.off()

par(mfrow=c(1,2))
par(mar=c(14,4,8,2)) #centralizou os graficos
plot(riqueza~area)
boxplot(riqueza~area.cate) 
dev.off()

par(mfrow=c(1,2))
par(mar=c(8,4,8,1))
plot(riqueza~area)
par(mar=c(14,2,4,0.5))
boxplot(riqueza~area.cate) 
#deixou os graficos em posicoes diferentes (alterando suas margens) dentro do plot
dev.off()

#Exercicio 3: Mudando diferentes Graficos####
riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
area.cate <- rep(c("pequeno", "grande"), each=6)

plot(riqueza~area)
plot(riqueza~area, bty="l", tcl=0.3) #a linha lateral direita e a barra superior sumiu. Alem disso a diposicao dos ticks foram alteradas para o interior da area do grafico 

boxplot(riqueza~area.cate, bty="l", tcl=0.3) #nao atribui os parametros designados

par(bty="l")
par(tcl=0.3)
boxplot(riqueza~area.cate)
# os parametros foram obedecidos, igual ao que ocorreu na funcao plot. Alem de mudar o tipo do grafico para um boxplot
dev.off()
#Exercicio 4####
riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
abundancia <- rev(riqueza)

plot(riqueza ~area) #cria um grafico de dispersao de pontos de riqueza em funcao da area
lines(lowess(area, riqueza)) #insere uma reta nao-parametrica
model = lm(riqueza~area) #cria um modelo linear dos dados, e joga dentro de um vetor model
plot(riqueza~area) #cria um grafico de dispersao de pontos de riqueza em funcao da area
abline(model) #insere uma linha de tendencia a partir do modelo linear contido no objeto model
plot(riqueza~area)
abline(15,0.4) #insere uma reta com a=15 e inclinacao 0.4
plot(riqueza~area)
abline(v=mean(area)) #insere uma linha vertical no valor da media da area
abline(h=mean(riqueza)) #insere uma linha horizontal no valor da media da riqueza
plot(riqueza~area)
text(20,40,"texto") #insere um texto na intersessao dos pontos x=20 e y=40
plot(riqueza~area)
mtext("legenda no lado errado" #define o escorpo do texto a ser inserido
      ,side=4 #define qual o lado externo que o texto serao inserido
      ,line=0.2 #define o posicionamento horizontal no lado designado
      ,at=20 #define o posicionamento vertical no lado designado
      ,cex=2 #define o tamanho da fonte
      ,family="serif") #define o tipo da fonte

plot(riqueza~area) #plota o grafico da riqueza em funcao da area
par(new=TRUE) #permite plotar um novo grafico dentro do ja plotado
plot(abundancia~area, col="red") #retorna o gragico plotado dentro do anterior

#Plotando dois graficos em um so####
plot(riqueza~area) #plota o grafico da riqueza em funcao da area
par(new=TRUE) #permite plotar um novo grafico dentro do ja plotado
plot(abundancia~area
     ,axes=FALSE #diz para nao plotar os eixos desse grafico
     ,ann=FALSE #diz para n�o plotar a legenda do lado direito
     ,pch=16) #define o tipo do ponto
axis(4) #insere um novo eixo aos graficos plotados juntos
jpeg(filename = "Algumnome.jpg")
dev.off()

plot(riqueza~area)#plota o grafico da riqueza em funcao da area
arrows(20,30,40,25) #insere uma seta nos intervalos indicados
rect(20,20,40,40) #insere um retangulo ou quadrado nos intervalos indicados

#Salvando arquivos####
jpeg(filename = "Rplotaula.jpg" #define nome do arquivo
     ,width = 480 #define largura em pixels
     ,height = 480 #define altura em pixels
     ,units = "px"
     ,pointsize = 12
     ,quality = 100
     ,bg = "white"
     ,res = NA
     ,restoreConsole = TRUE)

par(mfrow=c(1,2)) # define que serao expostos dois graficos, lado a lado (1 uma e 2 colunas) dentro de uma mesma figura total.
par(mar=c(14,4,8,2)) #define tamanho das margens
plot(riqueza~area)
boxplot(riqueza~area.cate)

dev.off() #cancela os comandos de par e fecha os graficos


#Exercicios 5####
##5.1 Editando alguns paremetros graficos##

plot(dados_biomassa$dap ~ dados_biomassa$ht
     ,xlab = "Altura (m)" #define nome do eixo x
     ,ylab = "DAP (cm)" #define nome do eixo y
     ,bty = "l" #define tipo do formato dos eixos
     ,tcl = 0.3 #altera a disposi��o do tick
     ,main = "Biomassa de Eucalyptus saligna" #tentei colocar o nome da esp�cie entre <i> </i> para ver se ficava italico e nao foi
     ,cex = 1 #define o tamaho das fontes
     )
dev.off()

##5.2 Dois graficos juntos####
x11()
par(mfrow=c(1,2))
boxplot(dados_biomassa$dap ~ dados_biomassa$talhao) #primeiro grafico
text(16,20, "A")


m1=mean(dados_biomassa$dap[dados_biomassa$talhao=="16"])
m2=mean(dados_biomassa$dap[dados_biomassa$talhao=="17"])
m3=mean(dados_biomassa$dap[dados_biomassa$talhao=="18"])
m4=mean(dados_biomassa$dap[dados_biomassa$talhao=="22"])
m5=mean(dados_biomassa$dap[dados_biomassa$talhao=="23"])
m6=mean(dados_biomassa$dap[dados_biomassa$talhao=="32"])

sd1=sd(dados_biomassa$dap[dados_biomassa$talhao=="16"])
sd2=sd(dados_biomassa$dap[dados_biomassa$talhao=="17"])
sd3=sd(dados_biomassa$dap[dados_biomassa$talhao=="18"])
sd4=sd(dados_biomassa$dap[dados_biomassa$talhao=="22"])
sd5=sd(dados_biomassa$dap[dados_biomassa$talhao=="23"])
sd6=sd(dados_biomassa$dap[dados_biomassa$talhao=="32"])

avg= c(m1, m2, m3, m4, m5, m6)
sdev= c(sd1, sd2, sd3, sd4, sd5, sd6)

plot(x= c(1:6), y= avg,cex=1.5, pch=16,col=c(1:6),ylim=range(c(avg-sdev, avg+sdev)))
arrows(x0=c(1:6), y0=avg-sdev, x1=c(1:6), y1=avg+sdev, length=0.05, angle=90, code=3)
abline(h=mean(avg), col="red")
text(1.5,20,"B")


## 5.3 Adivinhando o c�digo ####
adv= read.csv("ex3.csv")

x11()
par(mfrow=c(1,2))

plot(x= adv$x1
     ,y=adv$y1
     ,ylim=c(0,3)
     ,ylab= ""
     ,xlab="Log (Patch size)(ha)"
     ,pch = 17
     ,bty= "l"
)
abline(lm(adv$y1~adv$x1), lwd = 2)
text(2.5,3, "a")
mtext("Euclidean distances", side=2,line= 2.2, cex=1.5)


par(bty= "l")
boxplot(adv$x2~adv$y2, ylim= c(0,3), cex.axis= 0.6, names=c("Small","Medium Edge","Medium Interior","Large Edge","Large Interior","Control"))

text(6,2.9, "b")
points(x=1, y=3, pch=8, lwd = 1)
points(x=2, y=3, pch=8, lwd = 1)
points(x=2.9, y=3, pch=8, lwd = 1)
points(x=3.1, y=3, pch=8, lwd = 1)
points(x=4, y=3, pch=8, lwd = 1)
points(x=4.9, y=3, pch=8, lwd = 1)
points(x=5, y=3, pch=8, lwd = 1)
points(x=5.1, y=3, pch=8, lwd = 1)

