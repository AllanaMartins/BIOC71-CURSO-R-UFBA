########################################
######### Tutoriais de Programacao ####
####Disponivel em: http://ecologia.ib.usp.br/bie5782/doku.php?id=bie5782:02_tutoriais:tutorial8:start#
#### Allana Martins ###

##Testando a funcao####

media <-function(x) #faz a funcao chamada media, com um argumento
{
  soma=sum(x) #calcula a soma e joga para um objeto soma
  nobs=length(x) #conta os numeros que tem em x
  media=soma/nobs #divide a soma pelo numero de observacoes
  return(media) #retorna o valor da media
} 

ls() #mostra os objetos q estao no meu ambiente de trabalho
media #mostra as funcoes dentro da funcao media
media() #da erro pq nao foi informado o argumento x
dados=rnorm(20,2,1) #cria uma distribuicao normal de dados aleatorios com 20 valores com media 2 e desvio padrao 1 e joga no objeto dados
media(dados) #calcula a media dos valores do objeto dados
dados1=rnorm(200,2,1) #cria uma distribuicao normal de dados aleatorios com 200 valores com media 2 e desvio padrao 1 e joga no objeto dados1
media(dados1) #calcula a media dos valores do objeto dados1
dados2=(rnorm(10000,2,1)) #cria uma distribuicao normal de dados aleatorios com 10000 valores com media 2 e desvio padrao 1 e joga no objeto dados2
media(dados2) #calcula a media dos valores do objeto dados2
sd(dados) #calcula o desvio padrao dos valores do objeto dados
dados3=rnorm(20,2,0.01) #cria uma distribuicao normal de dados aleatorios com 20 valores com media 2 e desvio padrao 0.01 e joga no objeto dados3
media(dados3) #calcula a media dos valores do objeto dados3
dados4=rnorm(200,2,0.01) #cria uma distribuicao normal de dados aleatorios com 200 valores com media 2 e desvio padrao 0.01 e joga no objeto dados4
media(dados4) #calcula a media dos valores do objeto dados4
dados[2]<-NA #substitui o valor do segundo item do vetor dados por NA
dados #retorna os valores dentro do vetor dados
media(dados) #retorna a media de dados. Nesse caso voltou NA, pq alteramos o valor da segunda posicao por NA, e qualquer operacao matematica com NA resulta em NA

##Uma funcao mais elaborada####

media<-function(x,rmNA=TRUE) #cria uma funcao chamada media, com dois argumentos. Onde o segundo argumento o "rmNA" tem um valor padrao igual a TRUE
{
  if(rmNA==TRUE) #se houver NA
  {
    dados=(na.omit(x)) # omita todos os NA do objeto x e jogue dentro do objeto dados
    n.NA=length(x)-length(dados) #crie um objeto n.NA com numero de observacoes apos retirada do NA
    cat("\t", n.NA," valores NA excluídos\n") #printa a quantidade de observacoes de NA que foram removidos
  }
  else #se nao
  {
    dados=x #chama x de dados e opera as operacoes abaixo
  }
  soma=sum(dados) #calcula a soma e joga para um objeto soma
  nobs=length(dados) #conta os numeros que tem em x
  media=soma/nobs #divide a soma pelo numero de observacoes
  return(media) #retorna o valor da media
}

media(dados)

##Funcao para calcular variancia####

var.curso<-function(x) #cria uma funcao chamada var.curso, com apenas um argumento
{
  media=media(x) #calcula a media do objeto x
  dados=na.omit(x)	#omite os valores de NA dentro do objeto x, e joga dentro do objeto dados
  disvquad=(dados-media)^2 #calcular quanto o valor de dados disvia da media e eleva ao quadrado, e joga no objeto disvquad
  var.curso=sum(disvquad)/(length(dados)-1) #calcula a variancia onde soma os valores do objeto disvquad e divide pela quantidade de observacoes dos dados - 1
  return(var.curso) #retorne o valor do objeto var.curso
}

var.curso(dados) # retira os valores NA, diz quantos foram retirados e calcula a variancia
var(dados)### dica: veja o help dessa função "help(var)" #retorna NA, pois nao retira oa NA
var(dados,na.rm=TRUE) #retorna o valor da variancia, pois foi informado para retirar os NA de dados
var(dados,na.rm=FALSE) #retorna NA, pois nao foi informado para retirar os NA de dados

##Funcao para calcular o indice de Dispersao####

ID.curso<-function(x) #cria uma funcao chamada ID.curso, com um argumento
{
  id=var.curso(x)/media(x) #calcula a variancia de x, divide pela media de x e joga num vetor chamdo id
  return(id) #retorna o valor de id
}

##Simulando dados com parametros conhecidos####

#Simulando Aleatório
aleat=rpois(200,2) #cria distriubicao de poison com 200 valores
aleat #retorna o objeto

#Uniforme
unif=runif(200,0,4) #cria uma distribuicao com 200 observacoes entre 0 e 4
unif #retorna unif
unif=round(unif,0) #arredonda os valores para numeros inteiros
unif #retorna unif

#Agregado
agreg=round(c(runif(100,0,1),runif(100,5,10))) #agregaduas distribuicoes, arredonda elas e salva no objeto agreg
agreg #retorna agreg

#Calcular o coeficiente de dispersao para as tres distribuicoes

ID.curso(aleat)

ID.curso(unif)

ID.curso(agreg)

##Funcao para criar o teste de signficancia do ID####

test.ID <- function(x, nsim=1000) #cria uma funcao chamada test.ID, com dois argumentos: um eh x e numero de simulacoes do vetor da simulacao = 1000
{ 
  ID.curso=function(x){var(x)/mean(x)}# essa função precisa das funcoes media e ID.curso
  dados=na.omit(x) #retira os NA e joga no objeto
  ndados=length(dados) #cria objeto dados com o tamanho dos dados sem os NA
  med=mean(dados) #calcula a media dos dados e joga no objeto med
  id=var(dados)/med #calcula o indice o indice de dispersao 
  simula.aleat=rpois(length(dados)*nsim, lambda=med) #cria um simulacao aleatoria com o tamanho dos dados multiplicado pelo numero (1000) de respostas, com lambida igual a media
  sim.dados=matrix(simula.aleat,ncol= ndados) #cria uma matriz com o objeto da simulacao, com o nome das colunas determinadas por ndados e joga num objeto sim.dados
  sim.ID=apply(sim.dados,1,ID.curso) #ele ira calcular o indice de dispersao em todas as linhas da matriz sim.dados e joga no objeto sim.ID
  quant.ID=quantile(sim.ID, probs=c(0.025,0.975)) #calcula os quantis 2.5% e 97.5% do objeto sim.ID.  Pega 5% e divide para cara lado da distribuicao bicaudal
  if(id>=quant.ID[1] & id<=quant.ID[2]) #se o indice estiver entre os dois valores (dentro do intervalo de confianca
     {
    cat("\n\n\n\t distribuição aleatória para alfa=0.05 \n\t ID= ",id,"\n\n\n") #printa a frase entre as aspas
  }
  if(id < quant.ID[1]) #se o indice for menor que 2.5%
  { 
    cat("\n\n\n\t distribuição uniforme, p<0.025 \n\t ID= ",id,"\n\n\n") #printa a frase em aspas
  }
  if(id>quant.ID[2]) #se o indice for maior que 97.5%
  { 
    cat("\n\n\n\t distribuição agregado, p>0.975 \n\t ID= ",id,"\n\n\n") #printa a frase em aspas
  }
  resulta=c(id,quant.ID) #junta o indicei e os quantis em um vetor chamado resulta
  names(resulta)<-c("Indice de Dispersão", "critico 0.025", "critico 0.975") #nomeia os objetos do vetor resulta
  return(resulta) #retorna os objetos
}

#Testanto os dados simulados
#aplica a funcao nos tres objetos
test.ID(aleat)
test.ID(agreg)
test.ID(unif)

##Outra funcao####

eda.shape <- function(x) #cria uma funcao chamada eda.shape com um argumento
{
  x11() #abre janela externa
  par(mfrow = c(2,2))	## muda o dispositivo gráfico para 2x2
  hist(x)                 ## produz histograma de x
  boxplot(x) #produz o boxplot de x
  iqd <- summary(x)[5] -	summary(x)[2]     ## faz a diferença entre o quinto elemento x e o segundo
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l") #plota densidade probabilistica baseado nos seus dados
  qqnorm(x) #plota todos os quantis da minha distribuicao, com a mesma media e mesma variancia
  qqline(x) #plota a linha da distribuicao normal
  par(mfrow=c(1,1)) #reseta o mfrow
  
}

set.seed(22) ## estabelece uma semente aleatoria 
dados.pois20<-rpois(20,lambda=6) ## sorteia dados aleatorios de uma funcao poisson com media 6
sum(dados.pois20) ## a somatoria aqui sempre dara 131, somente porque a semente eh a mesma
set.seed(22)
dados.norm20<-rnorm(20,mean=6, sd=2) ## sorteia 20 dados de uma funcao normal com media 6 e dp = 2
sum (dados.norm20)               ### aqui o resultado dara sempre 130.48

###aplicar eda.shape para dados.dens

eda.shape(dados.pois20)

eda.shape(dados.norm20)

###aumentando a amostra

eda.shape(rpois(500,6))

eda.shape(rnorm(500,6))

##Modificando uma funcao####

eda.shape1 <- function(x) #cria uma funcao chamada eda.shape1 com um argumento
{
  x11()
  par(mfrow = c(2,2))
  hist(x,main="Histograma de x")
  boxplot(x, main="BoxPlot de x")
  iqd <- summary(x)[5] -	summary(x)[2]
  plot(density(x,width=2*iqd),xlab="x",ylab="",type="l", main="Distribuição de densidade de x")
  qqnorm(x,col="red",main="Gráfico Quantil x Quantil",xlab="Quantil Teórico",ylab="Quantil da Amostra")
  qqline(x)
  par(mfrow=c(1,1))
  
}
#a funcao eda.shape1 se direncia da eda.shape pelas condicoes da aparencia
##Executando a funcao modificada####

eda.shape1(rnorm(500,6))

##Fazendo ciclos de operacoes####

x1=rpois(20,1) #sorteia dados aleatorios de uma funcao poisson com media 1
x2=rpois(20,2) #sorteia dados aleatorios de uma funcao poisson com media 2
x3=rpois(20,3) #sorteia dados aleatorios de uma funcao poisson com media 3
x4=rpois(20,1) #sorteia dados aleatorios de uma funcao poisson com media 1
sp.oc=matrix(c(x1,x2,x3,x4), ncol=4) #cria uma matriz com os objetos acima criados e joga em um objeto
colnames(sp.oc)<-c("plot A", "plot B", "plot C", "plot D") #nomeia as colunas da matriz
rownames(sp.oc)<-paste("sp", c(1:20)) #determina a sequencia de especies (de 1 a 20) das linhas da matriz
str(sp.oc) #retorna a estrutura da matriz
dim(sp.oc) #retorna a dimensao da matriz
head(sp.oc) #retorna as 6 primeiras linhas da matriz

#Uma funcao para contar especies por parcelas####
dados = matrix(c(x1,x2,x3,x4), ncol=4)
n.spp<-function(dados) #cria uma funcao chamada n.spp com um argumento
{
  nplot=dim(dados)[2] #diz o numero de linhas da matriz
  resultados=rep(0,nplot) #cria uma repeticao de 0, com o tamanho de nplot e coloca no objeto resultados
  names(resultados)<-paste("n.spp", c(1:nplot)) #nomeia o objeto resultados de n.spp variando de um até tamanho de nplot
  dados[dados>0]=1 #indexa em dados, os numeros maiores que zero e substitui por 1
  for(i in 1:(dim(dados)[2])) #cria um loop que vai de 1 ate o tamanho da dimensao de dados do segundo elemento (4 linhas)
  {
    cont.sp=sum(dados[,i]) #soma todas as linahs da coluna i e joga no objeto cont.sp
    resultados[i]=cont.sp #ele joga os resultados da soma no objeto resultados
  }
  return(resultados)
}


##### Aplicando a função 

n.spp(sp.oc) #retorna a quantidade de individuos por cada especie####

##Similaridade####
sim<-function(dados)
{
  nplot=dim(dados)[2] #cria um objeto nplot de dimensao do objeto dados 
  similar=matrix(1,ncol=nplot,nrow=nplot) #cria uma matiz chamada similar, com o nome de linhas preenchidas pelo objeto nplot e numero de colunas determinada pelo tamanho do mesmo objeto, preenchida internamente pelo numero 1
  rownames(similar)<-paste("plot", c(1:nplot)) #renomeia as linhas com a sequencia da palavra plot, comecando por 1 e terminando com o tamanho do objeto nplot (4 linhas)
  colnames(similar)<-paste("plot", c(1:nplot)) #renomeia as colunas com a sequencia da palavra plot, comecando por 1 e terminando com o tamanho do objeto nplot (4 colunas)
  dados[dados>0]=1 #substitui os valores maiores que 0, por um no objeto dados
  for(i in 1:nplot-1) #cria loop de 1 ate o tamanho de nplot - 1
  {
    m=i+1
    for(m in m:nplot)
    {
      co.oc=sum(dados[,i]>0 & dados[,m]>0)
      total.sp=sum(dados[,i])+sum(dados[,m])-co.oc
      similar[i,m]=co.oc/total.sp 
      similar[m,i]=co.oc/total.sp 
    }
    
  }
  return(similar)
}
##Aplicando a funcao SIM#

sim(sp.oc) #a funcao cria uma matriz par a par dos dados, com o valor da diagonal igual a 1

##Exercicios 9 - Construcao de Funcoes####

## Exercicio Analise exploratoia simultanea de duas variaveis ####

x = sample(1:100, 100, replace=T)
y = sample (1:200, 100, replace= T)

x=rnorm(100,8,1)
x=round(x,0)
y=rnorm(100,6,1)
y=round(y,0)

analise.expor = function(x,y) {
  x11()
  par(mfrow= c(2,2))
  hist(x, prob=T, main="Histograma de x") #plota histograma
  lines(density(x),col="blue") #desenha uma linha de densidade probabil�stica da normal 
  qqnorm(x,col="blue",main="Grafico de X",xlab="Distribuicao normal",ylab="Dados") ##plota os dados contra uma distribuicao normal
  qqline(x) ##plota uma linha para facilitar a comparacao.
  hist(y, prob= T, main="Histograma de y") #plota histograma
  lines(density(x),col="red") #desenha uma linha de densidade probabil�stica da normal 
  qqnorm(y,col="red",main="Grafico de Y",xlab="Distribuicao normal",ylab="Dados") ##plota os dados contra uma distribuicao normal
  qqline(y) ##plota uma linha para facilitar a comparacao.
  
  cor= cor.test(x,y) #executa uma correlacao
  
  summary.x = summary(x) #cria objeto com dados de estatistica descritiva do vetor x
  summary.y = summary(y) #cria objeto com dados de estatistica descritiva do vetor y
  
  s1=matrix(summary.x,ncol=6,nrow=1) #cria matrix com dados do sum�rio de x
  rownames(s1)<-c("x") 
  colnames(s1)<-c("minimo", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  
  s2=matrix(summary.y,ncol=6,nrow=1) #cria matrix com dados do sum�rio de y
  rownames(s2)<-c("y") 
  colnames(s2)<-c("minimo", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
  
  summary=list(X = s1, Y = s2, z= cor) #cria uma lista com s1, s2 e cor
  
  return(summary)
}


analise.expor(x,y)

