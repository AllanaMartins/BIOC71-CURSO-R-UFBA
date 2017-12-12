######################################################
# Introdução Linguagem R
# Vitor Passos Rios (bruxao)
# Universidade Federal da Bahia
# https://github.com/vrios/Intro-Linguagem-R/wiki
#####################################################

#quando possível, sua resposta deve ser um código do R

#####Trabalhando com dados#####
# o que é um dataframe? Qual a diferneça entre um dataframe e um list?
#uma lista ? um objeto composto por vetores que podem ter tamanhos e classes diferentes, um data frame ? um tipo especial de list onde os vetores possuem o mesmo tamanho, mas podem ter classes diferentes  

# o que as funções abaixo fazem? O que elas retornam?
is.vector(aves.c$urubu) #confirma que a linha urubu dentro do arquivo aves.c ? um vetor
is.numeric(aves.c$urubu) #confirma que a linha urubu dentro do arquivo aves.c ? num?rico
is.character(aves.c$urubu) #diz que a linha urubu dentro do arquivo aves.c n?o ? um car?ter
is.matrix(peso) #n?o existe matriz "peso" feita 
is.na(aves.c$urubu) #mostra onde est? todos os NA na linha urubu
is.data.frame(aves.c$urubu) #mostra falso, pois a linha urubu no arquivo aves.c n?o ? uma lista de vetores
            



#o que queremos dizer quando dizemos uma função "retorna" alguma coisa? #dizemos que ela mostra o resultado da fun??o que foi executada
#vo? pode colocar seu resultado dentro de um objeto. o seu resultado pode ser um gr?fico que vc pode colocar num objeto pra usar depois 
# is.data.frame(caixeta)
#retornado = is.data.frame(caixeta)
#o que a função summary() faz? #mostra um sum?rio estat?stico do que eu selecionei que ele calcule 
summary(aves.c$urubu)

#Três áreas foram submetidas aplicação de três adubos diferentes, e uma foi
#mantida como controle. A biomassa seca (kg) por parcela foi medida para
#verificar se houve efeito do adubo. Os dados obtidos foram:

#(Controle)	A	37	34	36	42	31  33
#B	62	54	57	64	60
#C	63	58	60	62	61 44
#D	41	37	38	49	35 49
#Crie um dataframe com estes dados

Controle = c(37,34,36,42,31,33)
AdubB = c(62,54,57,64,60)
AdubC = c(63,58,60,62,61,44)
AdubD = c(41,37,38,49,35,49)
adubos = c(rep ("Controle", 6), rep ("AdubB", 5), rep ("AdubC", 6), rep ("AdubD", 6))
biomassa = data.frame (x= adubos , y = c(Controle, AdubB, AdubC, AdubD))
nomescolunas = c("tratamento", "peso")
colnames(biomassa) = nomescolunas

#Calcule a média de cada tipo de adubo, sem usar summary(), usando o dataframe
#que você criou acima
mediaControle = mean(biomassa$peso[biomassa$tratamento == "Controle"])
mediaAdubB = mean(biomassa$peso[biomassa$tratamento == "AdubB"])
mediaAdubC = mean(biomassa$peso[biomassa$tratamento == "AdubC"])
mediaAdubD = mean(biomassa$peso[biomassa$tratamento == "AdubD"])


#que problemas você teve ao fazer o exercício acima? criar um data.frame com vetores com tamanhos diferentes

# O que a função unique() faz?
unique(biomassa$tratamento) #mostra quantos e quais nomes diferentes existem na coluna indicada

#Usando o conjunto de dados caixeta.csv, disponivel em
#http://ecologia.ib.usp.br/bie5782/doku.php?id=dados:dados-caixeta, calcule
#quantas espécies estão presentes na coluna especie. Para ler o arquivo, execute
#o código abaixo
caixeta = read.table("caixeta.csv", header=T, sep=",", as.is=T)
unique(caixeta$especie)
sort(unique(caixeta$especie))
caixeta$especie[caixeta$especie == "Callophyllum brasiliensis"] = "Calophyllum brasiliensis"
sort(unique(caixeta$especie))
#existem 42 esp?cies diferentes

# o arquivo deve estar no seu diretório de trabalho
str(caixeta) #o que este comando faz? mostra a estrutura interna de um objeto
summary(caixeta) #mostra o resumo estat?stico dos meus dados. s? que faz os c?lculos de tudo junto, n?o serve. terei que indexar a vari?vel que eu quero

#podemos criar uma coluna a mais no nosso dataframe apenas dizendo ao R o que
#colocar lá. Por exemplo, criamos uma coluna com o nome do coletor do dado
caixeta$coletor= "Darwin" #regra da reciclagem
str(caixeta) 

#agora adicione no objeto caixeta uma coluna chamada "desvio" com o quanto a
#altura de cada árvore difere da média de todas as árvores
mediaTotal = mean(caixeta$h)
desvio = caixeta$h - mediaTotal
caixeta$desvio = desvio
#forma simplificada: caixeta$desvio = caixeta$h - mean(caixeta$h)

#Usando a função table(), conte quantos indivíduos de cada espécie existem
table(caixeta$especie)

#A função table() também pode contar por espécie em cada local, depois por local
#em cada espécie). Como?
table(caixeta$especie, caixeta$local)
#tudo que tiver dentro do par?ntese, ele vai entender que ? um vetor que vai jogar pra dentro de um objeto
#crie um objeto chamado pequenas que contenha todas as árvores com altura (h)
#menor que um metro.(Os dados no arquivo estão em centímetros)
pequenas = caixeta[caixeta$h < 100, ]
#crie um objeto chamado grandes que contenha todas as árvores acima de 3 metros
grandes = caixeta[caixeta$h > 300, ]
#crie um objeto chamado medias que todas as árvores com alturas entre um metro e
#tres metros
medias = caixeta[caixeta$h >= 100 & caixeta$h <= 300,]

#agora em uma linha de comando, crie um objeto que contenha a quantidade de
#árvores pequenas, médias e grandes, separadamente
todas = c(length(pequenas), length(media), length(grandes))
#mostra um comprimento de um vetor de data.frame ou matriz (ambos s?o conjuntos de vetores), se eu n?o especificar ele ir? me mostrar o n?mero de vetores dentro do data.frame ou matriz
todas = c(length(pequenas$local), length(medias$local), length(grandes$local))
#Crie um objeto chamado obj somente com os individuos de "Alchornea
#triplinervia" Em quantas localidades esta espécie ocorre? Qual comando para
#encontrar essa informação?
obj = caixeta[caixeta$especie == "Alchornea triplinervia",]

#podemos remover dados de um dataframe, usando o sinal de menos
###### which() d? os endere?os de tudo que coumprir a condi??o que tem depois dele
caixeta.sem.tabebuia = caixeta[ - which(caixeta$especie == "Tabebuia cassinoides"),] #ele tirou tudo que tem essa esp?cie do data.frame
unique(caixeta.sem.tabebuia$especie) # cita as coisas dentro do vetor
sort(unique(caixeta.sem.tabebuia$especie)) # cite em ordem alfab?tica

#agora crie um objeto removendo todas as "Tabebuia cassinoides" do local retiro
#(somente do local retiro)
#crio um vetor "caixeta.sem.retiro" e mostro uma condi??o criando um vetor (caixeta$local == "retiro" & caixeta$especie == "Tabebuia cassinoides"), mas coloco o !. esse sinal vai criar um objeto que N?O ser essa condi??o que eu especifiquei
caixeta.sem.retiro = caixeta[ #indexo a condi??o dentro de caixeta
                            !( caixeta$local == "retiro" 
                              &
                              caixeta$especie == "Tabebuia cassinoides"),
                            ]

#crie os objetos abaixo, junte eles num dataframe de duas colunas chamado comNA,
#depois crie um novo objeto chamado semNA, removendo todas as linhas que
#contenham NA, usando a notação acima (dica: só se usa "" para valores do tipo
#character)
id = 1:80
a = c(121.72355, 103.79754, 130.15442, 98.29305, 103.43365, 102.44998,
          NA, 111.07215, 113.74047, 103.16081, 80.87149, 98.66692,
          65.09549, 155.74965, 88.30168, 147.43610, 114.60806, 109.87433,
          149.54772, 83.54137
)
b = c(77.91352, 78.07251, 81.95604, 75.64862, 78.45213, 79.11058,
                79.98952, 79.18127, 840.1635, 74.86860, 82.01886, 78.26936,
                77.94691, 78.75372, 77.64901, NA, 77.19803, 72.48175,
                83.45336, 78.99681
)
c = c(127.9366, 201.7158, NA, 136.5880, 131.7213, 118.1486,
            125.7534, 139.6544, 163.5890, 139.7455, NA, 141.4450, 110.7311,
            157.5921, 176.8437, 102.8659, 121.8286, 134.7097, 157.1392, 166.7133
)
d = c(191.3721, 216.1671, 165.4380, 196.2730, 172.6565, 178.2955,
                 193.6687, NA, 160.2968, 208.4400, 204.0934, 208.1798,
                 186.6380, 193.9446, NA, 198.6853, 213.8838, 210.1881,
                 209.9109, 210.9228
)
comNA = data.frame (id, c(a,b,c,d))
semNA = comNA[!is.na(comNA$c.a..b..c..d.),] #is.na busca todos os NA da coluna "c.a..b..c..d." do dataframe comNA, o ! me d? tudo oq N?O for a condi??o que eu indiquei
semNA2 = na.omit(comNA) #faz a mesma coisa do de cima

#Abaixo temos dados de tamanho de mandíbula de chacal dourado. Crie um dataframe
#com esses dados. Qual a média por sexo? E para a espécie? Guarde estes valores
#em tres objetos diferentes.
#machos: 120 107 110 116 114 111 113 117 114 112
#femeas: 110 111 107 108 110 105 107 106 111 111 Quantos machos têm a mandíbula
#maior do que a média das fêmeas?
machos= c(120,107,110,116,114,111,113,117,114,112)
machas= c(110,111,107,108,110,105,107,106,111,111)
media.machos = mean(machos)
media.machas = mean(machas)
media.pop = mean(c(machos,machas))
machos.grandes = machos[machos > media.machas] #usando os objetos separados
tabela = data.frame(machos,machas)
machos.grandes2 = tabela$machos[machos > media.machas] #usando dataframe

#escreva **uma** linha de código que testa se um número é par, e caso seja,
#manda uma mensagem dizendo "par". dica: use a função %% (resto da divisão, ou
#remainder em inglês) e a função message()

###if
teste.de.normalidade = shapiro.test(dragoes$y)
if (teste.de.normalidade$p.value >= 0.05) { #o if olha se a condi??o dentro do () ? verdadeira,
  #se for verdadeira, ele executa oq tiver entro {}
  fit = lm(dragoes$y ~ dragoes$x)
  summary(fit)
}

#agora crie um código  como acima, mas que diga "par" se for par e "impar" se
#for impar. Dica: leia sobre as funções else e ifelse
if(8 %% 2 == 0){message("par")}
if(9 %% 2 != 0){message("?mpar")}

if(8 %% 2 == 0) {
  message("par")
} else {                    
  message ("?mpar")
}

if(dado seguir uma normal)
{teste param?trico}
else {teste n?o param?trico} #se o dado seguir uma normal, realiza teste param?trico, 
                            #se n?o for, realiza o n?o param?trico
ifelse #quando for pouca coisa

#crie um ciclo (for) que mostre o console todos os inteiros de 1 até 10, um por
#um
for (i in 1:10) {
  a = i^2
  message(a)
}

for (i in 6:0) {
  a = 
  message (i)
}
#usar o exemplo da caixeta


#crie um ciclo (for) que some todos os inteiros de 25 até 43, guardando esses
#valores num objeto soma (dica: crie o objeto soma com valor zero antes do
#código do ciclo)

#####Desafio level Hard#####
#Escreva um código que escreva os números de um a 100, com as seguintes
#condições: Se o número for multiplo de 3, ao invés do dígito, escreva "Fu". Se
#o número for múltiplo de 5, ao invés do dígito, escreva "bá". Para números que
#forem multiplos tanto de 3 quanto de 5, escreva "Fubá". A melhor solução ganha
#um chocolate
