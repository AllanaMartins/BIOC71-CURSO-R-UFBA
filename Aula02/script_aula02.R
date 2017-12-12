######################################################
# Introdu√ß√£o Linguagem R
# Vitor Passos Rios
# Universidade Federal da Bahia
# https://github.com/vrios/Intro-Linguagem-R/wiki
#####################################################

#quando poss√≠vel, sua resposta deve ser um c√≥digo do R

#####Trabalhando com dados#####
# o que √© um dataframe? Qual a diferne√ßa entre um dataframe e um list?
#uma lista È um objeto composto por vetores que podem ter tamanhos e classes diferentes, um data frame È um tipo especial de list onde os vetores possuem o mesmo tamanho, mas podem ter classes diferentes 

# o que as fun√ß√µes abaixo fazem? O que elas retornam?
is.vector(aves.c$urubu) #confirma que a linha urubu dentro do arquivo aves.c È um vetor
is.numeric(aves.c$urubu) #confirma que a linha urubu dentro do arquivo aves.c È numÈrico
is.character(aves.c$urubu) #diz que a linha urubu dentro do arquivo aves.c n„o È um car·ter
is.matrix(peso) #n„o existe matriz "peso" feita 
is.na(aves.c$urubu) #mostra onde est· todos os NA na linha urubu
is.data.frame(aves.c$urubu) #mostra falso, pois a linha urubu no arquivo aves.c n„o È uma lista de vetores
            



#o que queremos dizer quando dizemos uma fun√ß√£o "retorna" alguma coisa? #dizemos que ela mostra o resultado da funÁ„o que foi executada
#voÍ pode colocar seu resultado dentro de um objeto. o seu resultado pode ser um gr·fico que vc pode colocar num objeto pra usar depois 
# is.data.frame(caixeta)
#retornado = is.data.frame(caixeta)
#o que a fun√ß√£o summary() faz? #mostra um sum·rio estatÌstico do que eu selecionei que ele calcule 
summary(aves.c$urubu)

#Tr√™s √°reas foram submetidas aplica√ß√£o de tr√™s adubos diferentes, e uma foi
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

#Calcule a m√©dia de cada tipo de adubo, sem usar summary(), usando o dataframe
#que voc√™ criou acima
mediaControle = mean(biomassa$peso[biomassa$tratamento == "Controle"])
mediaAdubB = mean(biomassa$peso[biomassa$tratamento == "AdubB"])
mediaAdubC = mean(biomassa$peso[biomassa$tratamento == "AdubC"])
mediaAdubD = mean(biomassa$peso[biomassa$tratamento == "AdubD"])


#que problemas voc√™ teve ao fazer o exerc√≠cio acima? criar um data.frame com vetores com tamanhos diferentes

# O que a fun√ß√£o unique() faz?
unique(biomassa$tratamento) #mostra quantos e quais nomes diferentes existem na coluna indicada

#Usando o conjunto de dados caixeta.csv, disponivel em
#http://ecologia.ib.usp.br/bie5782/doku.php?id=dados:dados-caixeta, calcule
#quantas esp√©cies est√£o presentes na coluna especie. Para ler o arquivo, execute
#o c√≥digo abaixo
caixeta = read.table("caixeta.csv", header=T, sep=",", as.is=T)
unique(caixeta$especie)
sort(unique(caixeta$especie))
caixeta$especie[caixeta$especie == "Callophyllum brasiliensis"] = "Calophyllum brasiliensis"
sort(unique(caixeta$especie))
#existem 42 espÈcies diferentes

# o arquivo deve estar no seu diret√≥rio de trabalho
str(caixeta) #o que este comando faz? mostra a estrutura interna de um objeto
summary(caixeta) #mostra o resumo estatÌstico dos meus dados. sÛ que faz os c·lculos de tudo junto, n„o serve. terei que indexar a vari·vel que eu quero

#podemos criar uma coluna a mais no nosso dataframe apenas dizendo ao R o que
#colocar l√°. Por exemplo, criamos uma coluna com o nome do coletor do dado
caixeta$coletor= "Darwin" #regra da reciclagem
str(caixeta) 

#agora adicione no objeto caixeta uma coluna chamada "desvio" com o quanto a
#altura de cada √°rvore difere da m√©dia de todas as √°rvores
mediaTotal = mean(caixeta$h)
desvio = caixeta$h - mediaTotal
caixeta$desvio = desvio
#forma simplificada: caixeta$desvio = caixeta$h - mean(caixeta$h)

#Usando a fun√ß√£o table(), conte quantos indiv√≠duos de cada esp√©cie existem
table(caixeta$especie)

#A fun√ß√£o table() tamb√©m pode contar por esp√©cie em cada local, depois por local
#em cada esp√©cie). Como?
table(caixeta$especie, caixeta$local)
#tudo que tiver dentro do parÍntese, ele vai entender que È um vetor que vai jogar pra dentro de um objeto
#crie um objeto chamado pequenas que contenha todas as √°rvores com altura (h)
#menor que um metro.(Os dados no arquivo est√£o em cent√≠metros)
pequenas = caixeta[caixeta$h < 100, ]
#crie um objeto chamado grandes que contenha todas as √°rvores acima de 3 metros
grandes = caixeta[caixeta$h > 300, ]
#crie um objeto chamado medias que todas as √°rvores com alturas entre um metro e
#tres metros
medias = caixeta[caixeta$h >= 100 & caixeta$h <= 300,]

#agora em uma linha de comando, crie um objeto que contenha a quantidade de
#√°rvores pequenas, m√©dias e grandes, separadamente
todas = c(length(pequenas), length(media), length(grandes))
#mostra um comprimento de um vetor de data.frame ou matriz (ambos s„o conjuntos de vetores), se eu n„o especificar ele ir· me mostrar o n˙mero de vetores dentro do data.frame ou matriz
todas = c(length(pequenas$local), length(medias$local), length(grandes$local))
#Crie um objeto chamado obj somente com os individuos de "Alchornea
#triplinervia" Em quantas localidades esta esp√©cie ocorre? Qual comando para
#encontrar essa informa√ß√£o?
obj = caixeta[caixeta$especie == "Alchornea triplinervia",]

#podemos remover dados de um dataframe, usando o sinal de menos
###### which() d· os endereÁos de tudo que coumprir a condiÁ„o que tem depois dele
caixeta.sem.tabebuia = caixeta[ - which(caixeta$especie == "Tabebuia cassinoides"),] #ele tirou tudo que tem essa espÈcie do data.frame
unique(caixeta.sem.tabebuia$especie) # cita as coisas dentro do vetor
sort(unique(caixeta.sem.tabebuia$especie)) # cite em ordem alfabÈtica

#agora crie um objeto removendo todas as "Tabebuia cassinoides" do local retiro
#(somente do local retiro)
#crio um vetor "caixeta.sem.retiro" e mostro uma condiÁ„o criando um vetor (caixeta$local == "retiro" & caixeta$especie == "Tabebuia cassinoides"), mas coloco o !. esse sinal vai criar um objeto que N√O ser essa condiÁ„o que eu especifiquei
caixeta.sem.retiro = caixeta[ #indexo a condiÁ„o dentro de caixeta
                            !( caixeta$local == "retiro" 
                              &
                              caixeta$especie == "Tabebuia cassinoides"),
                            ]

#crie os objetos abaixo, junte eles num dataframe de duas colunas chamado comNA,
#depois crie um novo objeto chamado semNA, removendo todas as linhas que
#contenham NA, usando a nota√ß√£o acima (dica: s√≥ se usa "" para valores do tipo
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
semNA = comNA[!is.na(comNA$c.a..b..c..d.),] #is.na busca todos os NA da coluna "c.a..b..c..d." do dataframe comNA, o ! me d· tudo oq N√O for a condiÁ„o que eu indiquei
semNA2 = na.omit(comNA) #faz a mesma coisa do de cima

#Abaixo temos dados de tamanho de mand√≠bula de chacal dourado. Crie um dataframe
#com esses dados. Qual a m√©dia por sexo? E para a esp√©cie? Guarde estes valores
#em tres objetos diferentes.
#machos: 120 107 110 116 114 111 113 117 114 112
#femeas: 110 111 107 108 110 105 107 106 111 111 Quantos machos t√™m a mand√≠bula
#maior do que a m√©dia das f√™meas?
machos= c(120,107,110,116,114,111,113,117,114,112)
machas= c(110,111,107,108,110,105,107,106,111,111)
media.machos = mean(machos)
media.machas = mean(machas)
media.pop = mean(c(machos,machas))
machos.grandes = machos[machos > media.machas] #usando os objetos separados
tabela = data.frame(machos,machas)
machos.grandes2 = tabela$machos[machos > media.machas] #usando dataframe

#escreva **uma** linha de c√≥digo que testa se um n√∫mero √© par, e caso seja,
#manda uma mensagem dizendo "par". dica: use a fun√ß√£o %% (resto da divis√£o, ou
#remainder em ingl√™s) e a fun√ß√£o message()

###if
teste.de.normalidade = shapiro.test(dragoes$y)
if (teste.de.normalidade$p.value >= 0.05) { #o if olha se a condiÁ„o dentro do () È verdadeira,
  #se for verdadeira, ele executa oq tiver entro {}
  fit = lm(dragoes$y ~ dragoes$x)
  summary(fit)
}

#agora crie um c√≥digo  como acima, mas que diga "par" se for par e "impar" se
#for impar. Dica: leia sobre as fun√ß√µes else e ifelse
if(8 %% 2 == 0){message("par")}
if(9 %% 2 != 0){message("Ìmpar")}

if(8 %% 2 == 0) {
  message("par")
} else {                    
  message ("Ìmpar")
}

if(dado seguir uma normal)
{teste paramÈtrico}
else {teste n„o paramÈtrico} #se o dado seguir uma normal, realiza teste paramÈtrico, 
                            #se n„o for, realiza o n„o paramÈtrico
ifelse #quando for pouca coisa

#crie um ciclo (for) que mostre o console todos os inteiros de 1 at√© 10, um por
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


#crie um ciclo (for) que some todos os inteiros de 25 at√© 43, guardando esses
#valores num objeto soma (dica: crie o objeto soma com valor zero antes do
#c√≥digo do ciclo)

#####Desafio level Hard#####
#Escreva um c√≥digo que escreva os n√∫meros de um a 100, com as seguintes
#condi√ß√µes: Se o n√∫mero for multiplo de 3, ao inv√©s do d√≠gito, escreva "Fu". Se
#o n√∫mero for m√∫ltiplo de 5, ao inv√©s do d√≠gito, escreva "b√°". Para n√∫meros que
#forem multiplos tanto de 3 quanto de 5, escreva "Fub√°". A melhor solu√ß√£o ganha
#um chocolate
