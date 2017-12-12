######################################################

# Introdução Linguagem R

# Vitor Passos Rios

# Universidade Federal da Bahia

# https://github.com/vrios/Intro-Linguagem-R/wiki

#####################################################



#####Usando o R como calculadora#####



#Digite no R os códigos abaixo.

#Dica: escreva neste arquivo, ao lado do comando, o que ele faz,

#como no exemplo abaixo



1 + 1 #soma os valores. soma igual a 2



# O símbolo # representa um comentário, isto é, tudo que vier à direita

# dele será ignorado quando o R ler o arquivo



34 - 72.3 #irá subtrair estes valores


# use pontos como separador de decimal. O que acontece se você usar virgula?
# ele não reconhece virgula como separador decimal

78 / 3 # ira dividir 78 por 3

2 ^ 3 # aqui o numero 2 esta na terceira potencia

3 * 2 # ira multiplicar o 3 por 2

sqrt(2) # aparece a raiz quadrada de 2

pi # o valor de pi aparece

log(x = 3,base = 10) #aparece o valor de x que e 3, na base 10

exp(x = 1) # exponencial do numero 1

#o que querem dizer estes valores dentro do parêntese? porque usamos o = aqui?
# os valores dentro de parenteses se referem a funcoes, o = e utilizado para indicar o valor de x



#####Atribuição de valores e criação de objetos#####

#O que os comandos abaixo fazem?

objeto1=3 #criasse um valor para o objeto, que nesse caso é 3

objeto1 #aparece o valor numérico atribuido ao "objeto1"

objeto = 42 #criasse um novo valor para um novo objeto

objeto #aparece o valor numérico ao "objeto"

objeto.2 <- 42 #criasse um objeto e atribui o valor 42 a ele

objeto.2 #aparace o valor de "objeto.2"

# Os símbolos = e <- fazem basicamente a mesma coisa. Que coisa?
# incorpora valores aos objetos

objeto.cubico <- objeto^3 #criasse um novo objeto,que é o "objeto.cubico" e atribui valor a ele, que nesse caso é o valor de "objeto" elevado a 3

resultado.1 <- (objeto.cubico / 3) + 7 #criasse um novo objeto o "resultado.1", onde o valor de "resultado.1" é o valor de objeto.cubico dividido por 3 e somado a 7

objeto.texto <- "texto sempre vem entre aspas" #o objeto texto foi criado, e nesse caso o valor dele é um texto, logo objeto.texto é uma frase

objeto.texto #aparece o texto que está dentro da caixa "objeto.texto"

objeto.texto.2 <- "42" #criasse um objeto novo, e o valor 42 vira um texto dentro desse objeto



objeto.vetor.1 = 1:34 # o objeto "objeto.vetor.1 foi criado e seu valor é uma sequência de números de 1 até 34

objeto.vetor.1 #apareceu a sequência dos números

objeto.vetor2 = c(1, 74.5, 48) #criou um objeto novo, onde seu valor é uma combinação de 3 números

objeto.vetor2 # apareceu a sequência dos números



?seq #apareceu um help tratando sobre as sequências numéricas

objeto.vetor3 = seq(from = 20, to = 32, by = 0.5) #o objeto criado é uma sequência de números de 20 até 32, onde pula de 0.5 em 0.5 entre eles



#podemos calcular estatísticas com o r

mean(objeto.vetor3) #media #aparece a média do objetor.vetor3

var(objeto.vetor3) #variancia #aparece a variancia do objeto.vetor3

median(objeto.vetor3) #mediana #aparece a mediana do objeto.vetor3

min(objeto.vetor3) #aparece o valor mínimo do objeto

max(objeto.vetor3) #aparece o valor máximo dentro do objeto

diff(objeto.vetor3) #calcula a diferença entre os elementos sucessivos do vetor



# R também faz comparações entre objetos

42 > 7

objeto == objeto.2

objeto == objeto.texto.2

# o que aconteceu acima? #apareceu TRUE, significa que os objetos são iguais. Ou seja, todos os objetos possuem o mesmo valor

7.1 <= 7.001 #aparece FALSE, ou seja esses valores não são iguais

?"<" #aparece um help sobre comparações entre valores



#pedindo ajuda

?pi

?log

??lm

help(log)

help.search("anova")



#####Arquivos e diretórios#####

# Como você descobre o diretório de trabalho?

getwd()

# Como você lista o conteúdo do diretório de trabalho?

ls()

# Como você define o diretório de trabalho?

setwd()

# Como você carrega um arquivo de script?

library()


# Como você salva os objetos que criou?
save.image()


# Como você carrega os objetos que criou?
load()


# Como ver quis objetos estão na sua área de trabalho?

ls()

# Como remover objetos da área de trabalho?
rm()

##### Lidando com erros#####

# O que acontece quando voce digita os comandos abaixo? Como consertar cada erro?

objetol #nao existe esse objeto

objeto .texto #existe espa?o entre os nomes, ele nao entende

Objeto

source("chuchu.R") #nao esta em meu diretorio

source(chuchu.R) #nao ? um objeto

setwd("C:/CavernaDoDragão") #n?o tem essa pasta

getwd #nao tem parentese

Getwd() #esta maiusculo, ele nao entende

#dica: quando o R der erro, copie e cole a mensagem de erro no google