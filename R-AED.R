#Grau de Instrução: 1- Ensino fundamental, 2- Ensino médio e 3- Superior;
#Região de procedência: 1 Interior, 2 Capital  e 3 Outra;
#limpeza
rm(list=ls(all=TRUE))
#Lendo os dados
exemplo=read.table("C:/Users/Público/downloads/Exemplo.txt", dec=".", h=T)
exemplo
attach(exemplo)
#A planilha digitada como está ainda não está pronta.
#Precisamos informar para o programa que as variáveis
#civil, instrucao e regiao, NÃO são numéricas e sim categóricas.
civil = factor(civil, label = c("solteiro", "casado"), levels = 1:2)
civil
instrucao = factor(instrucao, label = c("EnsinoFundamental", "EnsinoMedio", "Superior"), lev = 1:3, ord = T)
instrucao
regiao = factor(regiao, label = c("capital", "interior", "outra"), lev = c(2, 1, 3))
regiao
#Estes são dados no "estilo planilha", com variáveis de diferentes tipos:
#categóricas e numéricas (qualitativas e quantitativas).
#Portanto o formato ideal de armazenamento destes dados no R é o data.frame.
exemplo=data.frame(exemplo)
is.data.frame(exemplo)
names(exemplo)
dim(exemplo)
#Variável Qualitativa Nominal:
#A variável civil é uma qualitativa nominal.
#Desta forma podemos obter: uma tabela de frequências (absolutas e/ou relativas),
#um gráfico de setores, a medida Moda
#A seguir obtemos frequências absolutas e relativas
civil.tb = table(civil)
civil.tb
civil.tbr=prop.table(civil.tb)
civil.tbr
#Gráfico de setores
pie(table(civil))
title(sub="Figura 1: Gráfico de setores para a variável Estado Civil")
#A moda para esta variável
civil.mo = names(civil.tb)[which.max(civil.tb)]
civil.mo
#Variável Qualitativa Ordinal: Instrução
instrucao
is.factor(instrucao)
#As tabelas de frequências são obtidas de forma semelhante à mostrada anteriormente.
instrucao.tb = table(instrucao)
instrucao.tb
instrucao.tbr =  prop.table(instrucao.tb)
instrucao.tbr
# Gráfico de barras
barplot(instrucao.tb)
title(sub="Figura 2: Gráfico de barras para variável Instrução.")
#Cálculo da moda
instrucao.mo = names(instrucao.tb)[which.max(instrucao.tb)]
instrucao.mo
#Variável quantitativa discreta: variável filhos (número de filhos)
# Note que esta deve ser uma variável numérica, e não um fator.
#Frequências absolutas e relativas são obtidas como anteriormente.
filhos.tb = table(filhos)
filhos.tb
filhos.tbr = prop.table(filhos.tb)
filhos.tbr
#O gráfico adequado para frequências absolutas de uma variável discreta é mostrado a seguir.
plot(filhos.tb)
title(sub="Figura 3: Gráfico de frequências absolutas para variável Filhos.")
plot(filhos.tb, xlab="Filhos", ylab="FA")
title(sub="Figura 3: Gráfico de frequências absolutas para variável Filhos.")
#Sendo a variável numérica há uma maior diversidade de medidas Estatísticas que podem ser calculadas.
#como obter algumas medidas de posição: moda, mediana, média.
#Note que o argumento na.rm=T é necessário porque não há informação sobre número de filhos para alguns indivíduos.
filhos.mo = names(filhos.tb)[which.max(filhos.tb)]
filhos.mo
filhos.md = median(filhos, na.rm = T)
filhos.md
filhos.me = mean(filhos, na.rm = T)
filhos.me
#Agora para medidas de dispersão: variância, desvio padrão, coeficiente de variação.
filhos.var=var(filhos, na.rm = T)
filhos.var
filhos.dp = sd(filhos, na.rm = T)
filhos.dp
filhos.cv = 100 * filhos.dp/filhos.me
filhos.cv
#Note que há comandos para se obter várias medidas de uma sá vez.
summary(filhos)
#Variável quantitativa Contínua: Salário.
#Dois possíveis gráficos para variáveis contínuas: histograma e box-plot.
#Note que, há várias outras opções fornecidas pelos argumentos das funções, por exemplo, 'hist'.
hist(salario, main = "Figura 5: Histograma variável Salário")
#outra
hist(salario, main = " ", xlab= "Salário", ylab= "Frequência")
title(sub="Figura 5: Histograma para a variável Salário.")
boxplot(salario)
title(sub="Figura 6: Boxplot para a variável Salário.")
#colocando um gráfico ao lado do outro
par(mfrow=c(1,2))
hist(salario, main = " ", xlab= "Salário", ylab= "Frequência")
title(sub="Figura 7a: Histograma para a variável salário.")
boxplot(salario)
title(sub="Figura 7b: Boxplot para a variável Salário.")
#Medidas obtidas da mesma forma que para variáveis discretas. Veja alguns exemplos a seguir.
salario.md = median(salario, na.rm = T)
salario.md
salario.me = mean(salario, na.rm = T)
salario.me
salario.var=var(salario, na.rm = T)
salario.var
salario.dp = sd(salario, na.rm = T)
salario.dp
salario.cv = 100 * salario.dp/salario.me
salario.cv
summary(salario)
#######################################
# Análise Bivariada
#######################################
#Na análise bivariada procuramos identificar relações entre duas variáveis.
#Assim como na univariada estas relações podem ser resumidas por gráficos, tabelas e/ou medidas Estatística.
#O tipo de resumo vai depender dos tipos das variáveis envolvidas.
#Qualitativa vs Qualitativa: as variáveis civil (estado civil) e instrucao (grau de instrução).
#A tabela envolvendo duas variáveis é chamada tabela de cruzamento ou tabela de contingência e
#pode ser apresentada de várias formas.
# A forma mais adequada de apresentação vai depender dos objetivos da análise e da interpretação desejada para os dados.
#Iniciamente obtemos com table() a tabela de frequências absolutas.
#A tabela incluindo os totais marginais pode ser obtida com addmargins().
civ.gi.tb = table(civil, instrucao)
civ.gi.tb
addmargins(civ.gi.tb)
#Tabelas de frequências relativas são obtidas com prop.table()
prop.table(civ.gi.tb)
#Gráfico de barras
barplot(civ.gi.tb, legend = T)
#Qualitativa vs Quantitativa Para exemplificar este caso vamos considerar as variáveis instrucao e salario.
# Gráfico BoxPlot
boxplot(salario ~ instrucao)