eleiçao = read.csv('C:/Users/Intel/Desktop/Formação Cientista De Dados/Dados/Eleicao.csv', header = T, sep = ';')
eleiçao

plot(eleiçao$DESPESAS, eleiçao$SITUACAO)

cor(eleiçao$DESPESAS, eleiçao$SITUACAO)

modelo = glm(SITUACAO ~ DESPESAS, data = eleiçao, family='binomial')
summary(modelo)

plot(eleiçao$DESPESAS, eleiçao$SITUACAO, col= 'red', pch = 20)
points(eleiçao$DESPESAS, modelo$fitted, pch = 0.3)

prever = predict(modelo, newdata = eleiçao, type = 'response')
prever <- prever >= 0.5
prever

confusao <- table(prever, eleiçao$SITUACAO)
confusao
taxaacerto <- (confusao[1] + confusao[4]) / sum(confusao)
taxaacerto

eleiçao2 <- read.csv('C:/Users/Intel/Desktop/Formação Cientista De Dados/Dados/NovosCandidatos.csv', sep = ';')
eleiçao2$RESULT = predict(modelo, newdata = eleiçao2, type = 'response')
eleiçao2$RESULT
eleiçao2$RESULT >= 0.5

library(stats)
help(stats)


