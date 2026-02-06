# 3.1
# Seja uma fêmea de uma espécie gerar apenas um filhote por prenhez, com a probabilidade do filhote nascer com pelagem marrom 1/6. Qual o número de nascimentos até o primeiro sucesso?

# Identifico que a distribuição adequada para este exercício seja a distribuição binomial negativa

prob <- 1/6
a <- 1

# a) Probabilidade de que sejam necessários mais de 8 nascimentos

# sendo U o número de experimentos e X o número de fracassos antes de se obter a sucessos, sabendo que U = X + a

# P(U>8) = P(X+a>8) = P(X>8-1) = P(X>7) = 1 - P(X<=7) = 1 - [P(X=0)+...P(X=7)]

X <- 0:7

1-sum(dnbinom(x = X, size = a, prob = prob))

# b) Quantos nascimentos precisam ocorrer para garantir uma probabilidade de sucesso de no mínimo 0.95

# P(U<= u*) >= 0.95
# P(u) = θ(1-θ)^(u-1) = (1/6)(1-1/6)^(u-1) = 1/5(5/6)^u

# Probabilidade de nenhum sucesso em u tentativas
# P(nenhum sucesso em u tentativas) = (5/6)^u

# Ter pelo menos um sucesso até u
# P(U<=u) = 1 -(5/6)^u

# Resolvendo
# 1 - (5/6)^u >= 0.95
# (5/6)^u <= 0.05

# Aplicando log
# u*log(5/6) <= log(0.05)
# u >= log(0.05)/log(5/6) ≃ 16,43
# Como a resposta deseja o valor mínimo seja 0.95 para satisfazer, é necessário no mínimo 17 nascimentos.

# Para facilitar o cálculo é levado multiplicado 5 dos dois lados da equação
# Assim o valor corte passa de 0,95 para 4,75
n <- 1
equacao <- (5/6)^n
while(equacao < 4.75) {
  n <- n+1
  equacao <- equacao + (5/6)^n
}

# c) Se o custo de cada filhote é 100 reais, quanto é o custo esperado antes do nascimento do filhote marrom

# Sabendo que a probabilidade de um filhote vir com pelagem marrom é 1/6 e que cada um dos filhotes custa 100 reais
# então utilizando a fórmula da média da distribuição binomial negativa E(U) = a/θ = 1/(1/6) = 6, assim em média
# são necessários 6 nascimentos até nascer um filhote marrom, então o gasto esperado é de 600 reais

# 3.2
# C1: 1 cliente: probabilidade = 1/3
# C2: 2 clientes: probabilidade = 2/3
# Probabilidade de venda
# 1 contato: probabilidade = 1/20
# 2 contatos: probabilidade = 1/10
# v: ocorre alguma venda
# Em um período de 5 dias, qual a distribuição de probabilidade de Y 
# E qual é o valor esperado para os dias que consegue fazer alguma venda

# P(C1) = 1/3
# P(C2) = 2/3
# P(v|C1) = 1/20
# P(v|C2) = 1/10

pc1 <- 1/3
pc2 <- 2/3
pv_c1 <- 1/20
pv_c2 <- 1/10

# A probabilidade que em um dia qualquer possa ocorrer uma venda é P(v) = P(C1)*P(v|C1) + P(C2)*P(v|C2)
pv <- pc1*pv_c1 + pc2*pv_c2
pv

# Tendo em vista que o fato de ocorrer ou não venda não afeta a probabilidade da ocorrência de uma 
# venda nos demais dias, então pode ser resolvido por meio de uma distribuição binomial

y <- 0:5
n <- 5
prob <- dbinom(x = y, size = 5, prob = pv)
prob

# O valor esperado se da pela fórmula E(Y) = n*θ:
n*pv
# A probabilidade esperada quanto ao número de dias em que ocorre uma venda é de 0,41

# 3.3
# P(nível do ar em certa região metropolitana ultrapasse o limite de segurança) = 0,18
# a) Probabilidade de que em um mês de 30 dias o limite de segurança seja ultrapassado em mais de 7 dias?

# O valor desejado é de P(X>7) que é igual a 1 - P(X<=7)

θ <- 0.18
n <- 30
x <- 7
1- pbinom(q = x, size = n, prob = θ)

# A probabilidade de que em 1 mês de 30 dias o limite de segurança seja ultrapassado em mais que 7 dias
# é de 0.15

# b) Em meses de 30 dias qual o número médio e o desvio padrão do número de dias em que o limite de 
# segurança é ultrapassado?

# Número médio:
n*θ

# Desvio Padrão
sqrt(n*θ*(1-θ))

# 3.4
# P(morte|infarto miocárdio) = 0.149

# a) Para cada 5000 óbitos registrados, qual a média e o desvio padrão do número de mortes que 
# podem ser atribuídos ao infarto de miocárdio?
n <- 5000
θ <- 0.149

# Média
media <- n*θ

# Desvio Padrão
dp <- sqrt(n*θ*(1-θ))

# b) Em uma cidade da região ocorreram 826 mortes por infarto de miocárdio, tendo em vista que é 
# considerado incomum valores que se afastem mais que 2 desvios padrões, essa cidade deve ser 
# considerada incomum?

mortes <- 836
(mortes-media)/dp
# Como o valor de desvios padrões é maior que 2, pode-se considerar uma cidade incomum

# c) Qual a probabilidade que P(U>836)?
# P(U>=836) = 1 - P(U<836) = 1 -P(U<=835)
1 - pbinom(q = 835, size = n, prob = θ)

# A probabilidade que no mínimo 836 mortes sejam por infarto do miocárdio é de 0,00019. Assim pode-se
# considerar que o nível de incidência de infarto do miocárdio na cidade mencionada é preocupante.

# 3.5
# Os registros de uma pequena empresa indicam que 40% das faturas emitidas são pagas após o vencimento.
# De 8 faturas expedidas, determine a probabilidade de:
# a) Nenhuma ser paga com atraso.

θ <- 0.4
dbinom(x = 0, size = 8, prob = θ)
# A probabilidade de nenhuma fatura ser paga com atraso é de 0,016

# b) No máximo duas serem pagas com atraso.

# P(X<=2)
pbinom(q = 2, size = 8, prob = θ)
# A probabilidade de no máximo duas faturas é de 0.31

# c) Mais da metade ser paga até o vencimento
# P(X<4) = P(X<=3)
pbinom(q = 3, size = 8, prob = θ)

# A probabilidade de mais da metade ser paga até o vencimento é de 0,59

# 3.6
# Em um voo, 5% das pessoas que compram bilhetes não comparecem, assim se decide vender 84 bilhetes para
# um voo que só acomoda 80 passageiros.
# a) Qual a probabilidade de que todos os passageiros comparecerem no embarque consigam lugar no voo
θ <- 0.05
n <- 84
# Como desejamos saber a probabilidade de que todos os passageiros comparecam, então seria: P(X<4) = P(X<=3)
pbinom(q = 3, size = n, prob = θ)

# R: A probabilidade é de 0,38

# b) Qual o número médio que não comparecem
n*θ

# O número médio de passageiros que não comparecem é de 4.2

# c) Quantos bilhetes em excesso a companhia pode vender no máximo se é desejado que a probabilidade de 
# que haja lugar para todos os que comparecem para o embarque seja superior a 0.8?

# O valor desejado é P(X>=n-80) > 0.8
# Aplicando a regra do complementar
# 1 - P(X<n-80) > 0.8 = 1 - P(X<=n-81) > 0.8

n <- 81
prob <- 1-pbinom(q = n-81, size = n,prob = θ)

while(prob>0.8) {
  n <- n+1
  prob <- 1-pbinom(q = n-81, n, prob = θ)
}
print(n)
print(prob)

# Tendo em vista que para conseguir 83 assentos é 0,79 então para garantir os 0,8 desejado, é necessário
# vender somente 82 assentos

# 3.7
# Dentro dos casos em que fumantes apresentam câncer, 0.3 é atribuido ao fumo. Em um grupo de 6 fumantes com
# câncer, qual a probabilidade de que mais da metade seja adquirido pelo hábito do fumo

# P(X>3) = 1 - P(X<=3)
n <- 6
θ <- 0.3
1-pbinom(q = 3, size = n, prob = θ)

# A probabilidade que mais da metade dos câncer dos fumantes, deste grupo de 6 pessoas, seja devido ao
# hábito de fumo é de 0,07

# 3.8
# Durante um surto de gripe, 14 dos 38 mamíferos de um grupo foram infectados. Ao selecionar ao acaso 6 sendo
# Y: o número de animais infectados na amostra:
# a) Qual a probabilidade de que Y seja igual a 3?

# Aplicando conceitos da distribuição hipergeométrica
M <- 14
N <- 38
n <- 6

dhyper(x = 3,m = M, n = N-M, k = n)

# A probabilidade de que Y seja igual a 3 é de 0,26

# b) Qual é o número esperado de animais infectados na amostra selecionada e qual é o devio padrão para amostras
# com n= 6?

# Média
# E(X) = n*M/N
n*M/N

# Desvio Padrão
# V(X) = n*(M/N)*(1-(M/N))*(1-(n-1)/(N-1))
sqrt(n*(M/N)*(1-(M/N))*(1-(n-1)/(N-1)))

# A média de Y com 6 amostras é 2,21 e o desvio padrão é 1,09


# 3.9
# De um total de 85 golfinhos, 35 apresentam marcas permanentes. Numa saída de barco, uma pesquisadora fotografa 
# ao acaso as nadadeiras dorsais 10 vezes. Sabendo que as fotos podem ser do mesmo golfinho.
M <- 35
N <- 85
n <- 10

# Como é citado que se trata de uma amostragem com reposição, será utilizado a distribuição binomial, e não há o θ,
# então é necessário calcular: θ = M/N
θ <- M/N

# a) Qual a probabilidade de que mais da metade das fotografias permitam a identificação?
# P(X>5) = 1 - P(X<=5)
1-pbinom(q = 5, size = n, prob = θ)

# A probabilidade de que mais da metade das fotografias permitam a identificação é de 0,18

# b) Qual é a média e o desvio padrão para o número de foto-identificações?
# Média
n*θ
# A média é de 4,11

# Desvio Padrão
sqrt(n*θ*(1-θ))
# O desvio padrão é 1,55

# c) Recalcule tendo em vista que não há reposição.
1-phyper(q = 5, m = M, n = N-M, k = n)

# Média
n*M/N

# Desvio Padrão
sqrt(n*(M/N)*(1-(M/N))*(1-(n-1)/(N-1)))

# 3.10
# 0.4 dos peixes são grandes e tem mais de 2 quilos. Cada peixe grande capturado custa ao
# pescador 6 reais. Os demais custam 3 reais a unidade. Um pescador decide que encerrará
# a pescaria no momento que capturar o segundo peixe grande. Sabendo que todos os peixes 
# tem a mesma probabilidade de serem fisgados e sendo Y: número de peixes capturados na 
# pescaria.
# a) Calcule a esperança, variância e o desvio padrão para Y:
θ <- 0.4
a <- 2
cg <- 6
cp <- 3
# Utilizando a distribuição binomial negativa, podemos assumir a captura de um peixe grande
# como sucesso e a captura de um peixe pequeno um fracasso.

# Média (utilizando essa fórmula, devido a ser a média do número total, não somente o de falhas)
a/θ

variancia <- a*(1-θ)/θ^2
variancia

sqrt(variancia)

a*(1-θ)/θ
# A média da captura de peixes antes de capturar 2 peixes grandes é de 5, a variância é
# de 7.5 e o desvio padrão é 2.73 e a média de fracassos é de 3
# b) Qual é o custa da pescaria no cenário médio 
# Tendo em vista que esse cenário médio seria a captura de 3 pequenos e 2 grandes, então
# o custo médio seria 3*3+2*6=21 reais.
3*3+2*6

# 3.11
# Pisos laminados apresentam em média 0,005 falhas por m². Se este piso será colocado 
# numa área de 212 m², qual a probabilidade de que sejam encontradas 3 ou mais falhas?
# P(X>=3) = 1 - P(X<3) = 1 - P(X<=2)
λ <- 0.005
k <- 212
1-ppois(q = 2, lambda = λ*k)

# A probabilidade de que seja encontrada 3 ou mais falhas numa área de 212m² é de 0,09.

# 3.12
# O número de televisores/dia vendidos tem distribuição com média 1.5
# a) Determine a probabilidade de vender ao menos 4 televisores num período de 2 dias.
λ <- 1.5
# P(X>=4) = 1 - P(X<4) = 1 - P(X<=3)
1-ppois(q = 3, 1.5)

# b) Quantos televisores a firma precisaria manter em estoque no início de ma semana útil
# (6 dias), para que a probabilidade de que não falte mercadoria seja maior que 0.95?

# X: número de clientes que desejam levar um aparelho de televisão
# k: quantidade a ser mantida no estoque a cada início de semana
# P(X<=k) > 0.95
qpois(p = 0.95, lambda = λ)
# Para a probabilidade de que não falte mercadoria seja maior que 0.95, deverá ser mantido
# 4 televisores.

# 3.13
# Para avistar baleias minke, um pesquisador em um navio, navega a uma velocidade constante de 10 
# nós. Estudos indicam que nesta velocidade a taxa de encontro é de 1.8 indivíduos por hora
# a) Qual a probabilidade de que em 20 minutos seja avisada ao menos uma baleia

1.8 * 60
x   * 20
20*1.8/60

λ <- 1.8
# P(X>=1) = 1 - P(X<1) = 1 - P(X<=0) = 1 - P(X=0)
k <- 20/60
1-dpois(x = 0, lambda = λ*k)

# A probabilidade de avistar ao menos uma baleia em 20 minutos é de 0.45

# b) Qual é o número esperado de avistagens num período de 75 minutos de navegação?
k <- 75/60
k*λ
# O número esperado de avistagens em 75 minutos é de 2,25

# c) Qual deveria ser o tempo mínimo de navegação para que a probabilidade de avistar ao menos
# uma baleia minke seja superior a 0.8?

# P(X>=1) = 1 - P(X<1) = 1 - P(X<=0) = 1 - P(X=0) > 0,8
# Assim aplicando a fórmula de p(x) da distribuição de poisson
# 1 - e^(-u*k)*(u*k)^x/(x!) = 1 - e^(-u*k)*(u*k)^0/(0!) > 0.8
# e^(-u*k) < 0.2
# -u*k < log_e(0.2)
# k > -log_e(0.2)/u

-log(0.2)/λ*60

# O tempo mínimo de navegação para que a probabilidade de avistar ao menos uma baleia minki seja
#  maior que 0.8 é de 53 minutos.

# 3.14
# O número de navios que chegam a uma portuária em cada dia segue uma distribuição de Poisson com
# média de 6 navios/dia. As atuais instalações de equipamento e pessoal podem atender a no máximo 
# a 8 navios/dia. Se mais de 8 navios aportarem num dia o excesso não é inspecionado.
# a) Qual a probabilidade de que num dia qualquer navios atraquem e não sejam inspecionados?
λ <- 6
# P(X>8) = 1 - P(X<=8)
1-ppois(q = 8, lambda = λ)

# A probabilidade de que num dia qualquer navios atraquem e não sejam inspecionados é de 0,15

# b) Para garantir que todos os navios sejam inspecionados em pelo menos 98% dos dias, qual deve
# ser a capacidade diária de inspeção
qpois(p = 0.98, lambda = λ)

# Para garantir que em 98% dos dias todos os navios sejam inspecionados, então deveria se atender
# 12 navios por dia

# 3.15
# A cópula para machos de uma espécie de inseto depende do encontro de fêmeas receptivas. A 
# probabilidade de que uma fêmea encontrada seja receptiva é de 0,3. Um macho é capaz de copular
# com várias fêmeas diferentes em um único dia.
# a) Qual é a probabilidade de que um macho precise encontrar no máximo 8 fêmeas para que ocorra
# 3 cópulas?
θ <- 0.3
a <- 3
# P(X<=8-a) = P(X<=5)
dbinom(x = 5, size = 8, prob = θ)

# b) Qual o número desperado de fêmeas a serem encontradas para que ocorra 3 emparceiramentos?
a/θ

# O Número esperado de fêmeas a serem encontradas para que ocorra 3 emparceiramentos é de 10