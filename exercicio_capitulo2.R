# 2.1
variedade <- c(
  'ipê amarelo', 'ipê amarelo', 'ipê amarelo', 
  'ipê branco', 'ipê branco', 'ipê branco', 
  'jacarandá mimoso', 'jacarandá mimoso', 'jacarandá mimoso', 
  'manacá', 'manacá', 'manacá'
)
# idades 1 a 14
# cada variedade possui uma arvore em cada grupo de idade

arvores <- data.frame(sp=c(rep("ipa",14),rep("ipb",14),rep("jac",14),rep("man",14)),idade=rep(1:14,4))
# A: A árvore tem idade igual a 1 ano;
# B: A árvore é um ipê amarelo;
# C; A árvore é um ipê qualquer de idade avançada (>=12 anos);
# D: A árvore tem idade intermediária (4 anos <= idade < 12 anos);
# E: A árvore é um ipê

arvores[c(1:3),]

linhas <- 1:nrow(arvores)

# a.
# Propriedade 1: A+A=A e AA = A com A(idade=1).
A <- linhas[arvores$idade==1]
A
# Na qual A contém as posições onde a idade é 1 

# A+A=A (união)
c(A,A[A%in%A==FALSE])

# AA=A (interseção)
A[A%in%A==TRUE]

# Propriedade 2: A+B=B+A e AB=BA
B <- linhas[arvores$sp =='ipa']

# A+B=B+A
AmaisB <- c(A,B[B%in%A==FALSE])
BmaisA <- c(B,A[A%in%B==FALSE])

AmaisB
BmaisA

# AB=BA
AB <- A[A%in%B]
BA <- B[B%in%A]
AB
BA

# Propriedade 4: (A+B)+C=A+(B+C) e (AB)C=A(BC)
C <- linhas[(arvores$sp=='ipa'|arvores$sp=='ipb')&arvores$idade>=12]
C

# (A+B)+C=A+(B+C)
AmaisBmaisC <- c(AmaisB,C[C%in%AmaisB==FALSE])

BmaisC <- c(B,C[C%in%B==FALSE])
c(A,BmaisC[BmaisC%in%A==FALSE])
AmaisBmaisC

# (AB)C=A(BC) com E no lugar do C
E <- linhas[(arvores$sp=='ipa'|arvores$sp=='ipb')]
ABE <- AB[AB%in%E]

BE <- B[B%in%E]
A[A%in%BE]
ABE

# Propriedade 5: A+BC=(A+B)(A+C)
BC <- B[B%in%C]
AmaisBC <- c(A,BC[BC%in%A==FALSE])
AmaisC <- c(A,C[C%in%A==FALSE])
AmaisB[AmaisB%in%AmaisC]
AmaisBC

# Propriedade 6: A'B'=(A+B)'
Alinha <- linhas[linhas%in%A==FALSE]
Blinha <- linhas[linhas%in%B==FALSE]
Alinha[Alinha%in%Blinha]

linhas[linhas%in%AmaisB==FALSE]

# Propriedade 7: A'+B'=(AB)'
c(Alinha,Blinha[Blinha%in%Alinha==FALSE])

linhas[linhas%in%AB==FALSE]

# Propriedade 8: Se A->B então AB=A e A+B=B
B[B%in%E==TRUE]
B

# Propriedade 9: Se A->B e B->A, então A=B


D <- linhas[arvores$idade>=4&arvores$idade<12]

A%in%C%in%D


uniao <- function(A, B) {
  return(c(A,B[B%in%A==FALSE]))
}

produto <- function(A, B) {
  return(A[A%in%B])
}

negacao <- function(linhas, A) {
  return(linhas[linhas%in%A==FALSE])
}

# b.
# F=(A+C+D)'
AmaisCmaisD <- uniao(AmaisC, D)
AmaisCmaisD

F <- negacao(linhas, AmaisCmaisD)

Clinha <- negacao(linhas, C)
Dlinha <- negacao(linhas, D)

AlinhaClinha <- produto(Alinha,Clinha)
produto(AlinhaClinha,Dlinha)

# 2.2
# Garantindo que cada uma das árvores tem a mesma probabilidade de ser extraída,
# considerando as condições usadas no exercício anterior
# a.
pa <- length(A)/length(linhas)
pa

pb <- length(B)/length(linhas)
pb

pc <- length(C)/length(linhas)
pc

pd <- length(D)/length(linhas)
pd

# b.
# Cálculo de P(B|C) corresponde a ideia de se calcular a probabilidade
# de B dado que C é fato. Pela lei do produto P(BC)=P(C)P(B|C). 
# Portanto P(B|C)=P(BC)/P(C)
pbc <- length(BC)/length(linhas)
pbc

pbc/pc
# Dado que a probabilidade muda após o acontecimento de C,
# logo os eventos são dependentes.

# c.
# Cálculo da probabilidade de P(A+D)=P(A)+P(D)
pa+pd

# 2.3
# P(E1+E2|H)=P(E1|H)+P(E2|H)-P(E1E2|H)

# 2.4
# Dado que P(E1E2|H)=0, então P((E1E2)'|H)=1=P(E1'+E2'|H)
# P(E1'+E2'|H)=P((E1E2)'|H)=1-P(E1E2|H)
# então P(E1E2|H)=1-P((E1E2)'|H)=1-1=0

# 2.5
# P(B+C)=P(B)+P(C)-P(BC)
pb+pc-pbc

rm(list = ls())

# 2.6
# Probabilidade de certo filme ganhar o oscar por melhor fotografia 0.3
# Probabilidade que o filme ganhe melhor direção é de 0.20
# Probabilidade de ganhar os dois é de 0.05

# P(A)=0.3
pa <- 0.3
# P(B)=0.2
pb <- 0.2
# P(AB)=0.05
pab <- 0.05

# a. Probabilidade de ganhar pelo menos um prêmio
# Como os dois eventos não são exclusivos, deve-se usar a fórmula
# da extensão da lei de adição
# P(A+B)=P(A)+P(B)-P(AB)
pAmaisB <- pa+pb-pab

# b. Probabilidade de que não ganhe prêmio algum
# Como calculamos a chance de ganhar pelo menos um, a chance de 
# ganhar nenhum seria o negado de ganhar pelo menos um
1-pAmaisB

rm(list = ls())
# 2.7 
# Uma região está sujeita a sofrer um desastre natural, que é medido
# em uma escala discreta que vai de 1 a 6. A probabilidade de qualquer
# uma dessas 6 possibilidades é igual.
# Supondo que a região foi afetada de forma consecutiva. Faça a listagem
# de todos os 36 possíveis resultados. Seguindo o princípio da 
# permutabilidade se supõe que a probabilidade é de 1/36.
# Calcular a probabilidade de:
# a. A soma dos indíces ser menor que 8 e que o primeiro fenômeno tenha
# tido uma menor que 4
# b. Pelo menos um dos fenômenos tenha tido intensidade menor que 3

# a.
pares <- data.frame(expand.grid(c(1:6),c(1:6)))
pares
sum((pares$Var1+pares$Var2<8)&pares$Var1<4)/nrow(pares)

# b.
sum(pares$Var1<=3|pares$Var2<=3)/nrow(pares)

# 2.8
# St: A bactéria é suscetível à droga t
# com t={1,2,3}
# P(St)=0.9, a probabilidade é a mesma para cada um dos casos
# Qual a probabilidade de que a bactéria se mostre suscetível?
# Esta que se dá pela fórmula P(S1+S2+S3)
# Esta que tem sua complementar 1-P[(S1+S2+S3)'S]
# 1-[P(S1')P(S2')P(S3')]
# 1-(0.1)^3=0.999

# 2.9
# P(Si)=0.16, para n=12, logo P(Si')=0.84
# P(S1+...+Sn)=1-P[(S1+...+Sn)']
# 1-(ProdutoDeTodosOsTermos(P(Si')))=1-0.84^12=0.877

# 2.10
# A: O indivíduo é o progenitor
# B: O teste resulta em positivo
# Qual a probabilidade de que a paternidade seja verdadeira?
# P(A|B)=?
# P(B|A)=0.98, P(B|A')=0.05 e P(A)=0.15

# Aplicando probabilidade condicional
# P(A|B)=P(A)P(B|A)/P(B)
# P(B)=P(B|A)P(A)+P(B|A')P(A')
# Sendo que P(B|A)P(A) é a probabilidade de o teste dar positivo 
# quando o indivíduo é o pai
# E P(B|A')P(A') é a probabilidade de o teste dar negativo quando
# o indivíduo não é o pai
# P(A|B)=P(A)P(B|A)/P(B|A)P(A)+P(B|A')P(A')
0.15*0.98/(0.98*0.15+0.05*0.85)

# 2.11
# A: O candidato estudou o tema sorteado
# B: O candidato acertou a questão
# Tendo acertado a questão, qual a probabilidade de que ele tenha
# estudado o tema?
# P(A|B)=?
# P(B|A)=1, P(B|A')=1/m, P(A)=p
# P(A|B)=P(A)P(B|A)/P(B)=p*1/(p*1+(1-p)*1/m)

# 2.12
# A: O primeiro técnico identificou corretamente uma bactéria
# B: O segundo técnico faz uma identificação correta
# P(A)=2/3, P(B|A)=3/4, P(B|A')=1/5
# a. Qual a probabilidade de que o segundo técnico identifique 
# corretamente a bactéria? (P(B)=?)
# P(B)=P(A)P(B|A)+P(A')P(B|A')=(2/3)*(3/4)+(1/3)*(1/5)
P_B <- (2/3)*(3/4)+(1/3)*(1/5)

# b. Se ambos os técnicos forem convocados individualmente, qual a
# probabilidade de que a bactéria seja identificada por pelo menos
# um dos técnicos?
# P(A)+P(B)-P(AB)
# Aplicando lei do produto
# P(A)+P(B)-P(A)P(B|A)
(2/3)+P_B-(2/3)*(3/4)

rm(list = ls())
# 2.13
# A: Ganhar concorrência da parte elétrica
# B: Ganhar concorrência da parte hidráulica 
# P(A)=0.5, P(B|A)=0.75, P(B|A')=1/3
# a. Qual a probabilidade de ganhar os dois? (P(AB))
# P(AB)=P(A)P(B|A)
P_AB <- 0.5*0.75

# b. Ganhe apenas um dos contratos? (P(AB'+A'B))
# P(AB')+P(A'B)=P(A)P(B'|A)+P(A')P(B|A')
# P(B'|A)=1-P(B|A)=0.25
0.5*0.25+0.5*(1/3)

# c. Ganhe o contrato da parte hidráulica? (P(B))
# P(B)=P(A)P(B|A)+P(A')P(B|A')
P_B <- 0.5*0.75+0.5*(1/3)
P_B

# d. Sabendo que venceu a hidráulica, qual a probabilidade de vencer
#    a elétrica? (P(A|B))
# P(A|B)=P(A)P(B|A)/P(B)
0.5*0.75/P_B

# 2.14
# M: Freguês é mulher
# A: Freguês pede prato a base de carne
# M': Freguês é homem
# A': Freguês pede prato de salada
# Dado que P(A'|M')=0.2, P(A|M)=0.3, P(M')=0.78

# a. P(M)
# P(M)=1-P('M)
1-0.78

# b. P(A'|M')
0.2

# c. P(A||M)
0.3

# d. P(AM')
# P(AM')=P(A)P(M'|A)=P(A)(1-P(M|A))
# P(AM')=P(M')P(A|M')=P(M')(1-P(A'|M'))
P_AMlinha <- 0.78*(1-0.2)

# e. P(A+M')
# P(A+M')=P(A)+P(M')-P(AM')
# P(A)=P(M)P(A|M)+P(M')P(A|M')
# P(A+M')=P(M)P(A|M)+P(M')P(A|M')+P(M')-P(AM')
# P(A+M')=P(M)P(A|M)+P(M')(1-P(A'|M'))+P(M')-P(AM')
0.22*0.3+0.78*(1-0.2)+0.78-P_AMlinha

# f. P(M|A')
# P(M|A')=P(A'|M)P(M)/P(A')
# P(M|A')=P(A'|M)P(M)/(P(M)P(A'|M)+P(M')P(A'|M'))
# P(M|A')=(1-P(A|M))P(M)/(P(M)(1-P(A|M))+P(M')P(A'|M'))
(1-0.3)*0.22/(0.22*(1-0.3)+0.78*0.2)

# 2.15

