#Ctrl+Enter roda o código
#Evitar salvar a sessão - mudar na configuração
#Para o R, qualquer variável é um vetor (de tamanho 1 ou mais)
x = 1
x

#criando vetor:
x = c(3,5)
#Ele aplica operações vetorizadas (para cada elemento)
2*x

x ^ 2

#Criando range:
x = 1:5
y = 11:15

(x + y) * 2

x = 1:10 +0.5

#string
x = "kleber"

#Bool
x = c('str1','str2','str3')
x= T
y = F

#OBS: um vetor, diferente de uma lista, só tem um tipo de dado
#vetor
x = c(1, "oi") #converte tudo prum unico tipo
y = list(1, "oi") #lida com diferentes tipos de dados.
z = list(x, y, 3)


#########################
#Funções

dados = 1:10
N = 10

#Usando a função sum() para somar elementos de um vetor

sum(dados) / N

#Outras funções
media = mean(dados)
desvio = sd(dados)
coefvar = desvio / media # coeficiente de variação
round(coefvar, digits = 2)

median(dados)

#Criando nossas próprias funções






###################
divisivel = function(dividendo, divisor) {
 if (dividendo %% divisor == 0) {
   return (T)
 } 
  else {
   return (F)
 }
}

e_primo = function (numero) {
  if (numero == 2) {
    return (T)
  }
  numeros_pra_testar = 2:(numero-1)
  for (divisor in numeros_pra_testar) {
    if (divisivel(numero, divisor)) {
      return (F)
    }
  }
  return (T)
}


encontrar_primos = function (ate_quanto) {
  numeros_para_testar = 1:ate_quanto
  for (n in 1:ate_quanto) {
    if (e_primo(n)) {
      print(n)
    }
 }
}

encontrar_primos(13)

################

#Bhaskara
# (-b +- raiz de delta) / 2a
# delta = b² - 4*a*c 

calc_delta = function (coefa,coefb,coefc) {
  return (coefb^2 - 4*coefa*coefc)
}

bhaskara = function (coefa,coefb,coefc) {
 delta = calc_delta(coefa,coefb,coefc) 
 if (delta < 0) {
   return ("Sem raízes reais")
 }
 if (delta == 0) {
   return (-coefb / 2*coefa)
 } 
 else {
   return (c(-coefb + delta^0.5 / 2*coefa, -coefb - delta^0.5 / 2*coefa))
 }
}


bhaskara(1,3,-1)
