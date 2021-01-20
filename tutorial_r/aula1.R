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



################################
# Exercícios
#############################

# 1- Escrever função que receba um vetor de números e retorne a soma dos números no vetor (sem usar a função sum):

calcular_soma <- function(vector) {
  soma <- 0
  for (num in vector) {
    soma <- soma + num
  }
  return (soma)
}

calcular_soma(c(1,4,6,9))
calcular_soma(1:5)


# 2- Função que diz se um número é par ou ímpar:

e_par_ou_impar_um <- function (num) {
 if (num == 0) { return ("zero") }
 if (num %% 2 == 0) { return ("par") }
 if ((num %% 2) != 0) { return ("impar") }
}

e_par_ou_impar_um(7)
e_par_ou_impar_um(4)
e_par_ou_impar_um(0)

# 3- Função que recebe um vetor de números e diz se cada elemento é par ou ímpar.

e_par_ou_impar = function (vetor) {
  result = c()
  for (element in vetor) {
   result = append(result, e_par_ou_impar_um(element)) 
  }
  return (result)
}

e_par_ou_impar(c(2,10,9,1))
e_par_ou_impar(4)

par_ou_impar_named_vector <- function (vector) {
  pair_or_not = c()
  for (element in vector) {
   pair_or_not = append(pair_or_not, e_par_ou_impar_um(element)) 
  }
  names(vector) <- pair_or_not
  return (vector)
}

par_ou_impar_named_vector(c(2,10,9,1))
par_ou_impar_named_vector(4)

# 3- Função que receber um vetor de números e diz a porcentagem de elementos que é par e ímpar.

porcento_par_ou_impar = function (vetor) {
  par_impar = e_par_ou_impar(vetor)
  count_par = 0
  count_impar = 0
  count_zero = 0
  for (element in par_impar) {
   if (element == "par") { count_par = count_par + 1 }
   if (element == "impar") { count_impar = count_impar + 1 }
   if (element == "zero") { count_zero = count_zero + 1 }
  }
  message("Porcentagem par: ", (count_par/length(vetor))*100, "%")
  message("Porcentagem impar: ", (count_impar/length(vetor))*100, "%")
  message("Porcentagem zero: ", (count_zero/length(vetor))*100, "%")
}

porcento_par_ou_impar(c(2,10,9,1))
porcento_par_ou_impar(4)


# 4- Função que mostra elementos da sequência de Fibonacci.

calcular_fibonacci = function (quantos_elementos) {
  if (quantos_elementos <= 0) { return ("Please provide a number larger than 0") }
  fib = c()
  for (i in 1:quantos_elementos) {
    if (i == 1 | i == 2) {
     fib = append(fib, 1) 
    }
    else {
      fib = append(fib, sum(tail(fib, 2)))
    }
  }
  return (fib)
}

calcular_fibonacci(4) # resultado: 1 1 2 3
calcular_fibonacci(7) # resultado: 1 1 2 3 5 8 13
