# Método Simplex ----------------------------------------------------------
# Gabriel Lopes Rubinho - RA: 125009

# Maximizar            :            z = 3x + 2y 
# Sujeito às restrições:            2x + y <= 18
#                                   2x + 3y <= 42
#                                   3x + y <= 24
#                                   x >= 0, y >= 0

matriz_restricoes <- matrix(c(2, 1, 0, 2, 3, 0, 3, 1, 0, 5, 6, 7, 4, 8, 1), nrow = 5, ncol = 3, byrow = TRUE)
metodo_simplex <- function(funcao_objetivo, matriz_restricoes, 
                           resultados, maxi = TRUE) 
{
  # Como sempre irá passar os valores para o outro lado da equação, os valores da função
  # Objetivo serão trocados de sinal
  
  for (i in 1:length(funcao_objetivo))
  {
    funcao_objetivo[i] <- funcao_objetivo[i]
  }
  
  # Precisamos saber a quantidade de linhas e colunas da matriz de restrições dada
  
  numero_linha  <- nrow(matriz_restricoes)
  numero_coluna <- ncol(matriz_restricoes)
  
  # Só pode fazer se o número de linhas da matriz for igual ao número de resultados (b1)
  
  if (nrow(matriz_restricoes) == length(resultados)) 
    {
    
    matriz <- matrix(ncol = 2 + numero_coluna + numero_linha, nrow = numero_linha + 1)
    # Criação da matriz geral para casos
    
    colnames(matriz) <- c('Cb', 'Po', paste0('P', seq(1, ncol(matriz)-2))) 
    rownames(matriz) <- c(paste0('P', seq(numero_coluna + 1, ncol(matriz)- 2)), 'Z')
    # Acima, alterei o nome das colunas para melhor visualização

    for (i in 1:nrow(matriz) - 1) {
      matriz[i, 1] <- 0
      matriz[i, 2] <- resultados[i]
    }
    
    # Agora, implementar baseado na quantidade de variáevis
    
    for (i in 1:numero_coluna) { # COMEÇA NA 3º COLUNA 
      for (j in 1:numero_linha) {
        matriz[j, i+2] <- matriz_restricoes[j, i] # tem um +3 na coluna, pois sempre começará da 3 
      }
    }
    
    # O código acima foi usado para montar as variáveis p1, p2, ..., pN, baseada na quantidade de variável da função 
    
    diag(matriz[,c(paste0('P', seq(numero_coluna+1, ncol(matriz)-2)))]) <- 1
    
    # Na linha cima, substuímos todos os valores da diagonal restante pelas variáveis de "apoio"
    
  
    matriz[is.na(matriz)] <- 0
    
    for (i in 1:length(funcao_objetivo)){
      matriz[nrow(matriz), i+2] <- -funcao_objetivo[i]
    }
    
    
    valores_base <- c(0, 0, funcao_objetivo, rep(0, numero_linha))
    
    # A PARTIR DAQUI, A MATRIZ ESTÁ MONTADA
    
    # Precisamos escolher o menor valor da última linha
    # A partir daqui, começa a iteração
    
    
    while((min(matriz[nrow(matriz),]) < 0) == TRUE) {
      menor_valor <- min(matriz[nrow(matriz), ])
      
      for (i in 1:ncol(matriz)) {
        if (matriz[nrow(matriz), i] == menor_valor){
          vetor_coluna <- matriz[, i]
          break
        }
      } 
      
      # Acima, é feito uma conferência do menor valor da linha Z, para salvar os valores da coluna
      # em que a mesma se encontra
      
      
      for (i in 1:ncol(matriz)) {
        if(matriz[, i] == vetor_coluna) {
          get <- matriz[,2]/vetor_coluna
          salvar_coluna <- i
          break
        }
      } # Aqui, é feito as divisões da coluna pivô pelo vetor coluna, para tentar achar o menor

      
      get <- get[-length(get)] # Eliminamos o último valor, pois é da linha Z
      
      get <- get[get > 0] # Só pode escolher se for maior que 0
      
      valor_linha <- min(get) # Escolhendo o valor 
    
      
  
      for (i in 1:nrow(matriz)) {
        if (matriz[i, 2]/vetor_coluna[i] == valor_linha) {
          linha_salva <- i
        }
        
      } # Escolhendo o valor da linha que se encontra
      
      pivo <- matriz[linha_salva, salvar_coluna] # Atribuindo o valor ao pivô (interseçção entre
      # a linha e a coluna)
      
      matriz1 <- matriz
      
      matriz_teste <- matriz1
      
      for (i in 1:(ncol(matriz))) {
        
        matriz_teste[linha_salva, i] <- round(matriz_teste[linha_salva, i]/pivo, 2)
      }
    
      # ACIMA, É PRA LINHA DO PIVO
      
      matriz_teste1 <- matriz_teste
      
      # BAIXO, DEMAIS LINHAS
      # fazer uma do 0 pq a outra ta dando errado e eu n faço ideia do pq
      for (i in 1:nrow(matriz_teste)) {
        if (i == linha_salva) next 
        
        for (j in 1:ncol(matriz_teste)) {
          value <- matriz_teste[i, j] - (matriz_teste[i, salvar_coluna]*matriz_teste[linha_salva, j])
          matriz_teste1[i, j] <- value
          
        }
        
      }
      
      matriz_teste1[linha_salva, 1] <- valores_base[salvar_coluna]
      
      
      matriz <- matriz_teste1
  
    }
    
    # O caso acima engloba, caso tenha mais de duas 
    
  }
  
  else
    
  {
    stop('Erro na quantidade de linhas da matriz.')
  }
  
  coeficientes <- c(paste0('x', seq(1, length(funcao_objetivo))))
  valores <- c(funcao_objetivo)
  df1 <- data.frame(coefficients, values)
  
  #return(matriz)

  print('Problema de maximização com os coeficientes ')
  print(df1)
  a <- c()
  
  for (i in 1:nrow(matriz)) {
    if(matriz[i, 1] > 0) {
      a[i] <- matriz[i, 2]
      
    }
  }
  
  a <- a[!is.na(a)]
  is.integer(a)
  
  print(paste("Os valores são: ", paste(a, collapse = ", "))) # n tava funcionando o outro
  paste(c("O valor ótimo da função objetivo é: ", optimal), collapse=" ")
  
  
  
  
  #message("Valores da solução: ", a)
  #cat('Valores da solução: ', a)
  
  #cat("O valor 'optimal' da função objetiva é:", matriz[nrow(matriz), 2])
}

matriz_restricoes <- matrix(c(2, 1, 2, 3, 3, 1), nrow = 3, byrow = TRUE)
metodo_simplex(c(3, 2), matriz_restricoes, c(18, 42, 24))









