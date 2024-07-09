#-------------------------------------------------------------------afzali teste-----------------------------------------------------------------------------------
library(SoilR)

#-----------------------------------------------------------------------PET----------------------------------------------------------------------------------
library(SPEI)

#------------------------------------------------------------------Support Library------------------------------------------------------------------------
library(data.table)
library(datasets)
library("xlsx")
library(readxl)
library(magrittr)
library(dplyr)
library(purrr)

#-----------------------------------------------------------------------Data------------------------------------------------------------------------------
dataf <- read_excel("place", 
                    col_types = c("text","text","numeric","numeric","numeric","numeric")
                    ,sheet = "Sheet1")


##----------------------------------------------------------------Moisture modifield---------------------------------------------------------------------------------------
fW.RothC.Modified <- 
  function (P, 
            E, 
            S.Thick, 
            pClay, 
            pE, 
            bare_profile) 
  {
    # Covered: B = 0.6
    # Bare: B = 1
    
    Max.TSMD_bare = -(20 + 1.3 * pClay - 0.01 * (pClay^2)) * (S.Thick/23) * 
      (1/1)
    Max.TSMD_covered = -(20 + 1.3 * pClay - 0.01 * (pClay^2)) * (S.Thick/23) * 
      (1/0.6)
    
    Max.TSMD <- as.data.frame(bare_profile)
    
    Max.TSMD = Max.TSMD %>% 
      mutate(Max.TSMD = ifelse(bare_profile == TRUE, Max.TSMD_bare, Max.TSMD_covered))
    
    M = P - E * pE
    
    TSMD = Max.TSMD %>% select(Max.TSMD)
    TSMD$Acc.TSMD = 0
    
    for (i in 2:length(M)) {
      TSMD[1,"Acc.TSMD"] = ifelse(M[1] > 0, 0, M[1])
      if (TSMD[i - 1,"Acc.TSMD"] + M[i] < 0) {
        TSMD[i,"Acc.TSMD"] = TSMD[i - 1,"Acc.TSMD"] + M[i]
      }
      else{TSMD[i,"Acc.TSMD"] = 0}
      if (TSMD[i,"Acc.TSMD"] <= TSMD[i,"Max.TSMD"]) {
        TSMD[i,"Acc.TSMD"] = TSMD[i,"Max.TSMD"]
      }
    }
    
    TSMD <- TSMD %>% 
      rowwise() %>% 
      mutate(
        b = ifelse(Acc.TSMD > 0.444 * Max.TSMD, 1, (0.2 + 0.8 * ((Max.TSMD - Acc.TSMD)/(Max.TSMD - 0.444 * Max.TSMD))))
      )
    
    return(TSMD)
  }


#----------------------Anomalies per scenario and climate prediction model (Temperature and precipitation with 24 items each)------------------------------------
#RCP26 CanESM2 
RCTC = c(-0.71,-0.71,	-0.23,-0.23,-0.23,-0.14,-0.14,-0.14, -0.34,-0.34,-0.34,-0.71, 2.13,2.13, 2.13,2.13,2.13, 1.82,1.82,1.82, 1.91, 1.91, 1.91, 2.13)

#RCP26 MPI-ESM-LR 
RCTM = c(-0.01,-0.01,	-0.12,-0.12,-0.12, -0.02,-0.02,-0.02, -0.16,-0.16,-0.16, -0.01, 1.22, 1.22,	1.3,1.3,1.3, 1.17,1.17,1.17, 1.32, 1.32, 1.32, 1.22)

#RCP45 CanESM2 
RCFC = c(-1.03,-1.03,	-0.66,-0.66,-0.66, -0.31,-0.31,-0.31,	-0.37,-0.37,-0.37,-1.03, 2.94,2.94, 2.95,2.95,2.95, 2.68,2.68,2.68, 2.89, 2.89, 2.89,2.94)

#RCP45 MPI-ESM-LR 
RCFM = c(0.66,0.66, 0.06,0.06,0.06, -0.04,-0.04,-0.04, -0.22,-0.22,-0.22,0.66, 2, 2, 2.09,2.09,2.09,	2.02,2.02,2.02,	2.29,	2.29,	2.29, 2)



#----------------------------------------------------Latitudes-------------------------------------------------------------------------------------------
latn = c(7.7467,-5.607,-8.0683)

#----------------------------------------Create an empty list to store the vectors-------------------------------------------------------------
vector_list <- list()

# Set up the loop to create 12 vectors
for (z in 1:3) {
  # Generate the start and end values for this vector
  start_value <- ((z - 1) * 12) + 1
  end_value <- z * 12
  
  # Create the vector by concatenating the values using the c() function
  new_vector <- c(start_value:end_value)
  
  # Add the completed vector to the list
  vector_list[[z]] <- new_vector
}


#----------------------------------------------------Climate function---------------------------------------------------------------------------------------
Sv<- function(na, i){
  namev = c("Av M Temp( °C)","Rain mm/month","SOC(tC/ha)","clay%")
  d = dataf[[namev[na]]]
  return(d[i])
}



#-------------------------------------------------aggregation wheather function-----------------------------------------------------------------------------
FuncTts <- function(est, pr) {
  if (est < 1 || est > 3 || pr < 0 || pr > 4) {
    stop("Índices de estação ou cenário inválidos")
  }
  
  # Inicializa as matrizes para armazenar os resultados de temperatura e precipitação
  resultados_temperatura <- numeric(12)
  resultados_precipitacao <- numeric(12)
  
  # Calcula os índices de temperatura com base na estação
  indices_temperatura <- vector_list[[est]]
  
  # Calcula os índices de precipitação com base na estação e no cenário
  indices_precipitacao <- vector_list[[est]]
  
  # Calcula os valores de temperatura e precipitação para o cenário atual
  if (pr == 0) {
    # Cenário natural sem alteração
    valores_temperatura <- Sv(1, indices_temperatura)
    valores_precipitacao <- Sv(2, indices_temperatura)  # Usar os mesmos índices de temperatura
  } else {
    # Cenários modificados
    df_anomalias <- data.frame(
      RCP26_CanESM2 = RCTC,
      RCP26_MPI_ESM_LR = RCTM,
      RCP45_CanESM2 = RCFC,
      RCP45_MPI_ESM_LR = RCFM
    )
    
    lista_1 <- t(sapply(df_anomalias[[pr]], unlist))
    list_2 <- t(sapply(df_anomalias[[pr]], unlist))
    
    valores_temperatura <- Sv(1,)[vector_list[[est]]] + lista_1[1:12]
    valores_precipitacao <- Sv(2,)[vector_list[[est]]] + list_2[13:24]
  }
  
  # Armazena os valores nas matrizes de resultados
  resultados_temperatura <- valores_temperatura
  resultados_precipitacao <- valores_precipitacao
  
  # Retorna as matrizes de resultados de temperatura e precipitação
  return(list(resultados_temperatura, resultados_precipitacao))
}



#-------------------------------------------Calculate the potential evapotranspiration for the three natural seasons-------------------------------------------------
PETs =c()
for (s in seq(1:3))
  PETs[s] <- list(thornthwaite(FuncTts(s,0)[[1]], latn[s]))
#Permite usar os valores de PETs para demais funções do script
Sp<- function(na, i){
  d = PETs[[na]]
  return(d[i])
}
#criar vetor com valores de 0.75 com tamanho 12
div =c()
i <- 1
while(i <= 12){
  div[i] = 0.75
  i <- i + 1
}
#Converte para os valores de open-pan dividindo por 0.75
SDDs = c()
a <- 1
while(a <= 3){
  SDDs[a] = list((Sp(a,)/div[])[1:12])
  a <- a + 1
}
SDDs <- t(sapply(SDDs, unlist))




#------------------------------------------------Model calibration using the seasons without anomalies--------------------------------------------------------------
C <- list()  # Initialize list to store results

for (a in 1:3) {
  # Wrap the code block with tryCatch
  tryCatch({
    # Your existing code
    
    #fT=fT.RothC(Temp[,2]) #Temperature effects per month
    Tt=fT.RothC(Temp = c(FuncTts(a,0)[[1]]))
    Ww=fW.RothC.Modified(P=FuncTts(a,0)[[2]], E=SDDs[a,], S.Thick=30, pClay=Sv(4,a), pE=0.75, bare_profile=c(F,F,F,F,F,F,F,F,F,F,F,F))$b
    
    #função modificado Soil CArbon factor
    #bare = 2, vegetated =1 / SOIL CARBON FACTOR / Anual - 1.05 para cana
    
    #Calculo do IOM por Falloon et al 1998.
    IOMi = 0.049*((Sv(3,a))^1.139)
    
    # Constantes de decomposição dos pools do RothC
    kDPM = 10
    kRPM = 0.3
    kBIO = 0.66
    kHUM = 0.02
    
    #Tempo Steady State
    years=seq(1/12, 500, by=1/12) 
    
    CDIex=data.frame(years,rep(Tt*Ww,length.out=length(years)))
    
    RvalCarbon<- function(invro){
      return(RothCModel(t=years,
                        ks = c(k.DPM = kDPM, k.RPM = kRPM, k.BIO = kBIO, k.HUM = kHUM, k.IOM = 0),
                        C0 = c(0, 0, 0, 0, IOMi),
                        In = invro,
                        FYM = 0,
                        DR = 1.44,
                        clay = Sv(4,a),
                        xi = CDIex,
                        solver = deSolve.lsoda.wrapper,
                        pass = FALSE))}
    
    all_results <- possibly(RvalCarbon, otherwise = "error in file")
    
############################################## executa o rothC para tempo e entrada In variável ####################################################################
    invro = 0.5                                                                                       #t <-seq(0,1000,1)
    ct <- function(invro){ 
      ExRv=(all_results(invro))             
      Ctr=getC(ExRv) 
      Totn <- rowSums(Ctr)
      return (Totn[6000]-Sv(3,a))
    }
    
    newton.raphson <- function(f, a, b, tol = 1e-3, n = 1000) {
      require(numDeriv) # Package for computing f'(x)
      
      x0 <- a # Set start value to supplied lower bound
      k <- n # Initialize for iteration results
      
      # Check the upper and lower bounds to see if approximations result in 0
      fa <- f(a)
      if (fa == 0.0) {
        return(a)
      }
      
      fb <- f(b)
      if (fb == 0.0) {
        return(b)
      }
      
      for (i in 1:n) {
        dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
        x1 <- x0 - (f(x0) / dx) # Calculate next value x1
        k[i] <- x1 # Store x1
        # Once the difference between x0 and x1 becomes sufficiently small, output the results.
        if (abs(x1 - x0) < tol) {
          root.approx <- tail(k, n=1)
          #res <- list('root approximation' = root.approx, 'iterations' = length(k))
          #res <- list(root.approx, 'iterations' = length(k))
          res <- root.approx
          return(res)
        }
        # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
        x0 <- x1
      }
      print('Too many iterations in method')
    }
    
    #Computing the root of the above equation with the newton.raphson function yields:
    #Defina a função e os limites de raiz
    answer <- newton.raphson(ct, 1, 20)
    
    #############################get the pools values after input calibration #####################
    
    ctpool <- RvalCarbon(answer)
    cti <- getC(ctpool)
    ctv <- c(cti[6000, ])
    
    # Store the results in the list
    C[[a]] <- list(c(ctv[1], ctv[2], ctv[3], ctv[4], IOMi, answer))
  }, error = function(e) {
    # If an error occurs during tryCatch block execution
    # you can print a message or handle it as per your requirement
    print(paste("Error encountered for a =", a))
    # Skip to the next iteration
    return(NULL)
  })
}




#-------------------------------- Evapotranspiration for all lists with a generic variable----------------------------------------------------------------
ss <- list()
for (r in 1:3) {
  grupo <- list()  # Inicializa uma lista para cada grupo
  for (y in 0:4) {
    PETf <- c(thornthwaite(FuncTts(r, y)[[1]], latn[r]))  # Calcula o valor de PETf
    grupo[[y+1]] <- PETf  # Adiciona o valor de PETf ao grupo correspondente
  }
  ss[[length(ss) + 1]] <- grupo  # Adiciona o grupo à lista ss
}
# Dividir todos os valores por 0.75
for (i in seq_along(ss)) {
  for (j in seq_along(ss[[i]])) {
    ss[[i]][[j]] <- ss[[i]][[j]] / 0.75
  }
}



#-------------------------------------------------Simulaçaõ do steady State 30 anos-----------------------------------------------------------------------------
SimT = list()
for(r in 1:3){
  if(is.null(C[[r]][1])) {
    print(0)
  } 
else{
  gr <- list()
  for(y in 0:4){
    time=seq(1/12, 30, by=1/12) 
    Ts=fT.RothC(Temp = c(FuncTts(r,y)[[1]]))
    Ws=fW.RothC.Modified(P=FuncTts(r,y)[[2]], E=ss[[r]][[y+1]], S.Thick=30, pClay=Sv(4,r), pE=0.75, bare_profile=c(F,F,F,F,F,F,F,F,F,F,F,F))$b
    CDI=data.frame(years,rep(Ts*Ws,length.out=length(years)))
    RCar<- function(inv){
      return(RothCModel(t=time,
                        ks = c(k.DPM = kDPM, k.RPM = kRPM, k.BIO = kBIO, k.HUM = kHUM, k.IOM = 0),
                        C0 = c(C[[r]][[1]][1], C[[r]][[1]][2], C[[r]][[1]][3], C[[r]][[1]][4], C[[r]][[1]][5]),
                        In = C[[r]][[1]][6],
                        FYM = 0,
                        DR = 0.67,
                        clay = Sv(4,r),
                        xi = CDI,
                        solver = deSolve.lsoda.wrapper,
                        pass = FALSE))
    }
    Ex=RCar(t)  #RothC
    Ctrs=getC(Ex)      #RothC retorna os valores para cada pool
    gr[[y+1]] <- rowSums(Ctrs)
  }
  SimT[[length(SimT)+1]] <- gr
}
}



#------------------------------------------gráfico - simulações de cenários de mudança climática ---------------------------------------------------------------
# Dados de exemplo
for(mud in 1:2){
  tp <- seq(0:29)
  tc_ha_ano_1 <- SimT[[mud]][1]
  tc_ha_ano_2 <- SimT[[mud]][2]
  tc_ha_ano_3 <- SimT[[mud]][3]
  tc_ha_ano_4 <- SimT[[mud]][4]
  tc_ha_ano_5 <- SimT[[mud]][5]
  
  novos_valores_1 <- tc_ha_ano_1[[1]][seq(1, length(tc_ha_ano_1[[1]]), by = 12)]
  novos_valores_2 <- tc_ha_ano_2[[1]][seq(1, length(tc_ha_ano_2[[1]]), by = 12)]
  novos_valores_3 <- tc_ha_ano_3[[1]][seq(1, length(tc_ha_ano_3[[1]]), by = 12)]
  novos_valores_4 <- tc_ha_ano_4[[1]][seq(1, length(tc_ha_ano_4[[1]]), by = 12)]
  novos_valores_5 <- tc_ha_ano_5[[1]][seq(1, length(tc_ha_ano_5[[1]]), by = 12)]
  
  
  # Criar o gráfico com eixo x ajustado
  plot(tp, novos_valores_1, type = "l", col = "blue", xlab = "Tempo (anos)", ylab = "Taxa de crescimento (tc/ha)", main = "Gráfico de exemplo", ylim = c(22, 35), xlim = c(0, max(tp)))  # Aqui os limites do eixo x são definidos como de 0 ao máximo de tp
  
  
  # Adicionar as outras linhas
  lines(tp, novos_valores_2, col = "red")
  lines(tp, novos_valores_3, col = "green")
  lines(tp, novos_valores_4, col = "orange")
  lines(tp, novos_valores_5, col = "black")
  
  # Adicionar legenda
  legend("topright", legend = c("Linha 1", "Linha 2", "Linha 3", "Linha 4"), col = c("blue", "red", "green", "orange","black"), lty = 1)
}

