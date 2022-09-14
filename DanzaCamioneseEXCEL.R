  rm(list = ls())
  suppressPackageStartupMessages(library(dplyr, quietly = TRUE)) 
  suppressPackageStartupMessages(library(ROI))
  library(ROI.plugin.glpk)
  library(ROI.plugin.symphony)
  library(ROI.plugin.gurobi)
  library(ompr)
  library(ompr.roi)
  library("readxl")
  
  serv_hra <- read_excel("sol 11pts 1.4.xlsx", sheet = "serv_hra", skip=2, col_names = TRUE, col_types = "numeric")
  hi = 8
  hf = 19
  nh = hf - hi
  horas = paste(seq(hi, hf),":00", sep="")
  colnames(serv_hra) = c("indice", "cliente", "ind+1", horas)
  
  serv_hra = serv_hra[c("indice", horas)]
  serv_hra = as.data.frame(serv_hra)
  np = 11
  col_base = c()
  base = NULL
  for (p in 1:np){
    col_base = c(col_base, paste("P",p,"-",horas , sep=""))
    if (p == 1){
      base = serv_hra
    }else{
      base = cbind(base, serv_hra[horas])
    }
  }
  base = as.data.frame(base)
  #base = read_excel("sol 11pts 1.4.xlsx", sheet = "base", skip=4, col_names = TRUE, col_types = "text")
  colnames(base) = c("indice", col_base)
  base = base[col_base]
  
  cost = read_excel("sol 11pts 1.4.xlsx", sheet = "cost0", skip=0, col_names = TRUE, col_types = "numeric")
  cost = as.data.frame(cost)
  capacidad_hra_planta = read_excel("sol 11pts 1.4.xlsx", sheet = "capacidad_hra_planta", skip=1, col_names = TRUE, col_types = "numeric")
  colnames(capacidad_hra_planta) = c(col_base)
  capacidad_hra_planta = as.data.frame(capacidad_hra_planta)
  #clientes
  n = nrow(serv_hra)
  
  start = Sys.time()
  model = MIPModel() %>%
    add_variable(x[i, j], i = 1:n, j = 1:np, type = "integer") %>%
    set_objective(sum_over(x[i, j] * cost[i,(j+1)]*sum_over(serv_hra[i,(1+h)], h=1:nh), i = 1:n, j = 1:np), "min") %>%
    add_constraint(sum_over(x[i, j], j = 1:np) == 1, i = 1:n) %>%
    add_constraint(sum_over(base[i,(1+h+nh*(j-1))]*x[i, j], i = 1:n) <= capacidad_hra_planta[1,(h+nh*(j-1))],  h=1:nh, j=1:np) %>%
    add_constraint(x[i, j] >= 0, i = 1:n, j = 1:np)
  
  result = model %>%
    solve_model(with_ROI(solver = "glpk", verbose = TRUE))
  
  resultX = result %>%
    get_solution(x[i, j]) %>%
    filter(value > 0) %>%
    arrange(i)
  
  resultX
  end = Sys.time()
  
  cat("Finished in", end - start, "seconds\n", sep = " ")

  
  # Algoritmo genetico https://rpubs.com/Argaadya/550805
  library(GA)
  library(tictoc)
  library(dplyr)
  #df_item <- data.frame(item = c("Tires", "Bumper", "Engine", "Chasis", "Seat"), freq = c(80, 50, 70, 50, 70), weight = c(7, 17, 158, 100, 30))
  
  #df_item
  #df_item_long <- df_item[rep(rownames(df_item), df_item$freq), c(1, 3)]
  
  #df_item_long
  df_assign = data.frame(customer = rep(1:n, np), plant = rep(1:np, n), cost = c(rep(1, n*np)))
  df_assign = df_assign[order(df_assign$customer, df_assign$plant),]
  
  min_cost <- -1e6
  
  evalFunc <- function(x) {
    #df_temp = df_assign
    df_temp = df_assign[x == 1, ]
    #    set_objective(sum_over(x[i, j] * cost[i,(j+1)]*sum_over(serv_hra[i,(1+h)], h=1:nh), i = 1:n, j = 1:np), "min") %>%

    list_cost = c()
    for (i in 1:n){
      for (j in 1:np){
        cost_serv_hora = 0
        for (h in 1:nh){
          cost_serv_hora = cost_serv_hora + serv_hra[i,(1+h)]
        }
        x_ij = df_temp[df_temp$customer == i & df_temp$plant == j,]$cost
        list_cost = c(list_cost, x_ij*cost[i,(j+1)]*cost_serv_hora)
      }
    }
    total_cost = sum(list_cost)
    
    #stop()
    r1_true = c()
    # add_constraint(sum_over(x[i, j], j = 1:np) == 1, i = 1:n)
    for (i in 1:n){
      x_ij = df_temp[df_temp$customer == i,]$cost
      r1_true = c(r1_true, (sum(x_ij) == 1))
    }
    r1 = (sum(r1_true) == n)
    p_r1 = n/sum(r1_true)
    # add_constraint(sum_over(base[i,(1+h+nh*(j-1))]*x[i, j], i = 1:n) <= capacidad_hra_planta[1,(h+nh*(j-1))],  h=1:nh, j=1:np)
    r2_true = c()
    conteo = 0
    for (j in 1:np){
      for (h in 1:nh){
        servicios_cliente = 0
        for(i in 1:n){
          x_ij = df_temp[df_temp$plant == j & df_temp$customer == i,]$cost
          if (length(x_ij) > 0){
            cat("i=", i, " j=", j, " x_ij=", x_ij, "base=", base[i,(1+h+nh*(j-1))],"\n")
            servicios_cliente = servicios_cliente + base[i,(1+h+nh*(j-1))]*x_ij
            conteo = conteo + 1
            #print("entro")
          }
        }
        condicion_1 = servicios_cliente<=capacidad_hra_planta[1,(h+nh*(j-1))]
        #cat("servicios_cliente=", servicios_cliente, " capacidad_hra_planta= ", capacidad_hra_planta[1,(h+nh*(j-1))], " <= ",condicion_1, "\n")
        r2_true = c(r2_true, condicion_1)
      }
    }
    #cat("sum(r2_true)=", sum(r2_true), "==", nh*np, "(",conteo,")", "\n")
    r2 = (sum(r2_true) == nh*np)
    p_r2 = nh*np/sum(r2_true)
    #cat("p_r1=", p_r1, " p_r2=", p_r2, "\n")
    p_r1 = if_else(p_r1 == 1, 0,p_r1)
    p_r2 = if_else(p_r2==1, 0,p_r2)
    min_total_cost = -(total_cost*(p_r1 + p_r2))
    min_total_cost = if_else(min_total_cost < -Inf, min_cost, min_total_cost)
    #cat("r1=", r1, " r2=",r2, " total_cost=", total_cost, "\n")
    total_cost = if_else( r1 & r2, -total_cost, min_total_cost)
    #print(total_cost)
    return(total_cost)
  }
  library(parallel)
  
  
  
  
  n_cores = ceiling(parallel::detectCores()/1.25)
  tic()
  
  maxiter = 1000
  popSize = ifelse(maxiter*0.05<10, 10, maxiter*0.05)
  elitism = ceiling(popSize/2)
  run = maxiter*.05
  
  x = c(1, rep(0, np-1))
  
  dummy_solutions = popSize - 1
  for(d in 1:dummy_solutions){
    r = rep(sample(x), n)
    if (d == 1){
      suggestions = matrix(r, 1, n*np)
    }else{
      suggestions = rbind(suggestions, r)
    }
  }
  
  result_sol <- read_excel("sol 11pts 1.4.xlsx", sheet = "result", col_names = TRUE, col_types = "numeric")
  
  suggestions = matrix(t(result_sol), nrow=1, ncol=n*np)#rbind(suggestions,matrix(t(result_sol), nrow=1, ncol=n*np))
  
  
  gann2 <- ga(type = "binary", fitness = evalFunc, maxiter = maxiter,
              nBits = nrow(df_assign), seed = 123, parallel = n_cores,
              elitism=elitism, popSize=popSize, run=run, suggestions=suggestions)
  
  summary(gann2)
  
  toc()
  plot(gann2)
  
  
  df_sol <- df_assign[gann2@solution[1, ] == 1, ]
  
  df_sol <- df_sol %>% group_by(i, j)
  
  df_sol
  
  sum(df_sol$total_cost)
  