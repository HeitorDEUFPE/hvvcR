#' descritive.response
#'
#' Recebe um data.frame ou tibble e retorna os gráficos
#' referentes a análise exploratória para um determinado
#'  tipo de variável resposta
#' @param data data.frame ou tibble
#' @param response coluna do data.frame ou tibble em que
#' a variável resposta esta
#' @param type Tipo da variável resposta, pode assumir os valores
#'  contidos no seguinte vetor: c("count","continuous","categoric")
#' @param fill É a cor das linhas do gráfico "grey" por padrão.
#' @param color Cor dos objetos do gráfico "black" por padrão.
#' @export


descritive.response <- function(data,response,type,fill = "grey",color = "black"){
  n <- c(1:ncol(data))
  names.var <- names(data)
  data.count <- NULL
  if(type=="categoric"){
    for(i in n[-response]){
      if(class(data[,i])=="numeric"|class(data[,i])=="integer"){
        par(print(data %>%
                    ggplot(aes(x = data[,response],
                               y = data[,i])) +
                    geom_boxplot(fill = fill, color = color) + 
                    labs(x = names.var[response],
                         y = names.var[i])),
            ask = TRUE)
      }
    }
  }
  if(type=="count"){
    for(i in n[-response]){
      if(class(data[,i])=="numeric"|class(data[,i])=="integer"){
        par(print(data %>%
                    ggplot(aes(x = data[,i],
                               y = data[,response])) +
                    geom_point(fill = fill, color = color) + 
                    labs(x = names.var[i],
                         y = names.var[response])),
            ask = TRUE)
      }
      if(class(data[,i])=="factor"|class(data[,i])=="character"){
        par(print(data  %>%
                    mutate(group = factor(data[,i]),
                           response = data[,response]) %>% 
                    group_by(group) %>% 
                    summarise(count = sum(response)) %>% 
                    ggplot(aes(x = group,
                               y = count)) +
                    geom_bar(stat = "identity",
                             fill = fill,
                             color = color,
                             position = "dodge") + 
                    labs(x = names.var[i],
                         y = names.var[response])),
            ask = TRUE)
      }
    }
  }
  if(type=="continuous"){
    for(i in n[-response]){
      if(class(data[,i])=="numeric"|class(data[,i])=="integer"){
        par(print(data %>%
                    ggplot(aes(x = data[,i],
                               y = data[,response])) +
                    geom_point(fill = fill, color = color) + 
                    labs(x = names.var[i],
                         y = names.var[response])),
            ask = TRUE)
      }
      if(class(data[,i])=="factor"|class(data[,i])=="character"){
        par(print(data %>%
                    ggplot(aes(x = data[,i],
                               y = data[,response])) +
                    geom_boxplot(fill = fill,
                                 color = color) + 
                    labs(x = names.var[i],
                         y = names.var[response])),
            ask = TRUE)
      }
    }
  }
  par(ask = FALSE)
}

