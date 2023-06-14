
boxplots_churn <- function(data, save = FALSE) {
  p <- data %>% 
    select(c(where(is.numeric), Churn)) %>%
    pivot_longer(-Churn, names_to = "variavel", values_to = "valor") %>%
    ggplot(aes(y = Churn, x = valor, fill = Churn)) +
    geom_boxplot() +
    facet_wrap(~variavel, scales = "free_x") +
    ggtitle("Churn vs. Variáveis Numéricas")
  
  if(!save) return(p)
  
  ggplot2::ggsave("boxplot.png")
}