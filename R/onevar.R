
#' Determines and provides the most suitable plot for a given variable
#'
#' @description
#' The function will automatically determine whether the given variables should be categorical or numerical.
#' Variables with more than 10 categories will be treated as numerical.
#' @param df The data frame or tibble
#' @param var The variable you wish to explore (numerical or categorical)
#' @param askente Do you want Ente to choose the plot theme?
#' Default=TRUE
#'
#' @return a ggplot object
#' @export
#' @import dplyr ggplot2
#'
#' @examples
#' onevar(diamonds, cut)
onevar <- function(df, var, askente=TRUE){

  # if less than 10 levels, make it into factor
  num_levs <- df %>% select({{var}}) %>% n_distinct

  if(askente){
    if(num_levs <= 10){
      df <- df %>% mutate({{var}} := as_factor({{var}}))
      # then call bar function
      label <- rlang::englue("A barplot of total {{var}}")
      st <- df %>% mutate({{ var }} := fct_rev(fct_infreq({{ var }}))) %>%
        group_by({{var}}) %>% summarise(n = n()) %>% ungroup()
      st %>%
        ggplot(aes(x = {{ var }}, y=n, fill=n)) +
        geom_bar(stat="identity") +
        scale_fill_gradient2(low='#FFEA00', mid='#FFD000', high='#FF7B00') +
        geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.5,
                  color = "#023047")+
        theme(
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "#023047"),
          axis.title = element_text(colour = "#14213D"),
          axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
          plot.title = element_text(size = 16, hjust = 0.5, colour = "#14213D", margin = margin(10, 0, 10, 0)),
          plot.margin = unit(c(1,1,1,1), "cm"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(color = "#3D4F7DFF"),
          legend.position = "none"
        ) +
        labs(title=label, y = "counts")
    }else{
      # call histogram function
      label <- rlang::englue("A histogram of {{var}}")
      r <- df %>% reframe(r = range({{var}}))
      binwidth = as.numeric(r[2,]-r[1,]) / sqrt(nrow(df))
      df %>%
        ggplot(aes(x = {{ var }})) +
        geom_histogram(binwidth = binwidth, aes(y=after_stat(density)), color = "#FCA311",
                       fill = "#FFF3B0") +
        geom_density(col = "#14213D") +
        labs(title = label) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                           labels = function(x) format(x, scientific = FALSE)) +
        theme(
          panel.background = element_blank(),
          plot.background = element_rect(fill="white"),
          axis.text = element_text(color = "#3D4F7DFF"),
          plot.title = element_text(color = "#14213D", hjust = 0.5, size = 16),
          plot.subtitle = element_text(color = "white", hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          axis.line.y.left = element_line(colour = "#3D4F7DFF"),
          axis.line.x.bottom = element_line(colour = "#3D4F7DFF"),
          plot.caption = element_text(color = "white", hjust = 1),
          axis.ticks = element_line(color = "#3D4F7DFF"),
          axis.title.y = element_text(angle = 0, vjust = 0.5, color = "#14213D"),
          axis.title.x = element_text(color = "#14213D")
        )
    }
  } else{
    if(num_levs <= 10){
      df <- df %>% mutate({{var}} := as_factor({{var}}))
      # then call bar function
      label <- rlang::englue("A barplot of total {{var}}")
      st <- df %>% mutate({{ var }} := fct_rev(fct_infreq({{ var }}))) %>%
        group_by({{var}}) %>% summarise(n = n()) %>% ungroup()
      st %>%
        ggplot(aes(x = {{ var }}, y=n, fill=n)) +
        geom_bar(stat="identity") +
        labs(title=label, y = "counts")
    }else{
      # call histogram function
      label <- rlang::englue("A histogram of {{var}}")
      r <- df %>% reframe(r = range({{var}}))
      binwidth = as.numeric(r[2,]-r[1,]) / sqrt(nrow(df))
      df %>%
        ggplot(aes(x = {{ var }})) +
        geom_histogram(binwidth = binwidth, aes(y=after_stat(density))) +
        geom_density() +
        labs(title = label)
    }
  }
}

