



#' Determines and provides the most suitable plot(s) for two given variables
#'
#' @param df The dataframe or tibble
#' @param var1 First variable
#' @param var2 Second variable
#' @param askente Do you want Ente to choose the plot theme?
#' Default=TRUE
#'
#' @return a ggplot object
#' @import dplyr ggplot2
#' @export
#'
#' @examples
#' twovar(diamonds, cut, price)
twovar <- function(df, var1, var2,askente=TRUE){
  # if less than 10 levels, make it into factor
  num_levs1 <- df %>% select({{var1}}) %>% n_distinct
  num_levs2 <- df %>% select({{var2}}) %>% n_distinct

  if(askente){
    if(max(num_levs1, num_levs2) <= 10){ #both discrete
      df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>%
        mutate({{var2}} := as_factor({{var2}}))
      # then call bar function
      label <- rlang::englue("A count plot of {{var1}} and {{var2}}")
      df %>%
        ggplot(aes(x = {{ var1 }}, y={{var2}})) +
        geom_count(shape=21, fill = "#FFD000") +
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
          axis.ticks.x = eelement_line(color = "#3D4F7DFF"),
          axis.ticks.y = element_line(color = "#3D4F7DFF"),
          legend.key = element_blank()
        ) +
        labs(title=label)
    }else if (min(num_levs1, num_levs2) > 10){ # both continuous
      label <- rlang::englue("A scatter plot of {{var1}} and {{var2}}")
      df %>%
        ggplot(aes(x = {{ var1 }}, y={{var2}})) +
        geom_point(shape=21, fill="#FFD000") +
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
          axis.ticks.x = element_line(color = "#3D4F7DFF"),
          axis.ticks.y = element_line(color = "#3D4F7DFF")
        ) +
        labs(title=label)

    }else if(any(c(num_levs1, num_levs2)) <= 10){ # one continuous, one categorical
      label <- rlang::englue("A plot of {{var1}} and {{var2}}")
      if(num_levs1 <= num_levs2){ # categorize var1
        df <- df %>% mutate({{var1}} := as_factor({{var1}}))
      }else{
        df <- df %>% mutate({{var2}} := as_factor({{var2}}))
      }
      colplot <- df %>%
        ggplot(aes(x={{var1}}, y={{var2}})) +
        geom_col(fill = "#FFD000")+
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
          axis.ticks.x = element_line(color = "#3D4F7DFF"),
          axis.ticks.y = element_line(color = "#3D4F7DFF")
        ) +
        labs(title=label)

      violplot <- df %>%
        ggplot(aes(x={{var1}}, y={{var2}})) +
        geom_violin(fill = "#FFD000")+
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
          axis.ticks.x = element_line(color = "#3D4F7DFF"),
          axis.ticks.y = element_line(color = "#3D4F7DFF")
        ) +
        labs(title=label)
      list(colplot, violplot)

      boxplot <- df %>%
        ggplot(aes(x={{var1}}, y={{var2}})) +
        geom_boxplot(fill = "#FFD000")+
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
          axis.ticks.x = element_line(color = "#3D4F7DFF"),
          axis.ticks.y = element_line(color = "#3D4F7DFF")
        ) +
        labs(title=label)
      list(colplot, violplot, boxplot)
    }
  } else{
    if(max(num_levs1, num_levs2) <= 10){ #both discrete
      df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>%
        mutate({{var2}} := as_factor({{var2}}))
      # then call bar function
      label <- rlang::englue("A count plot of {{var1}} and {{var2}}")
      df %>%
        ggplot(aes(x = {{ var1 }}, y={{var2}})) +
        geom_count(shape=21) +
        labs(title=label)
    }else if (min(num_levs1, num_levs2) > 10){ # both continuous
      label <- rlang::englue("A scatter plot of {{var1}} and {{var2}}")
      df %>%
        ggplot(aes(x = {{ var1 }}, y={{var2}})) +
        geom_point(shape=21)+
        labs(title=label)

    }else if(any(c(num_levs1, num_levs2)) <= 10){ # one continuous, one categorical
      label <- rlang::englue("A plot of {{var1}} and {{var2}}")
      if(num_levs1 <= num_levs2){ # categorize var1
        df <- df %>% mutate({{var1}} := as_factor({{var1}}))
      }else{
        df <- df %>% mutate({{var2}} := as_factor({{var2}}))
      }
      colplot <- df %>%
        ggplot(aes(x={{var1}}, y={{var2}})) +
        geom_col()+
        labs(title=label)

      violplot <- df %>%
        ggplot(aes(x={{var1}}, y={{var2}})) +
        geom_violin()+
        labs(title=label)
      list(colplot, violplot)

      boxplot <- df %>%
        ggplot(aes(x={{var1}}, y={{var2}})) +
        geom_boxplot()+
        labs(title=label)
      list(colplot, violplot, boxplot)
    }
}
}
