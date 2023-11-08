


#' Determines and provides the most suitable plot(s) for three given variables
#'
#' @param df The dataframe or tibble
#' @param var1 First variable
#' @param var2 Second variable
#' @param var3 Third variable
#' @param askente Do you want Ente to choose the plot theme?
#' Default=TRUE
#'
#' @return a ggplot object
#' @import dplyr ggplot2
#' @export
#'
#' @examples
#' threevar(diamonds, table, color, cut)
threevar <- function(df, var1, var2, var3, askente=TRUE){

  num_levs1 <- df %>% select({{var1}}) %>% n_distinct
  num_levs2 <- df %>% select({{var2}}) %>% n_distinct
  num_levs3 <- df %>% select({{var3}}) %>% n_distinct

  if(askente){
    if(max(num_levs1, num_levs2, num_levs3) <= 10){ # all discrete
      df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>%
        mutate({{var2}} := as_factor({{var2}})) %>%
        mutate({{var3}} := as_factor({{var3}}))
      # then call bar function
      label <- rlang::englue("A count plot of {{var1}} and {{var2}} faceted by {{var3}}")
      df %>%
        ggplot(aes(x = {{ var1 }}, y={{var2}})) +
        geom_count(shape=21, fill = "#FFD000") +
        facet_wrap(vars({{var3}})) +
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
          axis.ticks.y = element_line(color = "#3D4F7DFF"),
          legend.key = element_blank(),
          strip.background = element_rect(fill = "#3D4F7DFF"),
          strip.text = element_text(colour = "white")
        ) +
        labs(title=label)
    }else if (min(num_levs1, num_levs2, num_levs3) > 10){ # all continuous
      label <- rlang::englue("A scatter plot of {{var1}} and {{var2}} colored by {{var3}}")
      df %>%
        ggplot(aes(x = {{ var1 }}, y={{var2}}, color={{var3}})) +
        geom_point() +
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

    }else if(sum(c(num_levs1, num_levs2, num_levs3) > 10) == 2){ # two continuous, one categorical
      label <- rlang::englue("A scatter plot of {{var1}} and {{var2}} faceted by {{var3}}")

      cat_var_ind <- which.min(c(num_levs1, num_levs2, num_levs3))

      if(cat_var_ind == 1){ # categorize var1
        df <- df %>% mutate({{var1}} := as_factor({{var1}}))
        df %>%
          ggplot(aes(x = {{ var2 }}, y={{var3}})) +
          geom_point() +
          facet_wrap(vars({{var1}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            legend.key = element_blank(),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
      }else if(cat_var_ind == 2){ # cat var2
        df <- df %>% mutate({{var2}} := as_factor({{var2}}))
        df %>%
          ggplot(aes(x = {{ var1 }}, y={{var3}})) +
          geom_point() +
          facet_wrap(vars({{var2}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            legend.key = element_blank(),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
      }else{ # cat var3
        df <- df %>% mutate({{var3}} := as_factor({{var3}}))
        df %>%
          ggplot(aes(x = {{ var1 }}, y={{var2}})) +
          geom_point() +
          facet_wrap(vars({{var3}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            legend.key = element_blank(),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
      }

    } else if (sum(c(num_levs1, num_levs2, num_levs3) > 10) == 1){  # one continuous, two categorical

      num_var_ind <- which.max(c(num_levs1, num_levs2, num_levs3))

      if(num_var_ind == 1){   # var1 cont
        label <- rlang::englue("A plot of {{var1}} and {{var2}} faceted by {{var3}}")
        df <- df %>% mutate({{var2}} := as_factor({{var2}})) %>% mutate({{var3}} := as_factor({{var3}}))
        colplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var2}})) +
          geom_col(fill = "#FFD000")+
          facet_wrap(vars({{var3}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)

        violplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var2}})) +
          geom_violin(fill = "#FFD000")+
          facet_wrap(vars({{var3}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
        list(colplot, violplot)

        boxplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var2}})) +
          geom_boxplot(fill = "#FFD000")+
          facet_wrap(vars({{var3}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
        list(colplot, violplot, boxplot)
      }else if(num_var_ind == 2){  # var2 cont
        label <- rlang::englue("A plot of {{var1}} and {{var2}} faceted by {{var3}}")
        df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>% mutate({{var3}} := as_factor({{var3}}))
        colplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var2}})) +
          geom_col(fill = "#FFD000")+
          facet_wrap(vars({{var3}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)

        violplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var2}})) +
          geom_violin(fill = "#FFD000")+
          facet_wrap(vars({{var3}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
        list(colplot, violplot)

        boxplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var2}})) +
          geom_boxplot(fill = "#FFD000")+
          facet_wrap(vars({{var3}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
        list(colplot, violplot, boxplot)
      }else{ #var3 cont
        label <- rlang::englue("A plot of {{var1}} and {{var3}} faceted by {{var2}}")
        df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>% mutate({{var2}} := as_factor({{var2}}))
        colplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var3}})) +
          geom_col(fill = "#FFD000")+
          facet_wrap(vars({{var2}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)

        violplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var3}})) +
          facet_wrap(vars({{var2}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
        list(colplot, violplot)

        boxplot <- df %>%
          ggplot(aes(x={{var1}}, y={{var3}})) +
          facet_wrap(vars({{var2}})) +
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
            axis.ticks.y = element_line(color = "#3D4F7DFF"),
            strip.background = element_rect(fill = "#3D4F7DFF"),
            strip.text = element_text(colour = "white")
          ) +
          labs(title=label)
        list(colplot, violplot, boxplot)
      }

    }
    }else{
      if(max(num_levs1, num_levs2, num_levs3) <= 10){ # all discrete
        df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>%
          mutate({{var2}} := as_factor({{var2}})) %>%
          mutate({{var3}} := as_factor({{var3}}))
        # then call bar function
        df %>%
          ggplot(aes(x = {{ var1 }}, y={{var2}})) +
          geom_count() +
          facet_wrap(vars({{var3}}))
      }else if (min(num_levs1, num_levs2, num_levs3) > 10){ # all continuous

        df %>%
          ggplot(aes(x = {{ var1 }}, y={{var2}}, color={{var3}})) +
          geom_point()

      }else if(sum(c(num_levs1, num_levs2, num_levs3) > 10) == 2){ # two continuous, one categorical

        cat_var_ind <- which.min(c(num_levs1, num_levs2, num_levs3))

        if(cat_var_ind == 1){ # categorize var1
          df <- df %>% mutate({{var1}} := as_factor({{var1}}))
          df %>%
            ggplot(aes(x = {{ var2 }}, y={{var3}})) +
            geom_point() +
            facet_wrap(vars({{var1}}))

        }else if(cat_var_ind == 2){ # cat var2
          df <- df %>% mutate({{var2}} := as_factor({{var2}}))
          df %>%
            ggplot(aes(x = {{ var1 }}, y={{var3}})) +
            geom_point() +
            facet_wrap(vars({{var2}}))

        }else{ # cat var3
          df <- df %>% mutate({{var3}} := as_factor({{var3}}))
          df %>%
            ggplot(aes(x = {{ var1 }}, y={{var2}})) +
            geom_point() +
            facet_wrap(vars({{var3}}))

        }

      } else if (sum(c(num_levs1, num_levs2, num_levs3) > 10) == 1){  # one continuous, two categorical

        num_var_ind <- which.max(c(num_levs1, num_levs2, num_levs3))

        if(num_var_ind == 1){   # var1 cont
          label <- rlang::englue("A plot of {{var1}} and {{var2}} faceted by {{var3}}")
          df <- df %>% mutate({{var2}} := as_factor({{var2}})) %>% mutate({{var3}} := as_factor({{var3}}))
          colplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var2}})) +
            geom_col()+
            facet_wrap(vars({{var3}}))


          violplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var2}})) +
            geom_violin()+
            facet_wrap(vars({{var3}}))

          boxplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var2}})) +
            geom_boxplot()+
            facet_wrap(vars({{var3}}))

          list(colplot, violplot, boxplot)

        }else if(num_var_ind == 2){  # var2 cont
          label <- rlang::englue("A plot of {{var1}} and {{var2}} faceted by {{var3}}")
          df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>% mutate({{var3}} := as_factor({{var3}}))
          colplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var2}})) +
            geom_col()+
            facet_wrap(vars({{var3}}))


          violplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var2}})) +
            geom_violin()+
            facet_wrap(vars({{var3}}))

          boxplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var2}})) +
            geom_boxplot()+
            facet_wrap(vars({{var3}}))

          list(colplot, violplot, boxplot)

        }else{ #var3 cont
          label <- rlang::englue("A plot of {{var1}} and {{var3}} faceted by {{var2}}")
          df <- df %>% mutate({{var1}} := as_factor({{var1}})) %>% mutate({{var2}} := as_factor({{var2}}))
          colplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var3}})) +
            geom_col()+
            facet_wrap(vars({{var2}})) +


          violplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var3}})) +
            facet_wrap(vars({{var2}})) +
            geom_violin()



          boxplot <- df %>%
            ggplot(aes(x={{var1}}, y={{var3}})) +
            facet_wrap(vars({{var2}})) +
            geom_boxplot()

          list(colplot, violplot, boxplot)
        }

      }
  }
}
