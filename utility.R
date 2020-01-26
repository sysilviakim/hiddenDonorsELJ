library(plyr)
library(sf)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(scales)
if (!require(wrangleFEC)) devtools::install_github("sysilviakim/wrangleFEC")
if (!require(Kmisc)) devtools::install_github("sysilviakim/Kmisc")
library(wrangleFEC)
library(Kmisc)
library(assertthat)
library(fst)
library(xtable)

options(java.parameters = "-Xmx12g", digits = 4, scipen = 100)

## C00401224 is ActBlue
## C00577130 is Bernie 2016

## Monthly graphs
fill_dem <- function(df, cand_df) {
  assert_that(nrow(df) != length(unique(df$identifier)))
  output <- left_join(df, cand_df) %>%
    mutate(
      fill = ifelse(committee_final == "C00577130", 1, 0),
      fill = ifelse(
        grepl("^C00575795$|^C00586537$|^C00619411$", committee_final), 2, fill
      ),
      fill = ifelse(!is.na(office) & office == "H", 3, fill),
      fill = ifelse(!is.na(office) & office == "S", 4, fill),
      fill = ifelse(
        grepl("^C00010603$|^C00000935$|^C00042366$", committee_final), 5, fill
      ),
      fill_label = factor(fill,
        levels = seq(0, 5),
        labels = paste0(
          "To ", 
          c("Others", "Sanders", "Clinton", "House", "Senate", "Dem. Party")
        )
      )
    ) %>%
    select(
      identifier, contribution_receipt_date,
      contribution_receipt_amount, fill, fill_label
    )
  return(output)
}

fill_congress <- function(df, congress = "house") {
  assert_that(nrow(df) != length(unique(df$identifier)))
  output <- df %>%
    mutate(
      fill = case_when(
        !!as.name(paste0(congress, "_open_out_cont")) > 0 ~ 1,
        !!as.name(paste0(congress, "_inc_out_cont")) > 0 ~ 2,
        !!as.name(paste0(congress, "_chall_out_cont")) > 0 ~ 3
      ),
      fill = ifelse(is.na(fill), 0, fill),
      fill_label = factor(
        fill,
        levels = c(0, 1, 2, 3),
        labels = c(
          ifelse(congress == "house", "In-District", "In-State"),
          "To Open Seats", "To Incumbents", "To Challengers"
        )
      )
    )
  return(output)
}

reweight100 <- function(df1, df2, var, period = "month") {
  output <- left_join(df1, df2 %>% select(!!as.name(var))) %>%
    ungroup() %>%
    group_by(!!as.name(var)) %>%
    nest() %>%
    select(data) %>%
    unlist(recursive = F) %>%
    lapply(
      ., function(x) {
        x %>%
          group_by(!!as.name(period)) %>%
          mutate(
            percent_weight =
              contribution_receipt_amount / sum(contribution_receipt_amount),
            percent_weight_count = 1 / n()
          ) %>%
          ungroup()      
      }
    )
  return(output)
}

monthly_plots <- function(df) {
  p1 <- ggplot(
    df, aes(x = month, y = 1, fill = fill_label)
  ) +
    stat_summary(fun.y = sum, geom = "bar", position = "stack") +
    scale_x_date(
      date_breaks = "3 months", labels = date_format("%b %Y"),
      limits = as.Date(c("2015-01-01", "2016-12-31")), expand = c(0, 0)
    ) +
    scale_fill_brewer(
      type = "seq", palette = "BrBG", direction = -1,
      name = "Contribution Destination"
    ) +
    xlab("Contribution Month") +
    ylab("Counts")
  p2 <- ggplot(
    df, aes(x = month, y = contribution_receipt_amount / 1e6, fill = fill_label)
  ) +
    stat_summary(fun.y = sum, geom = "bar", position = "stack") +
    scale_x_date(
      date_breaks = "3 months", labels = date_format("%b %Y"),
      limits = as.Date(c("2015-01-01", "2016-12-31")), expand = c(0, 0)
    ) +
    scale_fill_brewer(
      type = "seq", palette = "BrBG", direction = -1,
      name = "Contribution Destination"
    ) +
    xlab("Contribution Month") +
    ylab("Amount (1 million USD)")
  p3 <- ggplot(
    df, aes(x = month, y = percent_weight_count, fill = fill_label)
  ) +
    stat_summary(fun.y = sum, geom = "bar", position = "stack") +
    scale_x_date(
      date_breaks = "3 months", labels = date_format("%b %Y"),
      limits = as.Date(c("2015-01-01", "2016-12-31")), expand = c(0, 0)
    ) +
    scale_fill_brewer(
      type = "seq", palette = "BrBG", direction = -1,
      name = "Contribution Destination"
    ) +
    xlab("Contribution Month") +
    ylab("Count (%)")
  p4 <- ggplot(
    df, aes(x = month, y = percent_weight, fill = fill_label)
  ) +
    stat_summary(fun.y = sum, geom = "bar", position = "stack") +
    scale_x_date(
      date_breaks = "3 months", labels = date_format("%b %Y"),
      limits = as.Date(c("2015-01-01", "2016-12-31")), expand = c(0, 0)
    ) +
    scale_fill_brewer(
      type = "seq", palette = "BrBG", direction = -1,
      name = "Contribution Destination"
    ) +
    xlab("Contribution Month") +
    ylab("Amount (%)")
  return(list(p1, p2, p3, p4))
}

pretty_grouped_prop <- function(df, group = "bernie_vis_200", param) {
  ## Is it supplied with an extra condition?
  if (param == "gender") {
    output <- df %>% filter(gender != "either" & gender != "unknown")
  } else if (param == "race") {
    output <- df %>% filter(race != "unknown")
  } else {
    output <- df
  }
  # tidyr behavior changed with update to 1.0.0, so this does not work
  # x <- rownames(table(output[[param]]))
  # output <- output %>%
  #   group_by(!!as.name(group)) %>%
  #   nest() %>%
  #   select(data) %>%
  #   unlist(recursive = FALSE) %>%
  #   lapply(., function(x) prop.table(table(x[[param]])) * 100) %>%
  #   bind_rows(.) %>%
  #   set_colnames(
  #     ., paste0(
  #       c("Invisible", "Visible"), " at $",
  #       unlist(str_extract_all(group, "[0-9]+"))
  #     )
  #   ) %>%
  #   as.data.frame() %>%
  #   set_rownames(., x)
  # if (grepl("group", group)) colnames(output) <- levels(output[[group]])
  output <- output %>%
    group_by(!!as.name(group)) %>%
    nest() %>%
    mutate(x = map(data, ~prop.table(table(.x[[param]])) * 100)) %>%
    .[["x"]]
  return(output)
}

pretty_condprob <- function(df, A_var = "bernie_vis_200", A_val = 1,
                            B_var, B_val, output = "string") {
  B <- df %>%
    filter(!!as.name(B_var) == B_val)
  A <- df %>%
    filter((!!as.name(A_var) == A_val) & (!!as.name(B_var) == B_val))
  if (output != "string") {
    return(nrow(A) / nrow(B))
  } else {
    print(
      paste0(
        "Cond. on ", B_var, " == ", B_val, ", Pr(",
        A_var, " == ", A_val, ") is ", round(nrow(A) / nrow(B), digits = 3)
      )
    )
  }
}

geo_summ <- function(df) {
  output <- df %>% 
    summarise(
      all_donors = n(),
      vis_all = sum(bernie_vis_200),
      vis_mean = mean(bernie_vis_200),
      pop = mean(value),
      n1  = mean(n1),
      agi_avg = mean(agi_avg),
      inc_avg = mean(inc_avg),
      sum_vis_bernie_mean = 
        sum(bernie_sum * bernie_vis_200) / vis_all,
      sum_vis_all_mean = 
        sum(sum * bernie_vis_200) / vis_all,
      sum_invis_bernie_mean = 
        sum(bernie_sum * (1 - bernie_vis_200)) / (all_donors - vis_all),
      sum_invis_all_mean = 
        sum(sum * (1 - bernie_vis_200)) / (all_donors - vis_all)
    )
  return(output)
}

weekly_plots <- function(df, legend_title, labels, linetypes, colors,
                         ylab = "Frequency (1 million)", xlab = "Date",
                         date_break = "2 months") {
  p <- ggplot(
    df, aes(x = date, y = y, group = group, colour = group, linetype = group)
  ) + 
    geom_ribbon(
      aes(ymin = 0, ymax = y, fill = group), alpha = 0.5, lwd = 0.7
    ) +
    scale_linetype_manual(values = setNames(linetypes, labels)) +
    scale_fill_manual(values = setNames(colors, labels)) + 
    scale_colour_manual(values = setNames(colors, labels)) + 
    labs(
      y = ylab, x = xlab, 
      fill = legend_title, colour = legend_title, linetype = legend_title
    ) +
    guides(
      fill = guide_legend(title.position = "top", title.hjust = 0.5), 
      colour = guide_legend(title.position = "top", title.hjust = 0.5), 
      linetype = guide_legend(title.position = "top", title.hjust = 0.5)
    ) + 
    scale_x_date(
      breaks = date_break, date_labels = "%b %y"
    ) + 
    scale_y_continuous(labels = Kmisc::scaleFUN)
  return(p)
}