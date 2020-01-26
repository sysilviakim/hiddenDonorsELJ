# Sanity checks ================================================================
assert_that(max((main %>% filter(bernie_vis_200 == 0))$bernie_avg) <= 200)
temp <- main %>% 
  dplyr::group_by(
    bernie_vis_100, bernie_vis_200, bernie_vis_500, 
    bernie_vis_1000, bernie_vis_2700
  ) %>% 
  dplyr::summarise(
    n = n(), 
    perc = round(n() / nrow(main) * 100, digits = 1)
  )
print(xtable(temp))

# For t-tests ==================================================================
main$male <- 0
main$female <- 0
main[which(main$gender == "male"), ]$male <- 1
main[which(main$gender == "female"), ]$female <- 1
main$white <- 0
main$black <- 0
main$hispa <- 0
main$asian <- 0
main[which(main$race == "white"), ]$white <- 1
main[which(main$race == "black"), ]$black <- 1
main[which(main$race == "hispanic"), ]$hispa <- 1
main[which(main$race == "asian"), ]$asian <- 1

# Gender comparisons ===========================================================
lapply(
  c("bernie_vis_200", "vis_group_3"),
  function(x) pretty_grouped_prop(main, group = x, param = "gender")
)
# [[1]]
# Invisible at $200 Visible at $200
# female             46.78           42.83
# male               53.22           57.17
# 
# [[2]]
# female 46.78 43.79 39.31
# male   53.22 56.21 60.69

t.test(
  male ~ bernie_vis_200, data = main[which(grepl("male", main$gender)), ]
)
# Welch Two Sample t-test
# 
# data:  male by bernie_vis_200
# t = -37, df = 320000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means) is not equal to 0
# 95 percent confidence interval:
#   -0.04162 -0.03741
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.5322          0.5717 

lapply(
  c("male", "female"),
  function(x) pretty_condprob(main, B_var = "gender", B_val = x)
)
# [1] "Cond. on gender == male, Pr(bernie_vis_200 == 1) is 0.132"
# [1] "Cond. on gender == female, Pr(bernie_vis_200 == 1) is 0.115"

# Race/ethnicity comparisons ===================================================
lapply(
  c("bernie_vis_200", "vis_group_3"),
  function(x) pretty_grouped_prop(main, group = x, param = "race")
)
# [[1]]
# Invisible at $200 Visible at $200
# asian                2.888           2.528
# black                3.049           2.783
# hispanic             7.373           4.177
# white               86.691          90.513
# 
# [[2]]
# asian     2.888  2.445  2.822
# black     3.049  2.925  2.275
# hispanic  7.373  4.545  2.864
# white    86.691 90.085 92.039

t.test(
  white ~ bernie_vis_200, data = main[which(main$race != "unknown"), ]
)
# Welch Two Sample t-test
# 
# data:  white by bernie_vis_200
# t = -59, df = 340000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means) is not equal to 0
# 95 percent confidence interval:
#   -0.03949 -0.03695
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.8669          0.9051 
t.test(
  black ~ bernie_vis_200, data = main[which(main$race != "unknown"), ]
)
# Welch Two Sample t-test
# 
# data:  black by bernie_vis_200
# t = 7.4, df = 320000, p-value = 0.0000000000001
# alternative hypothesis: true difference in means) is not equal to 0
# 95 percent confidence interval:
#   0.001957 0.003360
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.03049         0.02783 
t.test(
  hispa ~ bernie_vis_200, data = main[which(main$race != "unknown"), ]
)
# Welch Two Sample t-test
# 
# data:  hispa by bernie_vis_200
# t = 71, df = 370000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means) is not equal to 0
# 95 percent confidence interval:
#   0.03107 0.03284
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.07373         0.04177 
t.test(
  asian ~ bernie_vis_200, data = main[which(main$race != "unknown"), ]
)
# Welch Two Sample t-test
# 
# data:  asian by bernie_vis_200
# t = 11, df = 330000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means) is not equal to 0
# 95 percent confidence interval:
#   0.002930 0.004273
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.02888         0.02528 


lapply(
  c("white", "black", "hispanic", "asian"),
  function(x) pretty_condprob(main, B_var = "race", B_val = x)
)
# [1] "Cond. on race == white, Pr(bernie_vis_200 == 1) is 0.129"
# [1] "Cond. on race == black, Pr(bernie_vis_200 == 1) is 0.115"
# [1] "Cond. on race == hispanic, Pr(bernie_vis_200 == 1) is 0.074"
# [1] "Cond. on race == asian, Pr(bernie_vis_200 == 1) is 0.11"

# Occupation comparison ========================================================
c("unemployed", "retired", "attorney", "teacher", "physician", 
  "student", "engineer", "consultant", "academic", "homemaker") %>%
  set_names(., .) %>%
  map(~ pretty_grouped_prop(main, group = "bernie_vis_200", param = .x))

# [[1]]
# Invisible at $200 Visible at $200
# 0             74.29           73.29
# 1             25.71           26.71
# [[2]]
# Invisible at $200 Visible at $200
# 0            98.683          96.068
# 1             1.317           3.932
# [[3]]
# Invisible at $200 Visible at $200
# 0            98.853          97.721
# 1             1.147           2.279
# [[4]]
# Invisible at $200 Visible at $200
# 0            95.068          95.924
# 1             4.932           4.076
# [[5]]
# Invisible at $200 Visible at $200
# 0           99.2005          97.896
# 1            0.7995           2.104
# [[6]]
# Invisible at $200 Visible at $200
# 0            95.981          98.792
# 1             4.019           1.208
# [[7]]
# Invisible at $200 Visible at $200
# 0             96.94          95.203
# 1              3.06           4.797
# [[8]]
# Invisible at $200 Visible at $200
# 0            98.736          98.119
# 1             1.264           1.881
# [[9]]
# Invisible at $200 Visible at $200
# 0             98.77          97.887
# 1              1.23           2.113
# [[10]]
# Invisible at $200 Visible at $200
# 0           99.7082         99.6373
# 1            0.2918          0.3627

c("unemployed", "retired", "attorney", "teacher", "physician", 
  "student", "engineer", "consultant", "academic", "homemaker") %>%
  set_names(., .) %>%
  map(~ pretty_grouped_prop(main, group = "vis_group_3", param = .x))

lapply(
  c("unemployed", "retired", "attorney", "teacher", "physician", 
    "student", "engineer", "consultant", "academic", "homemaker"),
  function(x) t.test(as.formula(paste(x, "~ bernie_vis_200")), data = main)
)
# [[1]]
# 
# Welch Two Sample t-test
# 
# data:  unemployed by bernie_vis_200
# t = -11, df = 320000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.011834 -0.008136
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.2571          0.2671 
# 
# 
# [[2]]
# 
# Welch Two Sample t-test
# 
# data:  retired by bernie_vis_200
# t = -66, df = 280000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.02693 -0.02537
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.01317         0.03932 
# 
# 
# [[3]]
# 
# Welch Two Sample t-test
# 
# data:  attorney by bernie_vis_200
# t = -37, df = 290000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01192 -0.01071
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.01147         0.02279 
# 
# 
# [[4]]
# 
# Welch Two Sample t-test
# 
# data:  teacher by bernie_vis_200
# t = 20, df = 340000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.007719 0.009394
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.04932         0.04076 
# 
# 
# [[5]]
# 
# Welch Two Sample t-test
# 
# data:  physician by bernie_vis_200
# t = -44, df = 280000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01362 -0.01247
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.007995        0.021038 
# 
# 
# [[6]]
# 
# Welch Two Sample t-test
# 
# data:  student by bernie_vis_200
# t = 110, df = 520000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.02759 0.02863
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.04019         0.01208 
# 
# 
# [[7]]
# 
# Welch Two Sample t-test
# 
# data:  engineer by bernie_vis_200
# t = -39, df = 300000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01825 -0.01650
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.03060         0.04797 
# 
# 
# [[8]]
# 
# Welch Two Sample t-test
# 
# data:  consultant by bernie_vis_200
# t = -22, df = 300000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.006731 -0.005617
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.01264         0.01881 
# 
# 
# [[9]]
# 
# Welch Two Sample t-test
# 
# data:  academic by bernie_vis_200
# t = -30, df = 290000, p-value <0.0000000000000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.009417 -0.008245
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.01230         0.02113 
# 
# 
# [[10]]
# 
# Welch Two Sample t-test
# 
# data:  homemaker by bernie_vis_200
# t = -5.6, df = 310000, p-value = 0.00000002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0009574 -0.0004603
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.002918        0.003627

lapply(
  names(main)[63:107],
  function(x) pretty_condprob(main, B_var = x, B_val = 1)
)
# [1] "Cond. on unemployed == 1, Pr(bernie_vis_200 == 1) is 0.128"
# [1] "Cond. on retired == 1, Pr(bernie_vis_200 == 1) is 0.297"
# [1] "Cond. on self_employed == 1, Pr(bernie_vis_200 == 1) is 0.15"
# [1] "Cond. on attorney == 1, Pr(bernie_vis_200 == 1) is 0.22"
# [1] "Cond. on teacher == 1, Pr(bernie_vis_200 == 1) is 0.105"
# [1] "Cond. on student == 1, Pr(bernie_vis_200 == 1) is 0.041"
# [1] "Cond. on engineer == 1, Pr(bernie_vis_200 == 1) is 0.182"
# [1] "Cond. on consultant == 1, Pr(bernie_vis_200 == 1) is 0.174"
# [1] "Cond. on academic == 1, Pr(bernie_vis_200 == 1) is 0.196"
# [1] "Cond. on homemaker == 1, Pr(bernie_vis_200 == 1) is 0.15"
# [1] "Cond. on manager == 1, Pr(bernie_vis_200 == 1) is 0.109"
# [1] "Cond. on executive == 1, Pr(bernie_vis_200 == 1) is 0.16"
# [1] "Cond. on salesman == 1, Pr(bernie_vis_200 == 1) is 0.082"
# [1] "Cond. on physician == 1, Pr(bernie_vis_200 == 1) is 0.272"

options(digits = 5, scipen = 999)
main %>% 
  group_by(bernie_vis_200) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_sum, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# A tibble: 2 x 5
# bernie_vis_200 data                       `25%` `50%` `75%`
#            <dbl> <list>                     <dbl> <dbl> <dbl>
# 1              0 <tibble [1,767,286 x 156]>    15    27    60
# 2              1 <tibble [250,352 x 156]>     270   400   700

main %>% 
  group_by(bernie_vis_200) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_sum_neg, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# A tibble: 2 x 5
# bernie_vis_200 data                       `25%` `50%` `75%`
# <dbl> <list>                     <dbl> <dbl> <dbl>
# 1              0 <tibble [1,767,286 x 156]>     0   0      5 
# 2              1 <tibble [250,352 x 156]>       2  31.8  175.

main %>% 
  group_by(bernie_vis_200) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_freq, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# A tibble: 2 x 5
# bernie_vis_200 data                       `25%` `50%` `75%`
# <dbl> <list>                     <dbl> <dbl> <dbl>
#   1              0 <tibble [1,767,286 × 129]>     1     1     3
#   2              1 <tibble [250,352 × 129]>       5    10    17

main %>% 
  group_by(bernie_vis_200) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_freq_neg, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# A tibble: 2 x 5
#   bernie_vis_200 data                       `25%` `50%` `75%`
# <dbl> <list>                     <dbl> <dbl> <dbl>
#   1              0 <tibble [1,767,286 x 101]>     0     0     2
#   2              1 <tibble [250,352 x 101]>       1     4    15

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_sum, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# # A tibble: 3 x 5
# vis_group_3        data                       `25%` `50%` `75%`
# <fct>              <list>                     <dbl> <dbl> <dbl>
#   1 Invisible at $200  <tibble [1,767,286 × 129]>   15    28     60
#   2 Visible, Crosser   <tibble [195,791 × 129]>    258.  350    550
#   3 Visible, Immediate <tibble [54,561 × 129]>     275   481.   857

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_sum_neg, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# # A tibble: 3 x 5
# vis_group_3        data                       `25%` `50%` `75%`
# <fct>              <list>                     <dbl> <dbl> <dbl>
#   1 Invisible at $200  <tibble [1,767,286 × 129]>  0     0       5
#   2 Visible, Crosser   <tibble [195,791 × 129]>    7.40 40     176.
#   3 Visible, Immediate <tibble [54,561 × 129]>     0     2.70  252

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_freq, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# # A tibble: 3 x 5
# vis_group_3        data                       `25%` `50%` `75%`
# <fct>              <list>                     <dbl> <dbl> <dbl>
#   1 Invisible at $200  <tibble [1,767,286 × 129]>     1     1     3
#   2 Visible, Crosser   <tibble [195,791 × 129]>       7    11    19
#   3 Visible, Immediate <tibble [54,561 × 129]>        1     3     7

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_freq_neg, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# # A tibble: 3 x 5
# vis_group_3        data                       `25%` `50%` `75%`
# <fct>              <list>                     <dbl> <dbl> <dbl>
#   1 Invisible at $200  <tibble [1,767,286 × 129]>     0     0     2
#   2 Visible, Crosser   <tibble [195,791 × 129]>       1     6    18
#   3 Visible, Immediate <tibble [54,561 × 129]>        0     1     4

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_avg, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# # A tibble: 3 x 5
# vis_group_3        data                       `25%` `50%` `75%`
# <fct>              <list>                     <dbl> <dbl> <dbl>
#   1 Invisible at $200  <tibble [1,767,286 × 129]>  10    15    27
#   2 Visible, Crosser   <tibble [195,791 × 129]>    21.1  32.7  52.4
#   3 Visible, Immediate <tibble [54,561 × 129]>     50.1 128.  250

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$bernie_avg_neg, probs = c(0.25, 0.5, 0.75))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)

main %>% 
  group_by(bernie_vis_200) %>% 
  summarise(
    mean(bernie_sum),
    mean(bernie_sum_neg),
    mean(bernie_freq),
    mean(bernie_freq_neg)
  ) %>% print(width = 1e6)
# A tibble: 2 x 5
# bernie_vis_200 `mean(bernie_sum)` `mean(bernie_sum_neg)` `mean(bernie_freq)`
# <dbl>              <dbl>                  <dbl>               <dbl>
#   1              0               46.1                   57.8                2.55
#   2              1              549.                   523.                13.2
# `mean(bernie_freq_neg)`
# <dbl>
#   1                    3.81
#   2                   19.1

main %>% 
  group_by(vis_group_3) %>% 
  summarise(
    mean(bernie_sum),
    mean(bernie_sum_neg),
    mean(bernie_freq),
    mean(bernie_freq_neg),
    mean(bernie_avg),
    mean(bernie_avg_neg)
  ) %>% print(width = 1e6)
# A tibble: 3 x 7
# vis_group_3        `mean(bernie_sum)` `mean(bernie_sum_neg)` `mean(bernie_freq)`
# <fct>                           <dbl>                  <dbl>               <dbl>
#   1 Invisible at $200                46.1                   57.8                2.55
# 2 Visible, Crosser                494.                   350.                15.1 
# 3 Visible, Immediate              745.                  1143.                 6.25
# `mean(bernie_freq_neg)` `mean(bernie_avg)` `mean(bernie_avg_neg)`
# <dbl>              <dbl>                  <dbl>
#   1                    3.81               22.5                  0.485
#   2                   21.2                43.4                 -8.67 
#   3                   11.6               233.                  49.8

## Sum truncation ==============================================================
p <- ggplot(
  main,
  aes(
    x = bernie_sum, group = factor(bernie_vis_200), 
    colour = factor(bernie_vis_200), fill = factor(bernie_vis_200)
  )
) + 
  stat_density(aes(y = ..count..),  trim = TRUE) + 
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 50)) + 
  scale_fill_manual(
    labels = c("Hidden", "Visible"), values = c('#bf812d', '#01665e')
  ) + 
  scale_colour_manual(
    labels = c("Hidden", "Visible"), values = c('#543005', '#003c30')
  ) + 
  labs(
    y = "Frequency", x ="Contribution Sum to Sanders (USD)",
    fill = "Donor Visibility", group = "Donor Visibility",
    colour = "Donor Visibility"
  )
  
pdf("./figures/sum_trunc.pdf", width = 7, height = 5)
Kmisc::pdf_default(p)
dev.off()

main %>% 
  group_by(bernie_vis_200) %>% 
  nest() %>% 
  mutate(
    ret = map(
      data, 
      ~as.Date(
        quantile(as.numeric(.$bernie_entry_date), probs = c(0.25, 0.5, 0.75)
      ), origin = "1970-01-01")
    ),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# bernie_vis_200 data                       `25%`      `50%`      `75%`     
# <dbl> <list>                     <date>     <date>     <date>    
#   1              0 <tibble [1,767,286 x 101]> 2015-10-01 2016-01-29 2016-03-06
#   2              1 <tibble [250,352 x 101]>   2015-06-30 2015-10-04 2016-01-28

main %>% 
  group_by(bernie_vis_200) %>% 
  nest() %>% 
  mutate(
    ret = map(
      data, 
      ~as.Date(
        quantile(as.numeric(.$bernie_exit_date), probs = c(0.25, 0.5, 0.75)
        ), origin = "1970-01-01")
    ),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# bernie_vis_200 data                       `25%`      `50%`      `75%`     
# <dbl> <list>                     <date>     <date>     <date>    
#   1              0 <tibble [1,767,286 x 101]> 2016-01-21 2016-03-09 2016-04-08
#   2              1 <tibble [250,352 x 101]>   2016-04-03 2016-05-18 2016-06-11

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(
      data, 
      ~as.Date(
        quantile(as.numeric(.$bernie_entry_date), probs = c(0.25, 0.5, 0.75)
        ), origin = "1970-01-01")
    ),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# # A tibble: 3 x 5
# vis_group_3        data                       `25%`      `50%`      `75%`
# <fct>              <list>                     <date>     <date>     <date>
#   1 Invisible at $200  <tibble [1,767,286 × 129]> 2015-10-01 2016-01-29 2016-03-06
#   2 Visible, Crosser   <tibble [195,791 × 129]>   2015-06-23 2015-09-23 2016-01-01
#   3 Visible, Immediate <tibble [54,561 × 129]>    2015-09-12 2016-01-25 2016-03-09

main %>% 
  group_by(vis_group_3) %>% 
  nest() %>% 
  mutate(
    ret = map(
      data, 
      ~as.Date(
        quantile(as.numeric(.$bernie_exit_date), probs = c(0.25, 0.5, 0.75)
        ), origin = "1970-01-01")
    ),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret)
# # A tibble: 3 x 5
# vis_group_3        data                       `25%`      `50%`      `75%`
# <fct>              <list>                     <date>     <date>     <date>
#   1 Invisible at $200  <tibble [1,767,286 × 129]> 2016-01-21 2016-03-09 2016-04-08
#   2 Visible, Crosser   <tibble [195,791 × 129]>   2016-04-15 2016-05-25 2016-06-19
#   3 Visible, Immediate <tibble [54,561 × 129]>    2016-02-10 2016-04-03 2016-05-25

unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$bernie_vis_200 == 1), ]$bernie_exit_date > x
  ) / nrow(main[which(main$bernie_vis_200 == 1), ]) * 100
))

unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$bernie_vis_200 == 0), ]$bernie_exit_date > x
  ) / nrow(main[which(main$bernie_vis_200 == 0), ]) * 100
))

unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$bernie_vis_200 == 1), ]$exit_date > x
  ) / nrow(main[which(main$bernie_vis_200 == 1), ]) * 100
))

unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$bernie_vis_200 == 0), ]$exit_date > x
  ) / nrow(main[which(main$bernie_vis_200 == 0), ]) * 100
))

## Setup for drawing ===========================================================
load("./output/setup.Rda")
raw_df <- raw_df[
  which(
    raw_df$committee_final == "C00577130" & 
      raw_df$contribution_receipt_date < as.Date("2016-08-01")
  ),
]
raw_df <- left_join(raw_df, main[, c("bernie_vis_200", "identifier")])
raw_df$bernie_vis_200 <- factor(raw_df$bernie_vis_200)
raw_df$money_27 <- if_else(raw_df$contribution_receipt_amount == 27, 1, 0)
raw_df$group <- with(raw_df, interaction(bernie_vis_200, money_27))
raw_df$date <- cut(raw_df$contribution_receipt_date, "week")
raw_df$date <- as.Date(raw_df$date)

library(data.table)
raw_df <- data.table::data.table(raw_df)
data.table::setkey(raw_df, contribution_receipt_amount)
test_sanders <- raw_df[, c("cont_sum", "cont_count") := {
  tmp1 = sum(contribution_receipt_amount) / 1e6
  tmp2 = .N
  list(tmp1, tmp2)
}, by = c("date", "group")][, head(.SD, 1), keyby = .(date, group)]

test_sanders$group <- factor(
  test_sanders$group, 
  levels = c("0.0", "1.0", "0.1", "1.1"),
  labels = c(
    "Hidden, Other Amount", "Visible, Other Amount", 
    "Hidden, $27", "Visible, $27"
  )
)

leg_label <- 
  "Contribution Frequencies and Sum to Sanders, by Donor Type x Amount"

hidden_light <- '#bf812d'
visible_light <- '#80cdc1'
hidden_dark <- '#543005'
visible_dark <- '#01665e'

## Plot Sanders timing, distinguishing 27 --------------------------------------
p1 <- weekly_plots(
  df = test_sanders %>% 
    dplyr::mutate(
      y = cont_count / 1e6,
      group = factor(group, levels = c(
        "Hidden, Other Amount", "Visible, Other Amount", 
        "Hidden, $27", "Visible, $27"
      ))
    ), 
  labels = c(
    "Visible, Other Amount", "Hidden, Other Amount",
    "Visible, $27", "Hidden, $27"
  ), 
  linetypes = c("dotted", "twodash", "dashed", "solid"), 
  colors = c(visible_light, hidden_light, visible_dark, hidden_dark), 
  ylab = "Frequency (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type x Amount"
)

p2 <- weekly_plots(
  df = test_sanders %>% 
    dplyr::mutate(
      y = cont_sum,
      group = factor(group, levels = c(
        "Visible, Other Amount", "Hidden, Other Amount", 
        "Hidden, $27", "Visible, $27"
      ))
    ), 
  labels = c(
    "Visible, Other Amount", "Hidden, Other Amount",
    "Visible, $27", "Hidden, $27"
  ), 
  linetypes = c("dotted", "twodash", "dashed", "solid"), 
  colors = c(visible_light, hidden_light, visible_dark, hidden_dark), 
  ylab = "Sum (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type x Amount"
)

p3 <- p2
p3$data$group <- factor(p3$data$group, levels = c(
  "Hidden, $27", "Visible, $27",
  "Hidden, Other Amount", "Visible, Other Amount"
))

p <- Kmisc::grid_arrange_shared_legend(
  Kmisc::pdf_default(p1) + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()
    ),
  Kmisc::pdf_default(p2) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ncol = 1, nrow = 2
)
pdf("./figures/sanders_timing.pdf", width = 6, height = 6)
gridExtra::grid.arrange(p)
dev.off()

## Setup for drawing --------------------------------------
raw_df$bernie_cont <- factor(raw_df$bernie_cont)
raw_df$group <- with(raw_df, interaction(bernie_vis_200, bernie_cont))

test <- raw_df[, c("cont_sum", "cont_count") := {
  tmp1 = sum(contribution_receipt_amount) / 1e6
  tmp2 = .N
  list(tmp1, tmp2)
}, by = c("date", "group")][, head(.SD, 1), keyby = .(date, group)]

test$group <- factor(
  test$group, 
  levels = c("1.0", "0.0", "1.1", "0.1"),
  labels = c(
    "Visible, To Others", "Hidden, To Others", 
    "Visible, To Sanders", "Hidden, To Sanders"
  )
)

## Plot all timing, distinguishing destination ---------------------------------
p1 <- weekly_plots(
  df = test %>% 
    dplyr::mutate(
      y = cont_count / 1e6,
      group = factor(group, levels = c(
        "Hidden, To Others", "Visible, To Others", 
        "Hidden, To Sanders", "Visible, To Sanders"
      ))
    ), 
  labels = c(
    "Visible, To Others", "Hidden, To Others", 
    "Visible, To Sanders", "Hidden, To Sanders"
  ), 
  linetypes = c("dotted", "twodash", "dashed", "solid"), 
  colors = c(visible_light, hidden_light, visible_dark, hidden_dark), 
  ylab = "Frequency (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type x Destination"
)

p2 <- weekly_plots(
  df = test %>% 
    dplyr::mutate(
      y = cont_sum,
      group = factor(group, levels = c(
        "Visible, To Others", "Hidden, To Others", 
        "Visible, To Sanders", "Hidden, To Sanders"
      ))
    ), 
  labels = c(
    "Visible, To Others", "Hidden, To Others", 
    "Visible, To Sanders", "Hidden, To Sanders"
  ), 
  linetypes = c("dotted", "twodash", "dashed", "solid"), 
  colors = c(visible_light, hidden_light, visible_dark, hidden_dark), 
  ylab = "Sum (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type x Destination"
)

p3 <- p2
p3$data$group <- factor(p3$data$group, levels = c(
  "Hidden, To Sanders", "Visible, To Sanders",
  "Hidden, To Others", "Visible, To Others"
))
p <- grid_arrange_shared_legend(
  Kmisc::pdf_default(p1) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()
    ),
  Kmisc::pdf_default(p2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ncol = 1, nrow = 2, legend_plot = Kmisc::pdf_default(p3)
)

# http://colorbrewer2.org/#type=sequential&scheme=OrRd&n=9
pdf("./figures/all_timing.pdf", width = 6, height = 6)
gridExtra::grid.arrange(p)
dev.off()

# round(prop.table(table(main$vis_group_3)) * 100, digits = 1)
# vis_group_3 ------------------------------------------------------------------
raw_df <- left_join(raw_df, main[, c("vis_group_3", "identifier")])
raw_df <- data.table::data.table(raw_df)
test_vis <- raw_df[raw_df$bernie_cont == 1, ][, c("cont_sum", "cont_count") := {
  tmp1 = sum(contribution_receipt_amount) / 1e6
  tmp2 = .N
  list(tmp1, tmp2)
}, by = c("date", "vis_group_3")][, head(.SD, 1), keyby = .(date, vis_group_3)]
test_vis$vis_group_3 <- factor(
  test_vis$vis_group_3,
  levels = c("Invisible at $200", "Visible, Crosser", "Visible, Immediate"),
  labels = c("Hidden", "Eventually Visible", "Immediately Visible")
)

p1 <- weekly_plots(
  df = test_vis %>% dplyr::mutate(y = cont_sum, group = vis_group_3), 
  labels = levels(test_vis$vis_group_3), 
  linetypes = c("solid", "twodash", "dashed"), 
  colors = c('#fc9272', '#ef3b2c', '#99000d'), 
  ylab = "Sum (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type"
)

p2 <- weekly_plots(
  df = test_vis %>% dplyr::mutate(y = cont_count / 1e6, group = vis_group_3), 
  labels = levels(test_vis$vis_group_3), 
  linetypes = c("solid", "twodash", "dashed"), 
  colors = c('#fc9272', '#ef3b2c', '#99000d'), 
  ylab = "Frequency (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type"
)

p <- Kmisc::grid_arrange_shared_legend(
  Kmisc::pdf_default(p2) + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()
    ),
  Kmisc::pdf_default(p1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ncol = 1, nrow = 2
)

pdf("./figures/vis_timing.pdf", width = 6, height = 6)
gridExtra::grid.arrange(p)
dev.off()

raw_df$group <- with(raw_df, interaction(vis_group_3, bernie_cont))
test_vis_six <- raw_df[, c("cont_sum", "cont_count") := {
  tmp1 = sum(contribution_receipt_amount) / 1e6
  tmp2 = .N
  list(tmp1, tmp2)
}, by = c("date", "group")][, head(.SD, 1), keyby = .(date, group)]
test_vis_six$group <- factor(
  test_vis_six$group,
  levels = c(
    "Invisible at $200.0", "Visible, Crosser.0", "Visible, Immediate.0",
    "Invisible at $200.1", "Visible, Crosser.1", "Visible, Immediate.1"
  ),
  labels = c(
    "Hidden, To Others", "Eventually Visible, To Others", 
    "Immediately Visible, To Others", "Hidden, To Sanders",
    "Eventually Visible, To Sanders", "Immediately Visible, To Sanders"
  )
)

p1 <- weekly_plots(
  df = test_vis_six %>% dplyr::mutate(y = cont_sum, group = group), 
  labels = c(
    "Hidden, To Others", "Eventually Visible, To Others", 
    "Immediately Visible, To Others", "Hidden, To Sanders",
    "Eventually Visible, To Sanders", "Immediately Visible, To Sanders"
  ), 
  linetypes = rev(c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash')), 
  colors = c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15'), 
  ylab = "Sum (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type"
)

p2 <- weekly_plots(
  df = test_vis_six %>% dplyr::mutate(y = cont_count / 1e6, group = group), 
  labels = c(
    "Hidden, To Others", "Eventually Visible, To Others", 
    "Immediately Visible, To Others", "Hidden, To Sanders",
    "Eventually Visible, To Sanders", "Immediately Visible, To Sanders"
  ), 
  linetypes = rev(c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash')), 
  colors = c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15'), 
  ylab = "Frequency (1 million USD)", 
  legend_title = "Contribution Frequencies and Sum, by Donor Type"
)

## Dates for vis_group_3 -------------------------------------------------------
test_vis %>% 
  dplyr::group_by(vis_group_3) %>% 
  summarise(sum(cont_sum), sum(cont_count) / 1e6)

main$vis_group_3 <- as.numeric(main$vis_group_3)
unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$vis_group_3 == 1), ]$bernie_exit_date > x
  ) / nrow(main[which(main$vis_group_3 == 1), ]) * 100
))
# [1] 61.5330 38.7016 15.6944  3.0205  0.0788

unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$vis_group_3 == 2), ]$bernie_exit_date > x
  ) / nrow(main[which(main$vis_group_3 == 2), ]) * 100
))
# [1] 89.6364 70.1156 33.3478  6.2352  0.0439

unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$vis_group_3 == 1), ]$exit_date > x
  ) / nrow(main[which(main$vis_group_3 == 1), ]) * 100
))

unlist(lapply(
  as.Date(
    c("2016-03-15", "2016-04-26", "2016-06-06", "2016-07-12", "2016-07-28")
  ),
  function(x) sum(
    main[which(main$vis_group_3 == 2), ]$exit_date > x
  ) / nrow(main[which(main$vis_group_3 == 2), ]) * 100
))


table(main[which(main$bernie_sum >= 2700), ]$g3)
