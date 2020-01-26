library(tidyverse)

obama_2012 <- data.frame(
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/724196/
  year_q1 = NA,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896673/
  year_q2 = 21561845.03	/ 33233681.24,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/756218/
  year_q3 = 19577222.74	/ 32156070.86	,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896693/
  year_end_1 = 17359887.97 / 31956550.43,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896750/
  year_2_jan = 5174086.51	/ 9345902.08,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896754/
  year_2_feb = 7459172.14	/ 13873901.12,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896852/
  year_2_mar = 14204648.55 / 27315287.10,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896848/
  year_2_apr = 10709295.13 / 21177041.53,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896893/
  year_2_may = 13511025.42 / 29649061.20,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896900/
  year_2_jun = 11975566.42 / 28100041.36,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/896964/
  year_2_jul = 16371561.12 / 39361275.82,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/897073/
  year_2_aug = 25072014.00 / 70726163.03,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/897086/
  year_2_sep = 33069327.61 / 96353095.86,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431445/897092/
  year_2_pre_general = 18290062.65 / 54476974.77
)

romney_2012 <- data.frame(
  ## NA
  year_q1 = NA,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/944268/
  year_q2 = 1106182.22 / 18234222.87,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/748283/
  year_q3 = 1988415.58 / 14068466.51,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/896726/
  year_end_1 = 2142900.56 / 24048673.20,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/896728/
  year_2_jan = 1172432.51	/ 6421460.29,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/780124/
  year_2_feb = 1071118.66	/ 11410593.43,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/896732/
  year_2_mar = 1669301.62 / 12592728.27,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/944275/
  year_2_apr = 2434484.77 / 11394395.03,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/944813/
  year_2_may = 4084561.69 / 15909800.79,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/800289/
  year_2_jun = 9864318.25 / 23957600.58,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/944286/
  year_2_jul = 11785492.02 / 28615640.62,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/811156/
  year_2_aug = 9449464.75 / 26852478.50,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/822044/
  year_2_sep = 12403803.52 / 41916749.21,
  ## https://docquery.fec.gov/cgi-bin/forms/C00431171/827761/
  year_2_pre_general = 11688103.20 / 38094748.78
)

trump_2016 <- data.frame(
  ## NA
  year_q1 = NA,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1015464/
  year_q2 = 39174.33 / 92249.33,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1036338/
  year_q3 = 2776974.38 / 3817978.93,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1047287/
  year_end_1 = 2102217.81	/ 2646748.68,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1051572/
  year_2_jan = 708626.35 / 941007.56,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1056932/
  year_2_feb = 1574700.47	/ 2029035.69,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1066437/
  year_2_mar = 2033139.20 / 2717878.42,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1074038/
  year_2_apr = 1279931.92 / 1716127.72,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1079423/
  year_2_may = 1975168.86 / 3104619.98,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1089674/
  year_2_jun = 12109361.92 / 19894211.40,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1291697/
  year_2_jul = 11791747.55 / 19646542.62,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1301972/
  year_2_aug = 12113417.89 / 18356758.46,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1319157/
  year_2_sep = 9722049.72 / 17354402.24,
  ## https://docquery.fec.gov/cgi-bin/forms/C00580100/1322657/
  year_2_pre_general = 8559515.44 / 13482321.68
)

clinton_2016 <- data.frame(
  ## NA
  year_q1 = NA,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1024052/
  year_q2 = 8098571.45 / 46938582.30,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1081046/
  year_q3 = 5196461.13 / 29139273.60,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1081052/
  year_end_1 = 5707930.46 / 32854373.71,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1081057/
  year_2_jan = 2740153.34	/ 13163717.03,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1093618/
  year_2_feb = 6846157.27	/ 23315741.00,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1081062/
  year_2_mar = 6097241.30 / 22359282.98,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1091718/
  year_2_apr = 5505510.01 / 20202467.46,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1091720/
  year_2_may = 4765267.30 / 19519145.98,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1099613/
  year_2_jun = 5882712.40 / 23667075.23,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1109498/
  year_2_jul = 11408608.08 / 31225016.68,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1126762/
  year_2_aug = 8463832.61 / 26898591.92,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1137625/
  year_2_sep = 12146089.39 / 41000429.86,
  ## https://docquery.fec.gov/cgi-bin/forms/C00575795/1137788/
  year_2_pre_general = 8662459.03 / 28006182.28	
)

sanders_2016 <- data.frame(
  ## NA
  year_q1 = NA,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1070785/
  year_q2 = 10465912.39 / 13745417.64,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1029414/
  year_q3 = 20187063.93 / 26208126.09,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1077572/
  year_end_1 = 21852583.81 / 33527723.10,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1077648/
  year_2_jan = 13127606.18 / 21283010.70,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1077916/
  year_2_feb = 26646653.68 / 43330679.47,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1077404/
  year_2_mar = 23970276.93 / 44745171.47,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1079445/
  year_2_apr = 10997651.28 / 26224370.99,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1094141/
  year_2_may = 5290962.61 / 15695203.38,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1126927/
  year_2_jun = 1796076.51 / 5910702.77,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1126765/
  year_2_jul = 313295.90 / 1068292.86,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1126961/
  year_2_aug = 20363.67 / 72330.55,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1157989/
  year_2_sep = 282.00 / 542.00,
  ## https://docquery.fec.gov/cgi-bin/forms/C00577130/1158036/
  year_2_pre_general = 152.00 / 302.00
)

cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73", 
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

p <- bind_rows(
  obama_2012, 
  romney_2012,
  trump_2016,
  clinton_2016,
  sanders_2016
) %>% 
  t() %>% 
  Kmisc::rowid_matrix_to_df() %>%
  filter(!is.na(V1)) %>%
  set_names(
    c("period", "Obama 2012", "Romney 2012", 
      "Trump 2016", "Clinton 2016", "Sanders 2016")
  ) %>%
  mutate(
    period = factor(
      period, levels = .$period,
      labels = c(
        "Prev.\nYear\nQ2", "Prev.\nYear\nQ3", "Prev.\nYear\nQ4",
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"
      )
    )
  ) %>%
  gather(key = "Race", value = "value", -period) %>%
  mutate(
    value = value * 100,
    Race = factor(
      Race, levels = c(
        "Obama 2012", "Romney 2012", 
        "Trump 2016", "Clinton 2016", "Sanders 2016"
      )
    )
  ) %>%
  ggplot(
    aes(x = period, y = value, group = Race, colour = Race, linetype = Race)
  ) + 
  geom_line() + 
  xlab("Reporting Period") + 
  ylab("% of Unitemized Contributions") + 
  scale_colour_manual(values = cbPalette)

pdf("figures/appF.pdf", width = 7, height = 3.5)
Kmisc::pdf_default(p)
dev.off()




