### LGBTQ Climate Survey Analysis
### August 2024 

library(tidyr)
library(dplyr)
library(stringr)
library(PNWColors) #use Bay palette
library(ordinal)
library(ggplot2)
library(forcats)
library(MASS)
library(car)

data_full <- read.csv("lgbtq_climate_data_clean_expanded_aug2024.csv") #to report individual item responses
data <- read.csv("lgbtq_climate_data_clean_aug2024.csv") #has society membership
data_lgbtq <- data_full |> filter(lgbtq2 == "yes")


### set referencdata_full### set reference groups -----
data$gender3 <- factor(data$gender3, 
                       levels = c("nonbinary", "woman",
                                  "man"))
data$gender3 <- relevel(data$gender3, ref = "man")

data$gender4 <- factor(data$gender4, 
                       levels = c("tgnc", "ciswoman",
                                  "cisman"))
data$gender4 <- relevel(data$gender4, ref = "cisman")

data$race2 <- factor(data$race2, 
                     levels = c("asian", "latinx",
                                "other", "white"))
data$race2 <- relevel(data$race2, ref = "white")

data$region <- as.factor(data$region)
data$region <- relevel(data$region, ref = "outsideUS")

data$climate_campus <- factor(data$climate_campus,
                              levels = c("very uncomfortable", "uncomfortable", 
                                         "neutral", "comfortable", 
                                         "very comfortable"))
data$climate_campus <- relevel(data$climate_campus, ref = "very uncomfortable")

data$climate_dept <- factor(data$climate_dept,
                            levels = c("very uncomfortable", "uncomfortable", 
                                       "neutral", "comfortable", 
                                       "very comfortable"))
data$climate_dept <- relevel(data$climate_dept, ref = "very uncomfortable")

data$climate_lab <- factor(data$climate_lab,
                           levels = c("very uncomfortable", "uncomfortable", 
                                      "neutral", "comfortable", 
                                      "very comfortable"))
data$climate_lab <- relevel(data$climate_lab, ref = "very uncomfortable")

data$climate_teach <- factor(data$climate_teach,
                             levels = c("very uncomfortable", "uncomfortable", 
                                        "neutral", "comfortable", 
                                        "very comfortable"))
data$climate_teach <- relevel(data$climate_teach, ref = "very uncomfortable")

data$climate_student <- factor(data$climate_student,
                               levels = c("very uncomfortable", "uncomfortable", 
                                          "neutral", "comfortable", 
                                          "very comfortable"))
data$climate_student <- relevel(data$climate_student, ref = "very uncomfortable")

### demographic table ----

demo_fxn <- function(x){
  tmp <- data.frame(data[[x]])
  demo <- merge(data.frame(table(tmp)), 
                data.frame(round((table(tmp)/sum(table(tmp)))*100, 2)), 
                "data..x..")
  colnames(demo)<-c("demo","count","perc")
  demo$`Percent (n)` <- paste0(demo$perc, " (", demo$count, ")")
  demo <- arrange(demo, desc(count))
  demo<-demo[,c("demo", "Percent (n)")]
  demo$demo <- stringr::str_to_title(demo$demo)
  demo$name <- x
  return(demo)
}

demo_table <- do.call(rbind, lapply(c("gender",
                                      "race", "lgbtq",
                                      "lgbq"),
                                    demo_fxn))

#writexl::write_xlsx(demo_table, path = "~/Desktop/ascb_climate_demo.xlsx")

###RQ1: To what extent do LGBTQ+ individuals differ from non-LGBTQ+ individuals with regard to sense of belonging and feelings of morale in their societies, workplace, and biology community and their campus/workplace climate? ----

summary(lm(formula = workplace_sob_mean ~ lgbq + genderbin + race2 + region, data = data))
summary(lm(formula = workplace_sob_mean ~ tgnc_all + race2 + region, data = data))

vif(lm(formula = workplace_sob_mean ~ lgbq + genderbin + race2 + region, data = data))
vif(lm(formula = workplace_sob_mean ~ tgnc_all + race2 + region, data = data))

### sense of belonging and feelings of morale regressions
belonging_mods <- do.call(rbind, lapply(c("workplace_sob_mean", "workplace_fom_mean",
                                          "biology_sob_mean", "biology_fom_mean"), 
                                        function(x){
                                          tmp.mod <- as.data.frame(summary(lm(formula = as.formula(paste0(
                                            x, "~ lgbq + genderbin + race2 + region")), 
                                            data = data))$coefficients[,-3])
                                          tmp.mod$outcome <- x
                                          tmp.mod$predictor <- rownames(tmp.mod)
                                          return(tmp.mod)
                                        })) 

belonging_mods2 <- do.call(rbind, lapply(c("workplace_sob_mean", "workplace_fom_mean",
                                           "biology_sob_mean", "biology_fom_mean"), 
                                         function(x){
                                           tmp.mod <- as.data.frame(summary(lm(formula = as.formula(paste0(
                                             x, "~ tgnc_all + race2 + region")), 
                                             data = data))$coefficients[,-3])
                                           tmp.mod$outcome <- x
                                           tmp.mod$predictor <- rownames(tmp.mod)
                                           return(tmp.mod)
                                         })) 

belonging_mods <- rbind(belonging_mods, belonging_mods2)
rm(belonging_mods2)

belonging_mods <- belonging_mods |>
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`)

belonging_mods |> filter(predictor != "(Intercept)") |> filter(pval<.05)


### society sense of belonging and feelings of morale - all societies together

# sense of belonging
soc_sob_df <- data |>
  pivot_longer(cols = c(ascb_sob_mean, bps_sob_mean, gsa_sob_mean, isscr_sob_mean, saber_sob_mean)) |>
  as.data.frame()


soc_sob_df$name <- factor(soc_sob_df$name, 
                          levels = c("ascb_sob_mean", "bps_sob_mean",
                                     "gsa_sob_mean", "isscr_sob_mean",
                                     "saber_sob_mean"))
soc_sob_df$name <- relevel(soc_sob_df$name, ref = "ascb_sob_mean")


summary(lm(formula = value ~ lgbq + genderbin + race2 + name + region + (1|ID), data = soc_sob_df))
summary(lm(formula = value ~ tgnc_all + race2 + name + region + (1|ID), data = soc_sob_df))

# feelings of morale
soc_fom_df <- data |>
  pivot_longer(cols = c(ascb_fom_mean, bps_fom_mean, gsa_fom_mean, isscr_fom_mean, saber_fom_mean)) |>
  as.data.frame()

soc_fom_df$name <- factor(soc_fom_df$name, 
                          levels = c("ascb_fom_mean", "bps_fom_mean",
                                     "gsa_fom_mean", "isscr_fom_mean",
                                     "saber_fom_mean"))
soc_fom_df$name <- relevel(soc_fom_df$name, ref = "ascb_fom_mean")


summary(lm(formula = value ~ lgbq + genderbin + race2 + name + region + (1|ID), data = soc_fom_df))
summary(lm(formula = value ~ tgnc_all + race2 + name + region + (1|ID), data = soc_fom_df))

### climate across contexts regressions
climate_mods <- do.call(rbind, lapply(c("climate_campus", "climate_dept", "climate_lab", "climate_teach",
                                        "climate_student"), 
                                      function(x){
                                        mod_out <- as.data.frame(summary(polr(formula = as.formula(
                                          paste0(x, " ~ lgbq + genderbin + race2 + region")),   
                                          data = data, Hess = TRUE))$coefficients)
                                        mod_out$context <- x
                                        mod_out$predictor <- rownames(mod_out)
                                        return(mod_out)
                                      }))

climate_mods2 <- do.call(rbind, lapply(c("climate_campus", "climate_dept", "climate_lab", "climate_teach",
                                         "climate_student"), 
                                       function(x){
                                         mod_out <- as.data.frame(summary(polr(formula = as.formula(
                                           paste0(x, " ~ tgnc_all + race2 + region")),   
                                           data = data, Hess = TRUE))$coefficients)
                                         mod_out$context <- x
                                         mod_out$predictor <- rownames(mod_out)
                                         return(mod_out)
                                       }))

climate_mods <- rbind(climate_mods, climate_mods2)
rm(climate_mods2)

climate_mods <- climate_mods %>%
  rename(est = Value, se = `Std. Error`, tval = `t value`) %>%
  as.data.frame()
rownames(climate_mods) <- NULL

#calculate pvals
climate_mods$pval <- pnorm(abs(climate_mods$tval), lower.tail = FALSE) *2

#calculate odds ratios
climate_mods$or <- exp(climate_mods$est)

climate_mods %>% filter(pval < .05 & predictor != "very uncomfortable|uncomfortable" &
                          predictor != "uncomfortable|neutral" & 
                          predictor != "neutral|comfortable" & 
                          predictor != "comfortable|very comfortable")


climate_mods <- climate_mods |>
  filter(predictor != "very uncomfortable|uncomfortable" &
           predictor != "uncomfortable|neutral" & 
           predictor != "neutral|comfortable" & 
           predictor != "comfortable|very comfortable")

#writexl::write_xlsx(climate_mods, path = "~/Desktop/ascb_climate_ordinal.xlsx")

###Fig 1: forest plots for belonging and comfort regressions -----

fig1a <- belonging_mods|> 
  filter(predictor == "tgnc_allyes" | predictor == "lgbqyes") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  ggplot(aes(x = est, y = predictor, color = predictor, alpha = psig)) +
  geom_point(size = 4) +
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.5, color = "grey25") + 
  geom_errorbarh(aes(xmin = est-(1.96*se), xmax = est+(1.96*se)),
                 linewidth = 1.5,
                 height = .5) +
  labs(x = "Beta ± 95% CI", y = "", title = "", color = "") + 
  scale_color_manual(breaks = c("tgnc_allyes", "lgbqyes"),
                     values = c("tgnc_allyes" = pnw_palette("Bay")[5],
                                "lgbqyes" = pnw_palette("Bay")[1]),
                     labels = c("Trans, nonbinary, genderqueer,\ngenderfluid", "Cisgender LGBQ")) +
  scale_alpha_manual(values = c(.6, 1)) +
  scale_x_continuous(limits = c(-1.7, .5), breaks = seq(-1.5, .5, by = .5)) +
  guides(alpha = "none") +
  facet_wrap(.~outcome, ncol = 1, strip.position = "left",
             labeller = labeller(outcome = c("biology_fom_mean" = "biology\nmorale",
                                             "biology_sob_mean" = "biology\nbelonging",
                                             "workplace_fom_mean" = "workplace\nmorale",
                                             "workplace_sob_mean" = "workplace\nbelonging"))) +
  theme_classic() +
  theme(legend.position = "right", 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
        legend.text = element_text(family="Helvetica", color = "black", size = 10),
        legend.title = element_text(family="Helvetica", color = "black", size = 10),
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text.y.left = element_text(family = "Helvetica", color = "black", size = 10, angle = 0, face = "bold"))

fig1a

fig1b <- climate_mods|>
  filter(predictor == "tgnc_allyes" | predictor == "lgbqyes") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  ggplot(aes(x = or, y = predictor, color = predictor, alpha = psig)) +
  geom_point(size = 4) +
  geom_vline(aes(xintercept = 1), linetype="dashed", linewidth=.5, color = "grey25") + 
  geom_errorbarh(aes(xmin = exp(est-(1.96*se)), xmax = exp(est+(1.96*se))),
                 linewidth = 1.5,
                 height = .5) +
  labs(x = "log10(Odds ratio ± 95% CI)", y = "", title = "", color = "") + 
  scale_color_manual(breaks = c("tgnc_allyes", "lgbqyes"),
                     values = c("tgnc_allyes" = pnw_palette("Bay")[5],
                                "lgbqyes" = pnw_palette("Bay")[1]),
                     labels = c("Trans, nonbinary, genderqueer,\ngenderfluid", "Cisgender LGBQ")) +
  scale_alpha_manual(values = c(.6, 1)) +
  scale_x_log10(limits = c(.07, 1.1), breaks = seq(.1, 1.3, by = .3)) +
  guides(alpha = "none") +
  facet_wrap(.~context, ncol = 1, strip.position = "left",
             labeller = labeller(context = c("climate_campus" = "campus or\ncompany",
                                             "climate_dept" = "department or\ndivision",
                                             "climate_lab" = "research lab",
                                             "climate_teach" = "classroom as\ninstructor",
                                             "climate_student" = "classroom as\nstudent"))) +
  theme_classic() +
  theme(legend.position = "right", 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
        legend.text = element_text(family="Helvetica", color = "black", size = 10),
        legend.title = element_text(family="Helvetica", color = "black", size = 10),
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text.y.left = element_text(family = "Helvetica", color = "black", size = 10, angle = 0, face = "bold"))

fig1b

###RQ2: To what extent do LGBTQ+ biologists perceive discrimination toward the LGBTQ+ community in biology?  ----
mean(data_lgbtq$lgbtq.discrim_mean)

mean(data_lgbtq[data_lgbtq$lgbq == "yes" & !is.na(data_lgbtq$lgbq),]$lgbtq.discrim_mean)
mean(data_lgbtq[data_lgbtq$tgnc == "yes" & !is.na(data_lgbtq$tgnc),]$lgbtq.discrim_mean)

###Fig 2: stacked bar graph of responses for discrimination items ----
tmp <- data.frame(table(data_lgbtq$lgbtq.discrim._1))
colnames(tmp) <- c("response", "count")
tmp$item <- "Discrimination against LGBTQ+ individuals is not a problem in the biology community"

tmp2 <- data.frame(table(data_lgbtq$lgbtq.discrim._2))
colnames(tmp2) <- c("response", "count")
tmp2$item <- "It is rare to see LGBTQ+ individuals discriminated against in the biology community"

tmp3 <- data.frame(table(data_lgbtq$lgbtq.discrim._3))
colnames(tmp3) <- c("response", "count")
tmp3$item <- "On average, people in the biology community treat LGBTQ+ individuals and non-LGBTQ+ people equally"

tmp4 <- data.frame(table(data_lgbtq$lgbtq.discrim._4))
colnames(tmp4) <- c("response", "count")
tmp4$item <- "Society has reached a point where LGBTQ+ and non-LGBTQ+ people have equal opportunities for achievement in the biology community"

rq2_graph_df <- rbind(tmp, tmp2, tmp3, tmp4)

rm(tmp, tmp2, tmp3, tmp4)

fig2 <- rq2_graph_df |>
  ggplot(aes(fill = response, y = count, x = reorder(item, count))) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "percent of LGBTQ+ participants (%, n = 486)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 7)),
                    labels = c("strongly disagree", "disagree", "somewhat disagree", 
                               "neutral",
                               "somewhat agree", "agree", "strongly agree")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.position = "bottom",
        legend.justification = "right")

fig2
###RQ3: What are LGBTQ+ biologists’ perceptions of their workplace climates? ----
mean(data_lgbtq$climate_inclusion_mean)
mean(data_lgbtq[data_lgbtq$lgbq == "yes" & !is.na(data_lgbtq$lgbq),]$climate_inclusion_mean)
mean(data_lgbtq[data_lgbtq$tgnc == "yes" & !is.na(data_lgbtq$tgnc),]$climate_inclusion_mean)

mean(data_lgbtq$climate_exclusion_mean)
mean(data_lgbtq[data_lgbtq$lgbq == "yes" & !is.na(data_lgbtq$lgbq),]$climate_exclusion_mean)
mean(data_lgbtq[data_lgbtq$tgnc == "yes" & !is.na(data_lgbtq$tgnc),]$climate_exclusion_mean)

###Fig 3: stacked bar graph of responses for LGBTQ climate items ----
##exclusion items

tmp <- data.frame(table(data_lgbtq$lgbtq.climate_2))
colnames(tmp) <- c("response", "count")
tmp$item <- "LGBTQ+ employees must be secretive"

tmp2 <- data.frame(table(data_lgbtq$lgbtq.climate_6))
colnames(tmp2) <- c("response", "count")
tmp2$item <- "The atmosphere for LGBTQ+ employees is oppressive"

tmp3 <- data.frame(table(data_lgbtq$lgbtq.climate2_2))
colnames(tmp3) <- c("response", "count")
tmp3$item <- "Coworkers make comments that seem to indicate a lack of awareness of LGBTQ+ issues"

tmp4 <- data.frame(table(data_lgbtq$lgbtq.climate2_3))
colnames(tmp4) <- c("response", "count")
tmp4$item <- "Employees are expected to not act 'too gay'"

tmp5 <- data.frame(table(data_lgbtq$lgbtq.climate2_4))
colnames(tmp5) <- c("response", "count")
tmp5$item <- "LGBTQ+ employees fear job loss because of their identity"

tmp6 <- data.frame(table(data_lgbtq$lbtq.climate3_1))
colnames(tmp6) <- c("response", "count")
tmp6$item <- "There is pressure for LGBTQ+ employees to stay closeted (to conceal their sexual orientation or gender identity/expression)"

tmp7 <- data.frame(table(data_lgbtq$lbtq.climate3_3))
colnames(tmp7) <- c("response", "count")
tmp7$item <- "LGBTQ+ employees are met with thinly veiled hostility (for example, scornful looks or icy tone of voice)"

tmp8 <- data.frame(table(data_lgbtq$lbtq.climate3_6))
colnames(tmp8) <- c("response", "count")
tmp8$item <- "LGBTQ+ people are less likely to be mentored"

rq3_exclusion <- rbind(tmp, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8)
rm(tmp, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8)

fig3b <- rq3_exclusion |>
  ggplot(aes(fill = response, y = count, x = item)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "percent of LGBTQ+ participants (%, n = 486)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 5)),
                    labels = c("strongly disagree", "disagree", 
                               "neutral",
                               "agree", "strongly agree")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.position = "bottom",
        legend.justification = "right")

fig3b

##inclusion items

tmp <- data.frame(table(data_lgbtq$lgbtq.climate_1))
colnames(tmp) <- c("response", "count")
tmp$item <- "LGBTQ+ employees are treated with respect"

tmp2 <- data.frame(table(data_lgbtq$lgbtq.climate_3))
colnames(tmp2) <- c("response", "count")
tmp2$item <- "Coworkers are as likely to ask nice, interested questions about LGBTQ+ relationships as they are about non-LGBTQ+ relationships"

tmp3 <- data.frame(table(data_lgbtq$lgbtq.climate_4))
colnames(tmp3) <- c("response", "count")
tmp3$item <- "LGBTQ+ people consider it a comfortable place to work"

tmp4 <- data.frame(table(data_lgbtq$lgbtq.climate_5))
colnames(tmp4) <- c("response", "count")
tmp4$item <- "Non-LGBTQ+ employees are comfortable engaging in LGBTQ+ friendly humor with LGBTQ+ employees (for example, kidding them about a date)"

tmp5 <- data.frame(table(data_lgbtq$lgbtq.climate2_1))
colnames(tmp5) <- c("response", "count")
tmp5$item <- "LGBTQ+ employees feel accepted by coworkers"

tmp6 <- data.frame(table(data_lgbtq$lgbtq.climate2_5))
colnames(tmp6) <- c("response", "count")
tmp6$item <- "My immediate work group is supportive of LGBTQ+ coworkers"

tmp7 <- data.frame(table(data_lgbtq$lgbtq.climate2_6))
colnames(tmp7) <- c("response", "count")
tmp7$item <- "LGBTQ+ employees are comfortable talking about their personal lives with coworkers"

tmp8 <- data.frame(table(data_lgbtq$lbtq.climate3_2))
colnames(tmp8) <- c("response", "count")
tmp8$item <- "Employee LGBTQ+ identity does not seem to be an issue"

tmp9 <- data.frame(table(data_lgbtq$lbtq.climate3_4))
colnames(tmp9) <- c("response", "count")
tmp9$item <- "The company or institution as a whole provides a supportive environment for LGBTQ+ people"

tmp10 <- data.frame(table(data_lgbtq$lbtq.climate3_5))
colnames(tmp10) <- c("response", "count")
tmp10$item <- "LGBTQ+ employees are free to be themselves"

tmp11 <- data.frame(table(data_lgbtq$lbtq.climate3_7))
colnames(tmp11) <- c("response", "count")
tmp11$item <- "LGBTQ+ employees feel free to display pictures of their partners"

tmp12 <- data.frame(table(data_lgbtq$lbtq.climate3_8))
colnames(tmp12) <- c("response", "count")
tmp12$item <- "The atmosphere for LGBTQ+ employees is improving"

rq3_inclusion <- rbind(tmp, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12)
rm(tmp, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12)

fig3a <- rq3_inclusion |>
  ggplot(aes(fill = response, y = count, x = item)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "percent of LGBTQ+ participants (%, n = 486)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 5)),
                    labels = c("strongly disagree", "disagree", 
                               "neutral",
                               "agree", "strongly agree")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.position = "bottom",
        legend.justification = "right")

fig3a

###RQ4: To what extent are LGBTQ+ biologists out to family, friends, and at work? How, if at all, does outness at work relate to their perceptions of workplace climate? ----

tmp <- data.frame(table(data_lgbtq$outness_immediatefam))
colnames(tmp) <- c("response", "count")
tmp$item <- "immediate family"

tmp2 <- data.frame(table(data_lgbtq$outness_extendedfam))
colnames(tmp2) <- c("response", "count")
tmp2$item <- "extended family"

tmp3 <- data.frame(table(data_lgbtq$outness_friends))
colnames(tmp3) <- c("response", "count")
tmp3$item <- "friends"

tmp4 <- data.frame(table(data_lgbtq$outness_coworkers))
colnames(tmp4) <- c("response", "count")
tmp4$item <- "coworkers/departmental colleagues"

tmp5 <- data.frame(table(data_lgbtq$outness_lab))
colnames(tmp5) <- c("response", "count")
tmp5$item <- "postdocs and/or graduate students in my research lab"

tmp6 <- data.frame(table(data_lgbtq$outness_gradcourse))
colnames(tmp6) <- c("response", "count")
tmp6$item <- "students in my graduate course that I teach most often "

tmp7 <- data.frame(table(data_lgbtq$outness_undergrads))
colnames(tmp7) <- c("response", "count")
tmp7$item <- "students in my undergraduate course that I teach most often "

tmp8 <- data.frame(table(data_lgbtq$outness_broadly))
colnames(tmp8) <- c("response", "count")
tmp8$item <- "the broader community/on social media "

rq4_outness <- rbind(tmp, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8)
rm(tmp, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8)

###Fig4: stacked bar graph of outness across contexts and heatmap with dept climate ----
fig4a <- rq4_outness |>
  filter(response != "") |>
  mutate(response = factor(response, levels = c("Not out", "Out to a few", "Out to some", "Out to most", "Out"))) |>
  ggplot(aes(fill = response, y = count, x = item)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "percent of LGBTQ+ participants (%)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 5)),
                    labels = c("not out", "out to a few", "out to some", "out to most", "out")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.position = "bottom",
        legend.justification = "right")

fig4a

#outness coworkers vs comfort in department heatmap
rq4_heatmap <- table(data_lgbtq$outness_coworkers, data_lgbtq$climate_dept) |> as.data.frame()
rq4_heatmap <- rq4_heatmap |> filter(Var1 != "")
colnames(rq4_heatmap) <- c("outness", "comfort", "count")
rq4_heatmap$total_comfort <- NA
rq4_heatmap[rq4_heatmap$comfort == "very comfortable",]$total_comfort <- sum(rq4_heatmap[rq4_heatmap$comfort == "very comfortable",]$count)
rq4_heatmap[rq4_heatmap$comfort == "comfortable",]$total_comfort <- sum(rq4_heatmap[rq4_heatmap$comfort == "comfortable",]$count)
rq4_heatmap[rq4_heatmap$comfort == "neutral",]$total_comfort <- sum(rq4_heatmap[rq4_heatmap$comfort == "neutral",]$count)
rq4_heatmap[rq4_heatmap$comfort == "uncomfortable",]$total_comfort <- sum(rq4_heatmap[rq4_heatmap$comfort == "uncomfortable",]$count)
rq4_heatmap[rq4_heatmap$comfort == "very uncomfortable",]$total_comfort <- sum(rq4_heatmap[rq4_heatmap$comfort == "very uncomfortable",]$count)
rq4_heatmap$total_outness <- NA
rq4_heatmap[rq4_heatmap$outness == "Not out",]$total_outness <- sum(rq4_heatmap[rq4_heatmap$outness == "Not out",]$count)
rq4_heatmap[rq4_heatmap$outness == "Out to a few",]$total_outness <- sum(rq4_heatmap[rq4_heatmap$outness == "Out to a few",]$count)
rq4_heatmap[rq4_heatmap$outness == "Out to some",]$total_outness <- sum(rq4_heatmap[rq4_heatmap$outness == "Out to some",]$count)
rq4_heatmap[rq4_heatmap$outness == "Out to most",]$total_outness <- sum(rq4_heatmap[rq4_heatmap$outness == "Out to most",]$count)
rq4_heatmap[rq4_heatmap$outness == "Out",]$total_outness <- sum(rq4_heatmap[rq4_heatmap$outness == "Out",]$count)


rq4_heatmap$pct_comfort <- rq4_heatmap$count/rq4_heatmap$total_comfort
rq4_heatmap$pct_outness <- rq4_heatmap$count/rq4_heatmap$total_outness

fig4b <- rq4_heatmap |>
  mutate(outness = factor(outness, levels = c("Not out", "Out to a few", "Out to some", "Out to most", "Out"))) |>
  mutate(comfort = factor(comfort, levels = c("very uncomfortable", "uncomfortable", "neutral", 
                                              "comfortable", "very comfortable"))) |>
  ggplot(aes(x = comfort, y = outness, fill = pct_outness)) +
  geom_tile() +
  geom_text(aes(label = count)) +
  scale_fill_gradient(low = "white", high = pnw_palette("Bay", 1)) +
  labs(y = "", x = "comfort with department climate", fill = "percent (%)") +
  coord_fixed() + 
  theme_classic() +
  theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 8, angle = 30, hjust = 1),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 8),
        text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.position = "right")

fig4b

###RQ5: To what extent have LGBTQ+ biologists experienced and/or witnessed LGBTQ+ exclusionary practices in the past year? ----

rq5_eb <- as.data.frame(matrix(data = NA, nrow = 18, ncol = 4))
colnames(rq5_eb) <- c("question", "group", "response", "count")
rq5_eb$group <- c("trans/genderqueer", "trans/genderqueer",
                  "LGBQ cis men", "LGBQ cis men",
                  "LGBQ cis women", "LGBQ cis women",
                  "trans/genderqueer", "trans/genderqueer",
                  "LGBQ cis men", "LGBQ cis men",
                  "LGBQ cis women", "LGBQ cis women",
                  "trans/genderqueer", "trans/genderqueer",
                  "LGBQ cis men", "LGBQ cis men",
                  "LGBQ cis women", "LGBQ cis women")
rq5_eb$response <- c("yes", "no", "yes", "no",
                     "yes", "no", "yes", "no",
                     "yes", "no", "yes", "no",
                     "yes", "no", "yes", "no",
                     "yes", "no")
rq5_eb[1:6,]$question <- "personally experienced exclusionary behavior at work"
rq5_eb[7:12,]$question <- "witnessed exclusionary behavior at work"
rq5_eb[13:18,]$question <- "personally experienced exclusionary behavior outside of work"

#personal_eb_work (rows 1:6)
table(data_lgbtq$personal_eb_work, data_lgbtq$tgnc)

rq5_eb[rq5_eb$group == "trans/genderqueer" & 
         rq5_eb$question == "personally experienced exclusionary behavior at work" &
         rq5_eb$response == "yes",]$count <- 55
rq5_eb[rq5_eb$group == "trans/genderqueer" & 
         rq5_eb$question == "personally experienced exclusionary behavior at work" &
         rq5_eb$response == "no",]$count <- 90


table(data_lgbtq[data_lgbtq$tgnc == "no",]$personal_eb_work, data_lgbtq[data_lgbtq$tgnc == "no",]$gender2)

rq5_eb[rq5_eb$group == "LGBQ cis men" & 
         rq5_eb$question == "personally experienced exclusionary behavior at work" &
         rq5_eb$response == "yes",]$count <- 31
rq5_eb[rq5_eb$group == "LGBQ cis men" & 
         rq5_eb$question == "personally experienced exclusionary behavior at work" &
         rq5_eb$response == "no",]$count <- 125

rq5_eb[rq5_eb$group == "LGBQ cis women" & 
         rq5_eb$question == "personally experienced exclusionary behavior at work" &
         rq5_eb$response == "yes",]$count <- 9
rq5_eb[rq5_eb$group == "LGBQ cis women" & 
         rq5_eb$question == "personally experienced exclusionary behavior at work" &
         rq5_eb$response == "no",]$count <- 144

#other_eb (rows 7:12)
table(data_lgbtq$other_eb, data_lgbtq$tgnc)

rq5_eb[rq5_eb$group == "trans/genderqueer" & 
         rq5_eb$question == "witnessed exclusionary behavior at work" &
         rq5_eb$response == "yes",]$count <- 62
rq5_eb[rq5_eb$group == "trans/genderqueer" & 
         rq5_eb$question == "witnessed exclusionary behavior at work" &
         rq5_eb$response == "no",]$count <- 83


table(data_lgbtq[data_lgbtq$tgnc == "no",]$other_eb, data_lgbtq[data_lgbtq$tgnc == "no",]$gender2)

rq5_eb[rq5_eb$group == "LGBQ cis men" & 
         rq5_eb$question == "witnessed exclusionary behavior at work" &
         rq5_eb$response == "yes",]$count <- 52
rq5_eb[rq5_eb$group == "LGBQ cis men" & 
         rq5_eb$question == "witnessed exclusionary behavior at work" &
         rq5_eb$response == "no",]$count <- 103

rq5_eb[rq5_eb$group == "LGBQ cis women" & 
         rq5_eb$question == "witnessed exclusionary behavior at work" &
         rq5_eb$response == "yes",]$count <- 56
rq5_eb[rq5_eb$group == "LGBQ cis women" & 
         rq5_eb$question == "witnessed exclusionary behavior at work" &
         rq5_eb$response == "no",]$count <- 97

#personal_eb_outside (rows 13:18)
table(data_lgbtq$personal_eb_outside, data_lgbtq$tgnc)

rq5_eb[rq5_eb$group == "trans/genderqueer" & 
         rq5_eb$question == "personally experienced exclusionary behavior outside of work" &
         rq5_eb$response == "yes",]$count <- 86
rq5_eb[rq5_eb$group == "trans/genderqueer" & 
         rq5_eb$question == "personally experienced exclusionary behavior outside of work" &
         rq5_eb$response == "no",]$count <- 62


table(data_lgbtq[data_lgbtq$tgnc == "no",]$personal_eb_outside, data_lgbtq[data_lgbtq$tgnc == "no",]$gender2)

rq5_eb[rq5_eb$group == "LGBQ cis men" & 
         rq5_eb$question == "personally experienced exclusionary behavior outside of work" &
         rq5_eb$response == "yes",]$count <- 64
rq5_eb[rq5_eb$group == "LGBQ cis men" & 
         rq5_eb$question == "personally experienced exclusionary behavior outside of work" &
         rq5_eb$response == "no",]$count <- 89

rq5_eb[rq5_eb$group == "LGBQ cis women" & 
         rq5_eb$question == "personally experienced exclusionary behavior outside of work" &
         rq5_eb$response == "yes",]$count <- 47
rq5_eb[rq5_eb$group == "LGBQ cis women" & 
         rq5_eb$question == "personally experienced exclusionary behavior outside of work" &
         rq5_eb$response == "no",]$count <- 105


###Fig 5: stacked bar graph of responses for EB by gender ----

fig5 <- rq5_eb |>
  mutate(question = factor(question, levels = c("personally experienced exclusionary behavior at work",
                                                "witnessed exclusionary behavior at work", 
                                                "personally experienced exclusionary behavior outside of work"))) |>
  ggplot(aes(fill = response, y = count, x = group)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "percent (%)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_fill_manual(values = pnw_palette("Bay", n = 2)) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap(.~question, ncol = 1) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(family="Helvetica", color = "black", size = 8, face = "bold", hjust = -.01))

fig5

