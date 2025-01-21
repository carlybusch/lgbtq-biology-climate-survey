### LGBTQ Climate Survey Analysis
### Jan 2025

library(tidyr)
library(dplyr)
library(stringr)
library(PNWColors) #use Bay palette
library(ordinal)
library(ggplot2)
library(forcats)
library(MASS)
library(car)

data_full <- read.csv("lgbtq_climate_full_deid_data.csv") #to report individual item responses
data <- read.csv("lgbtq_climate_society_data.csv") #has society membership
data_lgbtq <- read.csv("lgbtq_climate_lgbtq_only.csv") #only LGBTQ participants


### set reference groups -----
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

data_lgbtq$race2 <- factor(data_lgbtq$race2, 
                     levels = c("asian", "latinx",
                                "other", "white"))
data_lgbtq$race2 <- relevel(data_lgbtq$race2, ref = "white")

data$region <- as.factor(data$region)
data$region <- relevel(data$region, ref = "outsideUS")

data_lgbtq$region <- as.factor(data_lgbtq$region)
data_lgbtq$region <- relevel(data_lgbtq$region, ref = "outsideUS")

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

data$tgnc_cis <- NA
data[data$tgnc_all == "yes" & !is.na(data$tgnc_all),]$tgnc_cis <- "tgnc"
data[data$tgnc_all == "no" & !is.na(data$tgnc_all)
     & data$lgbtq == "No" & !is.na(data$lgbtq),]$tgnc_cis <- "cisstraight"

data_lgbtq$lgbtq_group <- NA
data_lgbtq[data_lgbtq$tgnc == "yes" & !is.na(data_lgbtq$tgnc),]$lgbtq_group <- "tgnc"
data_lgbtq[data_lgbtq$tgnc == "no" & data_lgbtq$lgbq == "yes" 
           & !is.na(data_lgbtq$lgbq) & !is.na(data_lgbtq$tgnc),]$lgbtq_group <- "cislgbq"

#faculty, lecturer/postdoc/staff, student, outside academia (gov, industry, nonprofit)
#data
{data$prof_position <- NA
data[data$job2 == "faculty" & !is.na(data$job2),]$prof_position <- "uni_faculty"

data[data$job2 == "postdoc" & !is.na(data$job2),]$prof_position <- "uni_staff"
data[data$job2 == "lecturer" & !is.na(data$job2),]$prof_position <- "uni_staff"
data[data$job2 == "staff" & !is.na(data$job2),]$prof_position <- "uni_staff"

data[data$job2 == "grad_student" & !is.na(data$job2),]$prof_position <- "uni_student"
data[data$job2 == "undergrad" & !is.na(data$job2),]$prof_position <- "uni_student"

data[data$job2 == "government" & !is.na(data$job2),]$prof_position <- "outside_acad"
data[data$job2 == "industry" & !is.na(data$job2),]$prof_position <- "outside_acad"
data[data$job2 == "nonprofit" & !is.na(data$job2),]$prof_position <- "outside_acad"
data[data$job2 == "other" & !is.na(data$job2),]$prof_position <- "outside_acad"
}

#data_lgbtq
{data_lgbtq$prof_position <- NA
data_lgbtq[data_lgbtq$job2 == "faculty" & !is.na(data_lgbtq$job2),]$prof_position <- "uni_faculty"

data_lgbtq[data_lgbtq$job2 == "postdoc" & !is.na(data_lgbtq$job2),]$prof_position <- "uni_staff"
data_lgbtq[data_lgbtq$job2 == "lecturer" & !is.na(data_lgbtq$job2),]$prof_position <- "uni_staff"
data_lgbtq[data_lgbtq$job2 == "staff" & !is.na(data_lgbtq$job2),]$prof_position <- "uni_staff"

data_lgbtq[data_lgbtq$job2 == "grad_student" & !is.na(data_lgbtq$job2),]$prof_position <- "uni_student"
data_lgbtq[data_lgbtq$job2 == "undergrad" & !is.na(data_lgbtq$job2),]$prof_position <- "uni_student"

data_lgbtq[data_lgbtq$job2 == "government" & !is.na(data_lgbtq$job2),]$prof_position <- "outside_acad"
data_lgbtq[data_lgbtq$job2 == "industry" & !is.na(data_lgbtq$job2),]$prof_position <- "outside_acad"
data_lgbtq[data_lgbtq$job2 == "nonprofit" & !is.na(data_lgbtq$job2),]$prof_position <- "outside_acad"
data_lgbtq[data_lgbtq$job2 == "other" & !is.na(data_lgbtq$job2),]$prof_position <- "outside_acad"
}

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

### LGBTQ identity counts ----
length(grep("Agender", data[!is.na(data$gender.specific),]$gender.specific))
length(grep("Gender-queer", data[!is.na(data$gender.specific),]$gender.specific))
length(grep("Questioning", data[!is.na(data$gender.specific),]$gender.specific))
length(grep("Transgender man", data[!is.na(data$gender.specific),]$gender.specific))
length(grep("Transgender woman", data[!is.na(data$gender.specific),]$gender.specific))
length(grep("A gender that is not", data[!is.na(data$gender.specific),]$gender.specific))
length(grep("Decline", data[!is.na(data$gender.specific),]$gender.specific))

length(grep("Asexual", data[!is.na(data$sexuality.specific),]$sexuality.specific))
length(grep("Bisexual", data[!is.na(data$sexuality.specific),]$sexuality.specific))
length(grep("Lesbian", data[!is.na(data$sexuality.specific),]$sexuality.specific))
length(grep("Pansexual", data[!is.na(data$sexuality.specific),]$sexuality.specific))
length(grep("Queer", data[!is.na(data$sexuality.specific),]$sexuality.specific))
length(grep("Questioning", data[!is.na(data$sexuality.specific),]$sexuality.specific))
length(grep("An identity", data[!is.na(data$sexuality.specific),]$sexuality.specific))
length(grep("Decline", data[!is.na(data$sexuality.specific),]$sexuality.specific))

###RQ1: To what extent do LGBTQ+ individuals differ from non-LGBTQ+ individuals with regard to sense of belonging and feelings of morale in their societies, workplace, and biology community and their campus/workplace climate? ----

summary(lm(formula = workplace_sob_mean ~ lgbq + genderbin + race2 + prof_position + region, data = data))
summary(lm(formula = workplace_sob_mean ~ tgnc_cis + race2 + prof_position + region, data = data))

vif(lm(formula = workplace_sob_mean ~ lgbq + genderbin + race2 + prof_position + region, data = data))
vif(lm(formula = workplace_sob_mean ~ tgnc_cis + race2 + prof_position + region, data = data))

### sense of belonging and feelings of morale regressions
belonging_mods <- do.call(rbind, lapply(c("workplace_sob_mean", "workplace_fom_mean",
                                          "biology_sob_mean", "biology_fom_mean"), 
                                        function(x){
                                          tmp.mod <- as.data.frame(summary(lm(formula = as.formula(paste0(
                                            x, "~ lgbq + genderbin + race2 + prof_position + region")), 
                                            data = data))$coefficients[,-3])
                                          tmp.mod$outcome <- x
                                          tmp.mod$predictor <- rownames(tmp.mod)
                                          return(tmp.mod)
                                        })) 

belonging_mods2 <- do.call(rbind, lapply(c("workplace_sob_mean", "workplace_fom_mean",
                                           "biology_sob_mean", "biology_fom_mean"), 
                                         function(x){
                                           tmp.mod <- as.data.frame(summary(lm(formula = as.formula(paste0(
                                             x, "~ tgnc_cis + race2 + prof_position + region")), 
                                             data = data))$coefficients[,-3])
                                           tmp.mod$outcome <- x
                                           tmp.mod$predictor <- rownames(tmp.mod)
                                           return(tmp.mod)
                                         })) 

belonging_mods$group <- "lgbq"
belonging_mods2$group <- "tgnc"

belonging_mods <- rbind(belonging_mods, belonging_mods2)
rm(belonging_mods2)

belonging_mods <- belonging_mods |>
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`)
rownames(belonging_mods) <- NULL

#belonging_mods |> filter(predictor != "(Intercept)") |> filter(pval<.05)


### society sense of belonging and feelings of morale - all societies together

# sense of belonging
soc_sob_df <- data |>
  pivot_longer(cols = c(A_sob_mean, B_sob_mean, C_sob_mean, D_sob_mean, E_sob_mean)) |>
  as.data.frame()


soc_sob_df$name <- factor(soc_sob_df$name, 
                          levels = c("A_sob_mean", "B_sob_mean",
                                     "C_sob_mean", "D_sob_mean",
                                     "E_sob_mean"))
soc_sob_df$name <- relevel(soc_sob_df$name, ref = "A_sob_mean")


summary(lm(formula = value ~ lgbq + genderbin + race2 + name + prof_position + region + (1|ID), data = soc_sob_df))
summary(lm(formula = value ~ tgnc_cis + race2 + name + prof_position + region + (1|ID), data = soc_sob_df))

# feelings of morale
soc_fom_df <- data |>
  pivot_longer(cols = c(A_fom_mean, B_fom_mean, C_fom_mean, D_fom_mean, E_fom_mean)) |>
  as.data.frame()

soc_fom_df$name <- factor(soc_fom_df$name, 
                          levels = c("A_fom_mean", "B_fom_mean",
                                     "C_fom_mean", "D_fom_mean",
                                     "E_fom_mean"))
soc_fom_df$name <- relevel(soc_fom_df$name, ref = "A_fom_mean")


summary(lm(formula = value ~ lgbq + genderbin + race2 + name + prof_position + region + (1|ID), data = soc_fom_df))
summary(lm(formula = value ~ tgnc_cis + race2 + name + prof_position + region + (1|ID), data = soc_fom_df))

#c(est, se, pval, outcome, predictor, group)
lgbq_soc_fom_output <- c(-0.500424, 0.173880, 0.00409, "soc_fom_mean", "lgbqyes", "lgbq")
tgnc_soc_fom_output <- c(-0.931143, 0.249658, 0.000204, "soc_fom_mean", "tgnc_cistgnc", "tgnc")
lgbq_soc_sob_output <- c(-0.39766, 0.20299, 0.0504, "soc_sob_mean", "lgbqyes", "lgbq")
tgnc_soc_sob_output <- c(-1.07668, 0.28939, 0.000211, "soc_sob_mean", "tgnc_cistgnc", "tgnc")
belonging_mods <- rbind(belonging_mods, lgbq_soc_sob_output, tgnc_soc_sob_output, lgbq_soc_fom_output, tgnc_soc_fom_output)
belonging_mods$est <- as.numeric(belonging_mods$est)
belonging_mods$se <- as.numeric(belonging_mods$se)
belonging_mods$pval <- as.numeric(belonging_mods$pval)

belonging_mods |> filter(pval < .05/6) #bonferroni correction for multiple hypothesis testing (6 outcomes)

### climate across contexts regressions
climate_mods <- do.call(rbind, lapply(c("climate_campus", "climate_dept", "climate_lab", "climate_teach",
                                        "climate_student"), 
                                      function(x){
                                        mod_out <- as.data.frame(summary(polr(formula = as.formula(
                                          paste0(x, " ~ lgbq + genderbin + race2 + prof_position + region")),   
                                          data = data, Hess = TRUE))$coefficients)
                                        mod_out$context <- x
                                        mod_out$predictor <- rownames(mod_out)
                                        return(mod_out)
                                      }))

climate_mods2 <- do.call(rbind, lapply(c("climate_campus", "climate_dept", "climate_lab", "climate_teach",
                                         "climate_student"), 
                                       function(x){
                                         mod_out <- as.data.frame(summary(polr(formula = as.formula(
                                           paste0(x, " ~ tgnc_cis + race2 + prof_position + region")),   
                                           data = data, Hess = TRUE))$coefficients)
                                         mod_out$context <- x
                                         mod_out$predictor <- rownames(mod_out)
                                         return(mod_out)
                                       }))

climate_mods$group <- "lgbq"
climate_mods2$group <- "tgnc"

climate_mods <- climate_mods %>%
  rename(est = Value, se = `Std. Error`, tval = `t value`) %>%
  as.data.frame()
rownames(climate_mods) <- NULL

climate_mods2 <- climate_mods2 %>%
  rename(est = Value, se = `Std. Error`, tval = `t value`) %>%
  as.data.frame()
rownames(climate_mods2) <- NULL

#calculate pvals
climate_mods$pval <- pnorm(abs(climate_mods$tval), lower.tail = FALSE) *2
climate_mods2$pval <- pnorm(abs(climate_mods2$tval), lower.tail = FALSE) *2

climate_mods <- climate_mods |>
  filter(predictor != "very uncomfortable|uncomfortable" &
           predictor != "uncomfortable|neutral" & 
           predictor != "neutral|comfortable" & 
           predictor != "comfortable|very comfortable")

climate_mods2 <- climate_mods2 |>
  filter(predictor != "very uncomfortable|uncomfortable" &
           predictor != "uncomfortable|neutral" & 
           predictor != "neutral|comfortable" & 
           predictor != "comfortable|very comfortable")

climate_mods <- rbind(climate_mods, climate_mods2)
rm(climate_mods2)

#calculate odds ratios
climate_mods$or <- exp(climate_mods$est)

climate_mods |> filter(pval < .05/5) #bonferroni correction for multiple hypothesis testing (5 contexts)

#writexl::write_xlsx(climate_mods, path = "~/Desktop/ascb_climate_ordinal.xlsx")

###Fig 1: forest plots for belonging and comfort regressions -----

fig1a <- belonging_mods|> 
  filter(predictor == "tgnc_cistgnc" | predictor == "lgbqyes") |>
  dplyr::mutate(outcome = factor(outcome, levels = c("biology_sob_mean", "biology_fom_mean",
                                               "soc_sob_mean", "soc_fom_mean",
                                               "workplace_sob_mean", "workplace_fom_mean")))|>
#  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  ggplot(aes(x = est, y = predictor, color = predictor)) +
  geom_point(size = 4) + #aes(shape = psig), 
  geom_vline(aes(xintercept = 0), linetype="dashed", linewidth=.5, color = "grey25") + 
  geom_errorbarh(aes(xmin = est-(1.96*se), xmax = est+(1.96*se)),
                 linewidth = 1.5,
                 height = .5) +
  labs(x = "Beta ± 95% CI", y = "", title = "Belonging and morale", color = "") + 
  scale_color_manual(breaks = c("tgnc_cistgnc", "lgbqyes"),
                     values = c("tgnc_cistgnc" = pnw_palette("Bay")[5],
                                "lgbqyes" = pnw_palette("Bay")[1]),
                     labels = c("TGNC", "Cisgender LGBQ")) +
#  scale_shape_manual(values = c(1,16)) +
  scale_x_continuous(limits = c(-1.7, .5), breaks = seq(-1.5, .5, by = .5)) +
#  guides(shape = "none") +
  facet_wrap(.~outcome, ncol = 1, strip.position = "left",
             labeller = labeller(outcome = c( "biology_sob_mean" = "Biology\nbelonging",
                                              "biology_fom_mean" = "Biology\nmorale",
                                             "soc_sob_mean" = "Society\nbelonging",
                                             "soc_fom_mean" = "Society\nmorale",
                                             "workplace_sob_mean" = "Workplace\nbelonging",
                                             "workplace_fom_mean" = "Workplace\nmorale"))) +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold", hjust = 0.5),
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
  filter(predictor == "tgnc_cistgnc" | predictor == "lgbqyes") |>
  mutate(psig = ifelse(pval < .05, "sig", "nonsig")) |>
  ggplot(aes(x = or, y = predictor, color = predictor)) +
  geom_point(size = 4) + #aes(shape = psig), 
  geom_vline(aes(xintercept = 1), linetype="dashed", linewidth=.5, color = "grey25") + 
  geom_errorbarh(aes(xmin = exp(est-(1.96*se)), xmax = exp(est+(1.96*se))),
                 linewidth = 1.5,
                 height = .5) +
  labs(x = "Log10(Odds ratio ± 95% CI)", y = "", title = "Comfort with climate", color = "") + 
  scale_color_manual(breaks = c("tgnc_cistgnc", "lgbqyes"),
                     values = c("tgnc_cistgnc" = pnw_palette("Bay")[5],
                                "lgbqyes" = pnw_palette("Bay")[1]),
                     labels = c("TGNC", "Cisgender\nLGBQ")) +
#  scale_shape_manual(values = c(1,16)) +
  scale_x_log10(limits = c(.13, 1.5), breaks = seq(0, 1.7, by = .5)) +
#  guides(shape = "none") +
  facet_wrap(.~context, ncol = 1, strip.position = "left",
             labeller = labeller(context = c("climate_campus" = "Campus or\ncompany",
                                             "climate_dept" = "Department or\ndivision",
                                             "climate_lab" = "Research lab",
                                             "climate_teach" = "Classroom as\ninstructor",
                                             "climate_student" = "Classroom as\nstudent"))) +
  theme_classic() +
  theme(legend.position = "right", 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
        legend.text = element_text(family="Helvetica", color = "black", size = 10),
        legend.title = element_text(family="Helvetica", color = "black", size = 10),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold", hjust = 0.5),
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text.y.left = element_text(family = "Helvetica", color = "black", size = 10, angle = 0, face = "bold"))

fig1b

###RQ2: To what extent do LGBTQ+ biologists perceive discrimination toward the LGBTQ+ community in biology?  ----
mean(data_lgbtq$lgbtq.discrim_mean)

#these do not account for missing data in the other group, so not reported
mean(data_lgbtq[data_lgbtq$lgbq == "yes" & !is.na(data_lgbtq$lgbq),]$lgbtq.discrim_mean)
mean(data_lgbtq[data_lgbtq$tgnc == "yes" & !is.na(data_lgbtq$tgnc),]$lgbtq.discrim_mean)

#these means are reported because they only include participants who have lgbq and tgnc data
mean(data_lgbtq[data_lgbtq$lgbtq_group == "tgnc" & !is.na(data_lgbtq$lgbtq_group),]$lgbtq.discrim_mean)
mean(data_lgbtq[data_lgbtq$lgbtq_group == "cislgbq" & !is.na(data_lgbtq$lgbtq_group),]$lgbtq.discrim_mean)

summary(lm(formula = lgbtq.discrim_mean ~ lgbtq_group + race2 + prof_position + region, data = data_lgbtq))

###Fig 2: stacked bar graph of responses for discrimination items ----
tmp <- data.frame(table(data_lgbtq$lgbtq.discrim._1))
colnames(tmp) <- c("response", "count")
tmp$item <- "Discrimination against LGBTQ+ individuals is not a problem"

tmp2 <- data.frame(table(data_lgbtq$lgbtq.discrim._2))
colnames(tmp2) <- c("response", "count")
tmp2$item <- "It is rare to see LGBTQ+ individuals discriminated against"

tmp3 <- data.frame(table(data_lgbtq$lgbtq.discrim._3))
colnames(tmp3) <- c("response", "count")
tmp3$item <- "On average, people treat LGBTQ+ individuals and non-LGBTQ+ people equally"

tmp4 <- data.frame(table(data_lgbtq$lgbtq.discrim._4))
colnames(tmp4) <- c("response", "count")
tmp4$item <- "Society has reached a point where LGBTQ+ and non-LGBTQ+ people have equal opportunities for achievement"

rq2_graph_df <- rbind(tmp, tmp2, tmp3, tmp4)

rm(tmp, tmp2, tmp3, tmp4)

fig2 <- rq2_graph_df |>
  ggplot(aes(fill = response, y = count, x = reorder(item, count))) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "Percent of LGBTQ+ participants (%, n = 486)", x = "", title = "Perceived discrimination in the biology community", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 7)),
                    labels = c("Strongly disagree", "Disagree", "Somewhat disagree", 
                               "Neutral",
                               "Somewhat agree", "Agree", "Strongly agree")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"),
        legend.text = element_text(family="Helvetica", color = "black", size = 8),
        plot.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "right")

fig2
###RQ3: What are LGBTQ+ biologists’ perceptions of their workplace climates? ----
mean(data_lgbtq$climate_inclusion_mean)
mean(data_lgbtq[data_lgbtq$lgbq == "yes" & !is.na(data_lgbtq$lgbq),]$climate_inclusion_mean)
mean(data_lgbtq[data_lgbtq$tgnc == "yes" & !is.na(data_lgbtq$tgnc),]$climate_inclusion_mean)

#these means are reported because they only include participants who have lgbq and tgnc data
mean(data_lgbtq[data_lgbtq$lgbtq_group == "tgnc" & !is.na(data_lgbtq$lgbtq_group),]$climate_inclusion_mean)
mean(data_lgbtq[data_lgbtq$lgbtq_group == "cislgbq" & !is.na(data_lgbtq$lgbtq_group),]$climate_inclusion_mean)

summary(lm(formula = climate_inclusion_mean ~ lgbtq_group + race2 + prof_position + region, data = data_lgbtq))

mean(data_lgbtq$climate_exclusion_mean)
mean(data_lgbtq[data_lgbtq$lgbq == "yes" & !is.na(data_lgbtq$lgbq),]$climate_exclusion_mean)
mean(data_lgbtq[data_lgbtq$tgnc == "yes" & !is.na(data_lgbtq$tgnc),]$climate_exclusion_mean)

#these means are reported because they only include participants who have lgbq and tgnc data
mean(data_lgbtq[data_lgbtq$lgbtq_group == "tgnc" & !is.na(data_lgbtq$lgbtq_group),]$climate_exclusion_mean)
mean(data_lgbtq[data_lgbtq$lgbtq_group == "cislgbq" & !is.na(data_lgbtq$lgbtq_group),]$climate_exclusion_mean)

summary(lm(formula = climate_exclusion_mean ~ lgbtq_group + race2 + prof_position + region, data = data_lgbtq))

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
  labs(y = "Percent of LGBTQ+ participants (%, n = 486)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 5)),
                    labels = c("Strongly disagree", "Disagree", 
                               "Neutral",
                               "Agree", "Strongly agree")) +
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
  labs(y = "Percent of LGBTQ+ participants (%, n = 486)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 5)),
                    labels = c("Strongly disagree", "Disagree", 
                               "Neutral",
                               "Agree", "Strongly agree")) +
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
  mutate(item = stringr::str_replace(item, "([[:alpha:]])", toupper)) |>
  ggplot(aes(fill = response, y = count, x = item)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "Percent of LGBTQ+ participants (%, n = 486)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_manual(values = rev(pnw_palette("Bay", n = 5)),
                    labels = c("Not out", "Out to a few", "Out to some", "Out to most", "Out")) +
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
  mutate(comfort = stringr::str_replace(comfort, "([[:alpha:]])", toupper)) |>
  mutate(comfort = factor(comfort, levels = c("Very uncomfortable", "Uncomfortable", "Neutral", 
                                              "Comfortable", "Very comfortable"))) |>
  ggplot(aes(x = comfort, y = outness, fill = pct_outness)) +
  geom_tile() +
  geom_text(aes(label = count)) +
  scale_fill_gradient(low = "white", high = pnw_palette("Bay", 1), labels = scales::label_percent(accuracy = 0.1)) +
  labs(y = "", x = "Comfort with department climate", fill = "Percent (%)") +
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


eb_tgnc_cislgbq <- as.data.frame(matrix(data = NA, nrow = 12, ncol = 4))
colnames(eb_tgnc_cislgbq) <- c("question", "group", "response", "count")
eb_tgnc_cislgbq$group <- c("trans/genderqueer", "trans/genderqueer",
                  "cisgender LGBQ", "cisgender LGBQ",
                  "trans/genderqueer", "trans/genderqueer",
                  "cisgender LGBQ", "cisgender LGBQ",
                  "trans/genderqueer", "trans/genderqueer",
                  "cisgender LGBQ", "cisgender LGBQ")
eb_tgnc_cislgbq$response <- c("yes", "no", "yes", "no",
                     "yes", "no", "yes", "no",
                     "yes", "no", "yes", "no")
eb_tgnc_cislgbq[1:4,]$question <- "personally experienced exclusionary behavior at work"
eb_tgnc_cislgbq[5:8,]$question <- "witnessed exclusionary behavior at work"
eb_tgnc_cislgbq[9:12,]$question <- "personally experienced exclusionary behavior outside of work"

#personal_eb_work (rows 1:4)
table(data_lgbtq$personal_eb_work, data_lgbtq$tgnc)

eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "trans/genderqueer" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "yes",]$count <- 55
eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "trans/genderqueer" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "no",]$count <- 90


table(data_lgbtq[data_lgbtq$tgnc == "no",]$personal_eb_work)

eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "cisgender LGBQ" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "yes",]$count <- 40
eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "cisgender LGBQ" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "no",]$count <- 269

#other_eb (rows 5:8)
table(data_lgbtq$other_eb, data_lgbtq$tgnc)

eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "trans/genderqueer" & 
                  eb_tgnc_cislgbq$question == "witnessed exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "yes",]$count <- 62
eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "trans/genderqueer" & 
                  eb_tgnc_cislgbq$question == "witnessed exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "no",]$count <- 83


table(data_lgbtq[data_lgbtq$tgnc == "no",]$other_eb)

eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "cisgender LGBQ" & 
                  eb_tgnc_cislgbq$question == "witnessed exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "yes",]$count <- 108
eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "cisgender LGBQ" & 
                  eb_tgnc_cislgbq$question == "witnessed exclusionary behavior at work" &
                  eb_tgnc_cislgbq$response == "no",]$count <- 200


#personal_eb_outside (rows 13:18)
table(data_lgbtq$personal_eb_outside, data_lgbtq$tgnc)

eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "trans/genderqueer" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior outside of work" &
                  eb_tgnc_cislgbq$response == "yes",]$count <- 86
eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "trans/genderqueer" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior outside of work" &
                  eb_tgnc_cislgbq$response == "no",]$count <- 62


table(data_lgbtq[data_lgbtq$tgnc == "no",]$personal_eb_outside)

eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "cisgender LGBQ" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior outside of work" &
                  eb_tgnc_cislgbq$response == "yes",]$count <- 111
eb_tgnc_cislgbq[eb_tgnc_cislgbq$group == "cisgender LGBQ" & 
                  eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior outside of work" &
                  eb_tgnc_cislgbq$response == "no",]$count <- 194

###chi square between tgnc and cis lgbq

ebwork_chisq <- reshape(eb_tgnc_cislgbq[eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior at work",],
        idvar = "group",
        v.names = "count",
        timevar = "response",
        direction = "wide")
ebwork_chisq$question <- NULL
rownames(ebwork_chisq) <- ebwork_chisq$group
ebwork_chisq$group <- NULL

print(chisq.test(ebwork_chisq))

sawwork_chisq <- reshape(eb_tgnc_cislgbq[eb_tgnc_cislgbq$question == "witnessed exclusionary behavior at work",],
                        idvar = "group",
                        v.names = "count",
                        timevar = "response",
                        direction = "wide")
sawwork_chisq$question <- NULL
rownames(sawwork_chisq) <- sawwork_chisq$group
sawwork_chisq$group <- NULL

print(chisq.test(sawwork_chisq))

ebout_chisq <- reshape(eb_tgnc_cislgbq[eb_tgnc_cislgbq$question == "personally experienced exclusionary behavior outside of work",],
                        idvar = "group",
                        v.names = "count",
                        timevar = "response",
                        direction = "wide")
ebout_chisq$question <- NULL
rownames(ebout_chisq) <- ebout_chisq$group
ebout_chisq$group <- NULL

print(chisq.test(ebout_chisq))

data_lgbtq$p_eb_work <- NA
data_lgbtq[data_lgbtq$personal_eb_work == "yes",]$p_eb_work <- 1
data_lgbtq[data_lgbtq$personal_eb_work == "no",]$p_eb_work <- 0
summary(glm(formula = p_eb_work ~ lgbtq_group + race2 + prof_position + region, family = "binomial", data = data_lgbtq))

data_lgbtq$p_eb_outside <- NA
data_lgbtq[data_lgbtq$personal_eb_outside == "yes",]$p_eb_outside <- 1
data_lgbtq[data_lgbtq$personal_eb_outside == "no",]$p_eb_outside <- 0
summary(glm(formula = p_eb_outside ~ lgbtq_group + race2 + region, family = "binomial", data = data_lgbtq))

data_lgbtq$eb_other <- NA
data_lgbtq[data_lgbtq$other_eb == "yes",]$eb_other <- 1
data_lgbtq[data_lgbtq$other_eb == "no",]$eb_other <- 0
summary(glm(formula = eb_other ~ lgbtq_group + race2 + prof_position + region, family = "binomial", data = data_lgbtq))

###Fig 5: stacked bar graph of responses for EB by gender ----

fig5 <- eb_tgnc_cislgbq |>
  mutate(group = str_replace(group, "trans/genderqueer", "TGNC")) |>
  mutate(group = str_replace(group, "cisgender LGBQ", "Cisgender LGBQ")) |>
  mutate(question = stringr::str_replace(question, "([[:alpha:]])", toupper)) |>
  mutate(question = factor(question, levels = c("Personally experienced exclusionary behavior at work",
                                                "Witnessed exclusionary behavior at work", 
                                                "Personally experienced exclusionary behavior outside of work"))) |>
  mutate(response = stringr::str_replace(response, "([[:alpha:]])", toupper)) |>
  mutate(response = factor(response, levels = c("Yes", "No"))) |>
  ggplot(aes(fill = response, y = count, x = group)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  coord_flip() +
  labs(y = "Percent (%)", x = "", fill = "") +
  scale_y_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = 0.1, suffix = "     ")) +
  scale_fill_manual(values = c(pnw_palette("Bay")[5], pnw_palette("Bay")[2])) +
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

###saving figs ----

ggsave(fig1a, device = "pdf", filename = "~/Desktop/ascb_lgbtq_oct31_fig1a.pdf", units = "cm", 
       width = 16, height = 8)
ggsave(fig1b, device = "pdf", filename = "~/Desktop/ascb_lgbtq_oct31_fig1b.pdf", units = "cm", 
       width = 16, height = 8)

fig1 <- ggpubr::ggarrange(fig1a, fig1b, nrow = 1, labels = c("A", "B"), widths = c(.75, 1))
fig1

ggsave(fig1, device = "pdf", filename = "~/Desktop/dec18_ascb_fig1.pdf", units = "cm", 
       width = 20, height = 8)
ggsave(fig1, device = "eps", filename = "~/Desktop/dec18_ascb_fig1.eps", units = "cm", 
       width = 20, height = 8)
ggsave(fig1, device = "tiff", filename = "~/Desktop/dec18_ascb_fig1.tiff", units = "cm", 
       width = 20, height = 8)

ggsave(fig2, device = "pdf", filename = "~/Desktop/dec18_ascb_fig2.pdf", units = "cm", 
       width = 20, height = 10)
ggsave(fig2, device = "eps", filename = "~/Desktop/dec18_ascb_fig2.eps", units = "cm", 
       width = 20, height = 10)
ggsave(fig2, device = "tiff", filename = "~/Desktop/dec18_ascb_fig2.tiff", units = "cm", 
       width = 20, height = 10)

ggsave(fig3a, device = "pdf", filename = "~/Desktop/ascb_lgbtq_nov22_fig3a.pdf", units = "cm", 
       width = 15, height = 15)
ggsave(fig3b, device = "pdf", filename = "~/Desktop/ascb_lgbtq_nov22_fig3b.pdf", units = "cm", 
       width = 15, height = 10)

fig3 <- ggpubr::ggarrange(fig3a, fig3b, ncol = 1, labels = c("A", "B"), common.legend = T, legend = "bottom", heights = c(1.8,1.2), font.label = list(size = 10))
fig3

ggsave(fig3, device = "pdf", filename = "~/Desktop/dec18_ascb_fig3.pdf", units = "cm", 
       width = 15, height = 20)
ggsave(fig3, device = "eps", filename = "~/Desktop/dec18_ascb_fig3.eps", units = "cm", 
       width = 15, height = 20)
ggsave(fig3, device = "tiff", filename = "~/Desktop/dec18_ascb_fig3.tiff", units = "cm", 
       width = 15, height = 20)

ggsave(fig4a, device = "pdf", filename = "~/Desktop/ascb_lgbtq_nov22_fig4a.pdf", units = "cm", 
       width = 16, height = 12)
ggsave(fig4b, device = "pdf", filename = "~/Desktop/ascb_lgbtq_nov22_fig4b.pdf", units = "cm", 
       width = 12, height = 12)

fig4 <- ggpubr::ggarrange(fig4a, fig4b, nrow = 1, labels = c("A", "B"), widths = c(1, .75))
fig4

ggsave(fig4, device = "pdf", filename = "~/Desktop/dec18_ascb_fig4.pdf", units = "cm", 
       width = 20, height = 8)
ggsave(fig4, device = "eps", filename = "~/Desktop/dec18_ascb_fig4.eps", units = "cm", 
       width = 20, height = 8)
ggsave(fig4, device = "tiff", filename = "~/Desktop/dec18_ascb_fig4.tiff", units = "cm", 
       width = 20, height = 8)

ggsave(fig5, device = "pdf", filename = "~/Desktop/dec18_ascb_fig5.pdf", units = "cm", 
       width = 20, height = 10)
ggsave(fig5, device = "eps", filename = "~/Desktop/dec18_ascb_fig5.eps", units = "cm", 
       width = 20, height = 10)
ggsave(fig5, device = "tiff", filename = "~/Desktop/dec18_ascb_fig5.tiff", units = "cm", 
       width = 20, height = 10)
