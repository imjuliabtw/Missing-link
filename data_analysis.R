library(ggplot2)
library(kableExtra)
library(knitr)
library(pander)
library(tidyverse)
library(dplyr)
library(knitr)
library(afex)
library(emmeans)
library(rempsyc)
library(flextable)
library(broom)
library(report)
library(effectsize)
library(WRS2)

#reading the dataset with all results
exp <- read.csv("dataexp.csv")

#preparing datasets for counting the time spent in the experiment
poczatek <- read.csv('consent.csv')
koniec <- read.csv("demografia.csv")

#checking the starting time of each participant
start <- poczatek %>% 
  filter(Response == "BEGIN",
        !is.na(UTC.Timestamp)) %>% 
  select(Participant.Private.ID,
         start_time = UTC.Timestamp)

#checking the ending time of each participant
end <- koniec %>% 
  filter(Response == "END",
         !is.na(UTC.Timestamp)) %>% 
  select(Participant.Private.ID,
         end_time = UTC.Timestamp)

#total time spent in the experiment
total_time <- start %>% 
  inner_join(end, by="Participant.Private.ID") %>% 
  mutate(czas_min = (end_time - start_time) /1000 /60)

total_time %>% 
  select(Participant.Private.ID,
         start_time,
         end_time, 
         czas_min)

#table with statistics about the time spent in the experiment
total_time %>% 
  summarise(N = sum(!is.na(czas_min)),
            M = mean(czas_min, na.rm = TRUE),
            SD = sd(czas_min, na.rm = TRUE))


#deleting unnecessary columns from the dataset, for easier analysis
for_delete2 <- c("Event.Index", "UTC.Timestamp", "UTC.Date.and.Time",
                "Local.Timestamp", "Local.Timezone", "Local.Date.and.Time",
                "Experiment.ID", "Experiment.Version", "Tree.Node.Key", "Repeat.Key",
                "Schedule.ID", "Participant.Public.ID", "Participant.Starting.Group",
                "Participant.Completion.Code", "Participant.External.Session.ID", "Participant.Device.Type",
                "Participant.Device", "Participant.OS", "Participant.Browser",
                "Participant.Monitor.Size", "Participant.Viewport.Size", "Checkpoint",
                "Room.ID", "Room.Order", "Task.Version", "branch.ovqe", "Participant.Status",
                "consent.Response", "consent.Quantised")


#deleting the unnecessary columns and empty rows
exp1 <- exp[ , !(names(exp) %in% for_delete2)]
exp2 <- exp1[!is.na(exp$Participant.Private.ID), ]

#leaving only quantised column of each question
exp_quantised <- exp2 %>% 
  select(Participant.Private.ID,
         Task.Name, randomiser.9dvt,
         contains(".Quantised"))


#creating dataset with one row corresponding to one participant
exp_one <- exp_quantised %>%
  group_by(Participant.Private.ID) %>%
  summarise(
    randomiser.9dvt = first(randomiser.9dvt),
    across(
      matches("Quantised"),
      ~ {
        x <- .[!is.na(.) & . != ""]
        if(length(x) > 0) x[1] else NA
      }
    ),
    
    .groups = "drop"
  )


#function for changing the formatting of question columns
for(i in 1:15) {
  col1 <- paste0("X", i, "A0B0_1.Quantised")
  col2 <- paste0("X", i, "A0B0_2.Quantised")
  
  variants1 <-c(
    col1,
    paste0(col1, ".1"),
    paste0(col1, ".2"),
    paste0(col1, ".3")
  )
  
  variants2 <-c(
    col2,
    paste0(col2, ".1"),
    paste0(col2, ".2"),
    paste0(col2, ".3")
  )
  
  exp_one[[paste0("X", i, "_1")]] <-
    dplyr::coalesce(!!!exp_one[variants1])
  
  exp_one[[paste0("X", i, "_2")]] <-
    dplyr::coalesce(!!!exp_one[variants2])
  
}

#the final dataset in which one row correspond to one respondend and there is 15 columns for each experimental question
exp_final <- exp_one %>% 
  select(Participant.Private.ID,
         randomiser.9dvt,
         X1_1:X15_2,
         controllack.Quantised,
         controlwith.Quantised,
         sex.Quantised,
         edu.Quantised,
         polish.Quantised,
         logic.Quantised)

#creating a column to note the type of sentence condition
exp_final2 <- exp_final%>% 
  mutate(sentence_type = if_else(
    grepl("And", randomiser.9dvt),
    1, 0))

#creating a column to note the condition of the modifier
exp_final3 <- exp_final2 %>% 
  mutate(modifier = if_else(
    grepl("Lack", randomiser.9dvt),
    0, 1))
))

#excluding participants based on the declared level of Polish
exp_final3_excl_lang <- exp_final3 %>% 
  filter(polish.Quantised %in% c("1", "2"))

#excluding participants 
exp_final3_excl_contr <- exp_final3_excl_lang %>% 
  filter(controlwith.Quantised == 1 | controllack.Quantised == 2)


#how many people before exclusions in each group
table(exp_final3$randomiser.9dvt)

#people in each group after exclusions
table(exp_final3_excl_lang$randomiser.9dvt)
table(exp_final3_excl_contr$randomiser.9dvt)

final_df <- exp_final3_excl_contr

#a long format of dataset for easier denoting of the between subjects condition of connection type
exp_long <- exp_final3 %>%
  pivot_longer(
    cols = matches("^X\\d+_[12]$"),
    names_to = c("example", "question"),
    names_pattern = "X(\\d+)_(\\d+)",
    values_to = "response"
  ) %>%
  mutate(
    example = as.numeric(example),
    question = as.numeric(question),
    connection_type = case_when(
      example <= 5 ~ 1,
      example <= 10 ~ 2,
      example <= 15 ~ 3
    )
  )


exp_long2 <- exp_long %>% 
  filter(polish.Quantised %in% c("1", "2"))

exp_long3 <- exp_long2 %>% 
  filter(controllack.Quantised == 2 | controlwith.Quantised == 1)

exp_long3$response = as.numeric(exp_long3$response)

#means of the first question - sensibility
exp_long3 %>% 
  filter(question == 1) %>% 
  group_by(modifier, sentence_type, connection_type) %>% 
  summarise(Mean = mean(response))

#means of the second question - naturalness to assert
exp_long3 %>% 
  filter(question == 2) %>% 
  group_by(modifier, sentence_type, connection_type) %>% 
  summarise(Mean = mean(response))

exp_long3 <- exp_long3 %>% 
  mutate(connection_type = factor(connection_type, levels = c(1, 2, 3),
         labels = c("IL+ST", "NIL+ST", "NIL+DT")),
         sentence_type = factor(sentence_type, levels = c(0, 1),
                                labels = c("Conditional", "Conjunction")),
         modifier = factor(modifier, levels = c(0, 1), labels = c("No modifier", "Present modifier")))

#creating a dataset with only data from the sensibless question
sensible <- exp_long3 %>% 
  filter(question == 1)

#creating a dataset with only data from the naturalness question
natural <- exp_long3 %>% 
  filter(question == 2)

#histogram of sensibless question
ggplot(
  sensible,
  aes(x = factor(response),
      fill = factor(modifier))
) + geom_bar(position = position_dodge2(preserve="single")) + 
  facet_grid(
    sentence_type ~ connection_type,
    labeller = labeller(
      sentence_type = c("Conditional", "Conjunction"),
      connection_type = c("IL+ST", "Present modifier")
    )
  ) + labs(
    x = "Response",
    y = "Number of responses",
    fill = "Modifier"
  ) +
  scale_fill_manual(values = c(
    "No modifier" = "#648FFF",
    "Present modifier" = "#FFB000"
  )) + theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = "11"),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

#histogram of the naturalness question
ggplot(
  natural,
  aes(x = factor(response),
      fill = factor(modifier))
) + geom_bar(position = position_dodge2(preserve="single")) + 
  facet_grid(
    sentence_type ~ connection_type,
    labeller = labeller(
      sentence_type = c("Conditional", "Conjunction"),
      connection_type = c("IL+ST", "Present modifier")
    )
  ) + labs(
    x = "Response",
    y = "Number of responses",
    fill = "Modifier"
  ) +
  scale_fill_manual(values = c(
    "No modifier" = "#785EF0",
    "Present modifier" = "#FE6100"
  )) + scale_y_continuous(
    limits = c(0, 80),
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = "11"),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

#dataset for checking the replication part of the experiment
for_replication <- exp_long3 %>% 
  filter(modifier == "No modifier")

#counting the means from participants' answers in each type of connection between clauses
participant_means <- for_replication %>% 
  group_by(Participant.Private.ID,
           sentence_type,
           connection_type,
           question) %>% 
  summarise(mean_response = mean(response, na.rm = TRUE), .groups = "drop")

#descriptive table form data in various groups
descriptive <- participant_means %>% 
  group_by(question, sentence_type, connection_type) %>% 
  summarise(
    N = n(),
    Mean = mean(mean_response, na.rm = TRUE),
    SD = sd(mean_response, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2)
  )

descriptive

#participants' means only for the sensibless question
sensible_data <- participant_means %>% 
  filter(question == 1)

#participants's means only for the naturalness question
natural_data <- participant_means %>% 
  filter(question == 2)

#sensibleness means in the different topics condition
sensible_DT <- sensible_data %>% 
  filter(connection_type == "NIL+DT")

#naturalness means in the different topics condition
natural_DT <- natural_data %>% 
  filter(connection_type == "NIL+DT")

#counding means and sd for sensibless in DT condition
sensible_DT %>% 
  group_by(sentence_type) %>% 
  summarise(
    N = n(),
    Mean = mean(mean_response),
    SD = sd(mean_response)
  )

#t-test Welch - cause of differences in the group for sensibleness
t.test(mean_response ~ sentence_type, data = sensible_DT)
cohens_d(mean_response ~ sentence_type, data = sensible_DT)

#naturalness - descriptive statistics in the DT condition
natural_DT %>% 
  group_by(sentence_type) %>% 
  summarise(
    N = n(),
    Mean = mean(mean_response),
    SD = sd(mean_response)
  )

#t-test Welch - cause of differences in the group for the naturalness question in DT
t.test(mean_response ~ sentence_type, data = natural_DT)
cohens_d(mean_response ~ sentence_type, data = natural_DT)

#robust yuen for sensibleness
robust_DT_sensible <- yuen(
  mean_response ~ sentence_type,
  data = sensible_DT,
  tr = 0.2
)

#robust yuen for naturalness to assert
robust_DT_natural <- yuen(
  mean_response ~ sentence_type,
  data = natural_DT,
  tr = 0.2
)


#--- MAIN ANALYSIS ----
sensible_ST <- sensible_data %>% 
  filter(connection_type %in% c("IL+ST", "NIL+ST"))

#mixed anova for the sensibless question when the topic is shared
annova_sensible <- aov_ez(
  id = "Participant.Private.ID",
  dv = "mean_response",
  data = sensible_ST,
  between = "sentence_type",
  within = "connection_type",
  type = 3,
  factorize = FALSE
)

annova_sensible

#checking for effects precisely in sensibleness question ST
connection_effects_sensible <- emmeans(
  annova_sensible,
  ~connection_type | sentence_type
)

connection_effects_sensible

pairs(
  connection_effects_sensible,
  adjust = "holm"
)

data_conditional <- sensible_ST %>% filter(sentence_type == "Conditional")
cohens_d(mean_response ~ connection_type, data = data_conditional)

data_conjunction <- sensible_ST %>% filter(sentence_type == "Conjunction")
cohens_d(mean_response ~ connection_type, data = data_conjunction)

sentence_effects_sensible <- emmeans(
  annova_sensible,
  ~sentence_type | connection_type
)

sentence_effects_sensible


pairs(
  sentence_effects_sensible,
  adjust = "holm"
)

data_ILST <- sensible_ST %>% filter(connection_type == "IL+ST")
cohens_d(mean_response ~ sentence_type, data = data_ILST)
data_NILST <- sensible_ST %>% filter(connection_type == "NIL+ST")
cohens_d(mean_response ~ sentence_type, data = data_NILST)


#naturalness question when the topic is shared between the clauses
natural_ST <- natural_data %>% 
  filter(connection_type %in% c("IL+ST", "NIL+ST"))

# mixed anova for the naturalness question in the shared topic conditions
annova_natural <- aov_ez(
  id = "Participant.Private.ID",
  dv = "mean_response",
  data = natural_ST,
  between = "sentence_type",
  within = "connection_type",
  type = 3,
  factorize = FALSE
)

annova_natural

#checking more precisly what are the effects in naturalness question
connection_effects_natural <- emmeans(
  annova_natural,
  ~connection_type | sentence_type
)

connection_effects_natural

data_conditional2 <- natural_ST %>% filter(sentence_type == "Conditional")
cohens_d(mean_response ~ connection_type, data = data_conditional2)

data_conjunction2 <- natural_ST %>% filter(sentence_type == "Conjunction")
cohens_d(mean_response ~ connection_type, data = data_conjunction2)

pairs(
  connection_effects_natural,
  adjust = "holm"
)

sentence_effects_natural <- emmeans(
  annova_natural,
  ~sentence_type | connection_type
)

sentence_effects_natural


pairs(
  sentence_effects_natural,
  adjust = "holm"
)

data_ILST2 <- natural_ST %>% filter(connection_type == "IL+ST")
cohens_d(mean_response ~ sentence_type, data = data_ILST2)
data_NILST2 <- natural_ST %>% filter(connection_type == "NIL+ST")
cohens_d(mean_response ~ sentence_type, data = data_NILST2)

library(car)

#conducting the levene test for the homogeneity of variance
leveneTest(
  mean_response ~ sentence_type,
  data = sensible_ST %>% 
    filter(connection_type == "IL+ST"),
  center = median
)


leveneTest(
  mean_response ~ sentence_type,
  data = sensible_ST %>% 
    filter(connection_type == "NIL+ST"),
  center = median
)


leveneTest(
  mean_response ~ sentence_type,
  data = natural_ST %>% 
    filter(connection_type == "IL+ST"),
  center = median
)


leveneTest(
  mean_response ~ sentence_type,
  data = natural_ST %>% 
    filter(connection_type == "NIL+ST"),
  center = median
)

sensible_ST <- sensible_data %>% 
  filter(connection_type %in% c("IL+ST", "NIL+ST")) %>% 
  droplevels()

natural_ST <- natural_data %>% 
  filter(connection_type %in% c("IL+ST", "NIL+ST")) %>% 
  droplevels()

#because of failing certain requirements of regular anova im conducting a robust analysis

install.packages("WRS2")
library(WRS2)


robust_sensible <- bwtrim(
  mean_response ~ sentence_type * connection_type,
  id = Participant.Private.ID,
  data = sensible_ST,
  tr = 0.2
)

robust_sensible


robust_natural <- bwtrim(
  mean_response ~ sentence_type * connection_type,
  id = Participant.Private.ID,
  data = natural_ST,
  tr = 0.2
)

robust_natural


scores_sensible <- sensible %>% 
  group_by(modifier, sentence_type, connection_type) %>% 
  summarise(M = round(mean(response), 2),
            SD = round(sd(response), 2),
            Mdn = median(response),
            .groups = "drop")

ft <- flextable(scores_sensible) %>% 
  set_header_labels(modifier = "Modifier",
                    sentence_type = "Sentence type",
                    connection_type = "Connection type",
                    M = "M",
                    SD = "SD",
                    Mdn = "Mdn") %>% 
  theme_booktabs() %>% 
  merge_v(j = c("modifier", "sentence_type")) %>% 
  valign(j = c("modifier", "sentence_type"), valign = "top") %>% 
  align(j = c("M", "SD", "Mdn"),
        align = "center",
        part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 12, part = "all") %>% 
  autofit()

scores_natural <- natural %>% 
  group_by(modifier, sentence_type, connection_type) %>% 
  summarise(M = round(mean(response), 2),
            SD = round(sd(response), 2),
            Mdn = median(response),
            .groups = "drop")

ft_n <- flextable(scores_natural) %>% 
  set_header_labels(modifier = "Modifier",
                    sentence_type = "Sentence type",
                    connection_type = "Connection type",
                    M = "M",
                    SD = "SD",
                    Mdn = "Mdn") %>% 
  theme_booktabs() %>% 
  merge_v(j = c("modifier", "sentence_type")) %>% 
  valign(j = c("modifier", "sentence_type"), valign = "top") %>% 
  align(j = c("M", "SD", "Mdn"),
        align = "center",
        part = "all") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 12, part = "all") %>% 
  autofit()





library(lme4)
library(lmerTest)

sensible_rep <- sensible %>% 
  filter(modifier == "No modifier") %>% 
  droplevels() %>% 
  mutate(
    Participant.Private.ID = factor(Participant.Private.ID),
    example = factor(example),
    sentence_type = factor(sentence_type),
    connection_type = factor(connection_type),
    response = as.numeric(response)
  )


options(contrasts = c("contr.sum", "contr.poly"))

model_sensible <- lme4::lmer(
  response ~ sentence_type * connection_type +
    (1 | Participant.Private.ID) +
    (1 | example),
  data = sensible_rep,
  REML = TRUE
)

summary(model_sensible)
lme4::isSingular(model_sensible)

car::Anova(
  model_sensible,
  type = 3,
  test.statistic = "Chisq"
)

model_sensible_afex <- afex::mixed(
  response ~ sentence_type * connection_type +
    (1 | Participant.Private.ID) + 
    (1 | example),
  data = sensible_rep,
  method = "KR" #Kenward-Roger method
)

model_sensible_afex

emm_connection_sensible <- emmeans(
  model_sensible_afex,
  ~ connection_type | sentence_type
)

emm_connection_sensible

pairs(emm_connection_sensible,
      adjust = "holm")

eff_size(
  emm_connection_sensible,
  sigma = sigma(model_sensible),
  edf = df.residual(model_sensible)
)

emm_sentence_sensible <- emmeans(
  model_sensible_afex,
  ~ sentence_type | connection_type
)

emm_sentence_sensible

pairs(emm_sentence_sensible,
      adjust = "holm")

eff_size(
  emm_sentence_sensible,
  sigma = sigma(model_sensible),
  edf = df.residual(model_sensible)
)

natural_rep <- natural %>% 
  filter(modifier == "No modifier") %>% 
  droplevels() %>% 
  mutate(
    Participant.Private.ID = factor(Participant.Private.ID),
    example = factor(example),
    sentence_type = factor(sentence_type),
    connection_type = factor(connection_type),
    response = as.numeric(response)
  )



model_natural_afex <- afex::mixed(
  response ~ sentence_type * connection_type +
    (1 | Participant.Private.ID) + 
    (1 | example),
  data = natural_rep,
  method = "KR" #Kenward-Roger method
)

model_natural_afex

emm_connection_natural <- emmeans(
  model_natural_afex,
  ~ connection_type | sentence_type
)

emm_connection_natural

pairs(emm_connection_natural,
      adjust = "holm")


model_natural <- lme4::lmer(
  response ~ sentence_type * connection_type +
    (1 | Participant.Private.ID) +
    (1 | example),
  data = natural_rep,
  REML = TRUE
)

eff_size(
  emm_connection_natural,
  sigma = sigma(model_natural),
  edf = df.residual(model_natural)
)

emm_sentence_natural <- emmeans(
  model_natural_afex,
  ~ sentence_type | connection_type
)

emm_sentence_natural

pairs(emm_sentence_natural,
      adjust = "holm")

eff_size(
  emm_sentence_natural,
  sigma = sigma(model_natural),
  edf = df.residual(model_natural)
)






modifier_sensible <- sensible %>%
  mutate(
    Participant.Private.ID = factor(Participant.Private.ID),
    example = factor(example),
    sentence_type = factor(sentence_type),
    modifier = factor(modifier),
    connection_type = factor(connection_type)
  )

model_modifier_sensible <- afex::mixed(
  response ~ sentence_type * modifier * connection_type +
    (1 | Participant.Private.ID) +
    (1 | example),
  data = modifier_sensible,
  method = "KR"
)

model_modifier_sensible


modifier_natural <- natural %>%
  mutate(
    Participant.Private.ID = factor(Participant.Private.ID),
    example = factor(example),
    sentence_type = factor(sentence_type),
    modifier = factor(modifier),
    connection_type = factor(connection_type)
  )

model_modifier_natural <- afex::mixed(
  response ~ sentence_type * modifier * connection_type +
    (1 | Participant.Private.ID) +
    (1 | example),
  data = modifier_natural,
  method = "KR"
)

model_modifier_natural


emm_modifier_sensible <- emmeans(
  model_modifier_sensible,
  ~ modifier | sentence_type * connection_type
)

emm_modifier_sensible


pairs(emm_modifier_sensible)

eta_squared(
  model_modifier_sensible$full_model,
  partial = FALSE,
  generalized = TRUE
)

eff_size(
  emm_modifier_sensible,
  sigma = sigma(model_modifier_sensible$full_model),
  edf = df.residual(model_modifier_sensible$full_model)
)


emm_modifier_natural <- emmeans(
  model_modifier_natural,
  ~ modifier | sentence_type * connection_type
)

emm_modifier_natural

pairs(emm_modifier_natural)

eta_squared(
  model_modifier_natural$full_model,
  partial = FALSE,
  generalized = TRUE
)

eff_size(
  emm_modifier_natural,
  sigma = sigma(model_modifier_natural$full_model),
  edf = df.residual(model_modifier_natural$full_model)
)
