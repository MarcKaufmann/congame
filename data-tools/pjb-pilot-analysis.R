library("tidyverse")

df_raw <- read_csv("all-instances.csv")

max_wtw <- "3.0" # Check that the scale maxes out at 3.0, i.e. 2.8 is the highest possible amount

neverswitch_to_max <- function(wtw) {
  if_else(wtw == "never-switched", max_wtw, wtw)
}

df <- df_raw %>%
  rename(has_completed = `completed?`) %>%
  # Keep participants completed the study
  filter(has_completed) %>%
  # Drop participants who did not consent or failed out of the study
  filter(rest_treatment %in% c("get-rest-then-elicit", "elicit-then-get-rest")) %>%
  # Replace "never-switched" by max_wtw
  mutate(
    pl5 = neverswitch_to_max(pl5),
    pl8 = neverswitch_to_max(pl8),
    pl11 = neverswitch_to_max(pl11),
    pl15 = neverswitch_to_max(pl15)
  ) %>%
  pivot_longer(names_to = "extra_tasks", names_prefix = "pl", values_to = "WTW", cols = starts_with("pl")) %>%
  select(participant_id, required_tasks, rest_treatment, relax_treatment, extra_tasks, WTW, everything()) %>%
  filter(WTW != "switch-back") %>%
  mutate(
    WTW = parse_double(WTW),
    extra_tasks = parse_double(extra_tasks),
    rest_treatment = as_factor(rest_treatment),
    relax_treatment = as_factor(relax_treatment)
  )

saveRDS(df, "clean_df.rds")

df %>% 
  group_by(extra_tasks, rest_treatment, relax_treatment) %>%
  summarise(mean(WTW), n())

df <- df %>%
  rename(rest_treatment = rest) %>%
  rename(relax = relax_treatment)

df <- df %>%
  mutate(rest = if_else(rest == "elicit-then-get-rest", "NR", "R"),
         relax = if_else(relax == "classical-piano", "classical", if_else(relax == "guided-meditation", "meditation", "waves")))

# After making data long, drop choices that are inconsistent
# After that, turn wtw into doubles
library(stargazer)
m1 <- lm(WTW ~ as_factor(extra_tasks) + rest, data = df)
m2 <- lm(WTW ~ as_factor(extra_tasks) + rest + relax, data = df)
m3 <- lm(WTW ~ as_factor(extra_tasks) + rest*relax, data = df)

stargazer(m1, m2, m3, header=FALSE, 
          title='Base Regressions', 
          type='latex', 
          float = TRUE, 
          single.row = TRUE,
          no.space = TRUE,
          column.sep.width = "3pt",
          font.size = "tiny") %>%
  write("basic_regs.tex")

m4 <- lm(WTW ~ as_factor(extra_tasks) + required_tasks + rest, data = df)
m5 <- lm(WTW ~ as_factor(extra_tasks) + required_tasks + rest + relax, data = df)
m6 <- lm(WTW ~ as_factor(extra_tasks) + required_tasks + rest*relax, data = df)

stargazer(m4, m5, m6, header=FALSE, 
          title='Required Tasks Regressions', 
          type='latex', 
          float = TRUE, 
          single.row = TRUE,
          no.space = TRUE,
          column.sep.width = "3pt",
          font.size = "tiny") %>%
  write("required_tasks_regs.tex")

m7 <- lm(WTW ~ as_factor(extra_tasks) + required_tasks + as_factor(participation_fee) + rest, data = df)
m8 <- lm(WTW ~ as_factor(extra_tasks) + required_tasks+ as_factor(participation_fee) + rest + relax, data = df)
m9 <- lm(WTW ~ as_factor(extra_tasks) + required_tasks + as_factor(participation_fee) + rest*relax, data = df )
  
stargazer(m7, m8, m9, header=FALSE, 
          title='Base Regressions', 
          type='latex', 
          float = TRUE, 
          single.row = TRUE,
          no.space = TRUE,
          column.sep.width = "3pt",
          font.size = "tiny") %>%
  write("fee_regs.tex")

library("ggplot2")

# Plot the disutility at 5, 8, 11, 15 tasks for all

df %>% 
  group_by(rest_treatment, extra_tasks) %>%
  summarize(WTW = mean(WTW), N = n()) %>%
  ggplot(mapping = aes(x = extra_tasks, y = WTW)) + 
  geom_line(aes(group = rest_treatment, color = rest_treatment)) + xlim(5, 15) + ylim(0, 3.2)

#df %>%
#  filter(rest_treatment == "elicit-then-get-rest") %>%
#  group_by(relax_treatment, extra_tasks) %>%
#  summarize(WTW = mean(WTW), N = n()) %>%
#  ggplot(mapping = aes(x = extra_tasks, y = WTW, group = relax_treatment, color = relax_treatment)) + 
#  geom_line() + xlim(5, 15) + ylim(0, 3.2) + 
#  labs(title = "Elicit before rest")
#
#df %>%
#  filter(rest_treatment == "get-rest-then-elicit") %>%
#  group_by(relax_treatment, extra_tasks) %>%
#  summarize(WTW = mean(WTW), N = n()) %>%
#  ggplot(mapping = aes(x = extra_tasks, y = WTW, group = relax_treatment, color = relax_treatment)) + 
#  geom_line() + xlim(5, 15) + ylim(0, 3.2) +
#  labs(title = "Rest then elicit")
  
df %>%
  mutate(rest_treatment = if_else(rest_treatment == "elicit-then-get-rest", "NR", "R")) %>%
  mutate(cross = paste(rest_treatment, relax_treatment, sep = "_")) %>%
  group_by(cross, relax_treatment, rest_treatment, extra_tasks) %>%
  summarize(WTW = mean(WTW), N = n()) %>%
  ggplot(mapping = aes(x = extra_tasks, y = WTW, group = cross)) +
  geom_line(aes(color = relax_treatment, linetype = rest_treatment)) + xlim(5, 15) + ylim(0, 3.2)

df %>%
  mutate(extra_tasks = as_factor(extra_tasks)) %>%
  ggplot(aes(x = extra_tasks, y = WTW)) + 
  geom_boxplot(aes(color = rest_treatment)) +
  facet_grid(. ~ rest_treatment)

df %>%
  filter(rest_treatment == "elicit-then-get-rest") %>%
  mutate(extra_tasks = as_factor(extra_tasks)) %>%
  ggplot(aes(x = extra_tasks, y = WTW)) + 
  geom_boxplot(aes(color = relax_treatment)) +
  facet_grid(. ~ relax_treatment) +
  labs(title = "No Rest before Elicitation")

df %>%
  filter(rest_treatment == "get-rest-then-elicit") %>%
  mutate(extra_tasks = as_factor(extra_tasks)) %>%
  ggplot(aes(x = extra_tasks, y = WTW)) + 
  geom_boxplot(aes(color = relax_treatment)) +
  facet_grid(. ~ relax_treatment) +
  labs(title = "Rest before Elicitation")
