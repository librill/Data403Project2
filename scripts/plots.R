# --
# plots
# --

source(here::here("functions.R"))

# --
# bar plot separated by default & no default for some classes
# --

selected_vars <- c(
  "HOUSING_TYPE_OWN",
  "FLAG_OWN_CAR",
  "FAMILY_STATUS_MARRIED",
  "EDUCATION_TYPE_HIGHER"
)

plot_df <- df |>
  dplyr::select(TARGET, HOUSING_TYPE_OWN, FLAG_OWN_CAR, FAMILY_STATUS_MARRIED,
                EDUCATION_TYPE_HIGHER) |>
  dplyr::mutate(TARGET = factor(TARGET, levels = c(0, 1),
                                labels = c("No issues",
                                           "Struggled with payments"))) |>
  tidyr::pivot_longer(cols = dplyr::all_of(selected_vars),
                      names_to  = "predictor",
                      values_to = "value") |>
  dplyr::mutate(predictor = factor(predictor, levels = c("HOUSING_TYPE_OWN",
                                                         "FAMILY_STATUS_MARRIED",
                                                         "FLAG_OWN_CAR",
                                                         "EDUCATION_TYPE_HIGHER"),
                                   labels = c("Owns Housing", "Married",
                                              "Owns Car",
                                              "Higher Education"))) |>
  dplyr::group_by(TARGET, predictor) |>
  dplyr::summarise(prop = mean(value, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = predictor, y = prop, fill = TARGET)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Struggled with payments" = "#3663df",
                               "No issues" = "#80b6fa"),
                    breaks = c("Struggled with payments", "No issues")) +
  labs(
    title = "Predictor Proportions by Target",
    x = "Predictor",
    y = "Proportion",
    fill = "Target"
  ) +
  theme_minimal()

# --
# plot default rate by number of children
# --

df <- df %>%
  mutate(TARGET = as.numeric(as.character(TARGET)))
plot_children <- df |>
  group_by(CNT_CHILDREN) |>
  summarise(default_rate = mean(TARGET)) |>
  ggplot(aes(x = factor(CNT_CHILDREN), y = default_rate, fill = factor(CNT_CHILDREN))) +
  geom_col() +
  labs(title="Default Rate by Number of Children",
       x = "Number of Children",
       y = "Default Rate") +
  theme_bw() +
  theme(legend.position = "none")

plot_children

# --
# plot default rate by education level (higher, secondary, other)
# --

df <- df %>%
  mutate(TARGET = as.numeric(as.character(TARGET)))
plot_education <- df |>
  mutate(
    Education = factor(case_when(
      EDUCATION_TYPE_HIGHER == 1 ~ "Higher",
      EDUCATION_TYPE_SECONDARY == 1 ~ "Secondary",
      TRUE ~ "Other"
    ), levels = c("Higher", "Secondary", "Other"))
  ) |>
  group_by(Education) |>
  summarise(default_rate = mean(TARGET)) |>
  ggplot(aes(x = Education, y = default_rate, fill = Education)) +
  geom_col() +
  labs(title = "Default Rate by Education Level",
       y = "Default Rate") +
  theme_bw() +
  theme(legend.position = "none")

plot_education
