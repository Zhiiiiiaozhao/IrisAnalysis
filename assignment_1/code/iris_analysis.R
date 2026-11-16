# Assignment 1

# Load package
library(tidyverse)

# Load Iris dataset
data(iris)

# Explore the dataset
head(iris)
summary(iris)
str(iris)

iris %>% summarize_if(is.numeric, mean)

# Save the dataset as a CSV file
write_csv(
  iris,
  "assignment_1/data/iris.csv"
)

# Summarize measurements by species
iris_summary <- iris %>%
  group_by(Species) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~ mean(.x),
        sd = ~ sd(.x)
      ),
      .names = "{.col}_{.fn}"
    ),
    n = n(),
    .groups = "drop"
  )

# Display and save the summary table
print(iris_summary)

write_csv(
  iris_summary,
  "assignment_1/results/iris_summary.csv"
)

# Transform the dataset from wide to long format
iris_long <- iris %>%
  pivot_longer(
    cols = -Species,
    names_to = "Measurement",
    values_to = "Value"
  ) %>%
  mutate(
    Measurement = recode(
      Measurement,
      "Sepal.Length" = "Sepal length",
      "Sepal.Width" = "Sepal width",
      "Petal.Length" = "Petal length",
      "Petal.Width" = "Petal width"
    )
  )

# Define a consistent color palette
species_colors <- c(
  "setosa" = "#1B9E77",
  "versicolor" = "#D95F02",
  "virginica" = "#7570B3"
)

# Plot: distributions of all measurements
distribution_plot <- ggplot(
  iris_long,
  aes(
    x = Species,
    y = Value,
    fill = Species
  )
) +
  geom_violin(
    trim = FALSE,
    alpha = 0.65,
    color = NA
  ) +
  geom_boxplot(
    width = 0.14,
    alpha = 0.85,
    outlier.shape = NA,
    color = "grey25"
  ) +
  geom_jitter(
    width = 0.08,
    size = 0.7,
    alpha = 0.35,
    color = "grey20"
  ) +
  facet_wrap(
    ~ Measurement,
    scales = "free_y",
    ncol = 2
  ) +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "Distribution of Iris Flower Measurements",
    x = NULL,
    y = "Measurement (cm)",
    fill = "Species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey35"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(distribution_plot)

ggsave(
  filename = "assignment_1/figures/iris_measurement_distributions.png",
  plot = distribution_plot,
  width = 9,
  height = 7,
  dpi = 300
)
