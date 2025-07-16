### Camera Trap Methodology

# Required packages
library(unmarked)
library(MuMIn)
library(ggplot2)

# Load in the data
encounters <- read.csv("Data/Encounter histories anonymised.csv", header = TRUE)

site_covariates <- read.csv("Data/Site covariates cameras.csv", header = TRUE) # contains difficulty and sex

posts <- read.csv("Data/Posts cameras.csv", header = TRUE)

height <- read.csv("Data/Height cameras.csv", header = TRUE)

kill <- read.csv("Data/Fresh kill cameras.csv", header = TRUE)

check <- read.csv("Data/Check numbers cameras.csv", header = TRUE)
col_names <- names(check)
check[,col_names] <- lapply(check[,col_names] , as.character)

# Setup umf
encounters_names <- encounters[,2:19]

obs_covariates <- list(posts = posts,
                       height = height,
                       kill = kill,
                       check = check)

umf <- unmarkedMultFrame(y = encounters_names, siteCovs = site_covariates, obsCovs = obs_covariates, numPrimary = 3)

summary(umf)

# Fit a null occupancy model
model_Null <- colext(psiformula = ~1,
                     gammaformula = ~1,
                     epsilonformula = ~1,
                     pformula = ~1,
                     data = umf)
model_Null

# Fit a full occupancy model
model_Full <- colext(psiformula = ~1,
                     gammaformula = ~1,
                     epsilonformula = ~1,
                     pformula = ~ check + Sex + Difficulty + height + posts + kill,
                     data = umf)

model_Full

# Fit all possible models
modelList <- dredge(model_Full, 
                    rank = "AIC")

modelList
## Best models both contain check, sex and difficulty. Need to back transform to get estimates.

# Fit the best model
model_best <- colext(psiformula = ~1,
                     gammaformula = ~1,
                     epsilonformula = ~1,
                     pformula = ~ check + Sex + Difficulty + height,
                     data = umf)

model_best


# Make predictions
difficulty <- c("Easy", "Easy","Easy","Easy","Easy","Easy","Hard","Hard","Hard","Hard","Hard","Hard")

heights <- c("L", "L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","M","M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H")

nd <- data.frame(check = rep(1:6, times = 12), Sex = rep(0:1, each = 12, times = 6), Difficulty = rep(difficulty, 12), height = heights)

nd$check <- as.factor(nd$check)
nd$Sex <- as.factor(nd$Sex)
nd$Difficulty <- as.factor(nd$Difficulty)
nd$height <- factor(nd$height, levels = c("L", "M", "H"))

predict_detection <- predict(model_best, type = "det", newdata = nd)

predict_detection

output <- cbind(nd, predict_detection)


# Plot predicted values
#output$Sex <- as.factor(output$Sex)

#output$height <- factor(output$height, levels = c("L", "M", "H"))

plot_output <- ggplot(output, aes(check, Predicted)) +
  geom_point(aes(check, Predicted, color = Sex), position = position_dodge(width = 0.5)) +
  geom_line(aes(color = Sex, group = Sex, linetype = Sex), position = position_dodge(width = 0.5), linewidth = 1) +
  facet_wrap(Difficulty ~ height) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = Sex), position = position_dodge(width = 0.5), width = 0.3, linewidth = 1) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "darkgray"),
        strip.text.x = element_text(size = 14)) +
  theme_classic() +
  scale_color_manual(values = c("forestgreen", "purple"), labels = c("Male", "Female")) +
  scale_shape_discrete(name = "Sex", labels = c("Male", "Female")) +
  scale_linetype_discrete(name = "Sex", labels = c("Male", "Female")) +
  labs(x = "Check",
       y = "Detection Probability") +
  #scale_x_continuous(breaks = seq(1, 6, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25))


plot_output


ggsave("Plots/Detection Probabilities 2019-21 for publication.jpeg", plot = plot_output, width = 15, height = 10, dpi = 300)
