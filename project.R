#!/usr/bin/R -q --slave -f

library(corrplot)
library(HH)
library(leaps)
library(car)

set.seed(11)

mlb2016 = read.csv("mlb_team_data_2016.csv")
mlb2017 = read.csv("mlb_team_data_2017.csv")
mlb2018 = read.csv("mlb_team_data_2018.csv")


mlb = rbind(mlb2016, mlb2017, mlb2018)

trimmed_features = c(
	"win_percentage",
	"average_batter_age",
	"average_pitcher_age",
	"batting_average",
	"earned_runs_against",
	"hits",
	"hits_allowed",
	"on_base_plus_slugging_percentage",
	"runs",
	"runs_against",
	"strikeouts_per_nine_innings",
	"stolen_bases",
	"strikeouts_per_base_on_balls",
	"whip",
	"saves",
	"times_struck_out"
)

mlb_trimmed = mlb[trimmed_features]

for (feature in trimmed_features) {

	png(filename = paste("individual_scatterplots/", feature, ".png", sep = ""), width = 720, height =  720)
	plot(mlb_trimmed$win_percentage ~ mlb_trimmed[[feature]],
		xlab = feature,
		ylab = "win_percentage"
	)
	. = dev.off()
}

# suggests that some variables should be removed
png("correlation_plot.png", width = 720, height = 720)
corrplot(cor(mlb_trimmed[-1]))
. = dev.off()
cor(mlb_trimmed$win_percentage, mlb_trimmed)

# plot weak variables
stolen_bases_model = lm(win_percentage ~ stolen_bases, data = mlb_trimmed)
summary(stolen_bases_model)

average_batter_age_model = lm(win_percentage ~ average_batter_age, data = mlb_trimmed)
summary(average_batter_age_model)

test_observations = sample(90, 60, replace = FALSE)
mlb_train = mlb_trimmed[test_observations, ]
mlb_test = mlb_trimmed[-test_observations, ]

# best is average_batter_age-hits_allowed-runs-runs_against-saves-times_struck_out
mlb_all = regsubsets(win_percentage ~ . - win_percentage, data = mlb_train)
summaryHH(mlb_all)

formula_p7 = "win_percentage ~ average_batter_age+average_pitcher_age+earned_runs_against+hits_allowed+runs+saves+times_struck_out"
formula_p4 = "win_percentage ~ earned_runs_against+runs+saves+times_struck_out"

mlb_p7 = lm(as.formula(formula_p7), data = mlb_train)
summary(mlb_p7)

mlb_p4 = lm(as.formula(formula_p4), data = mlb_train)
summary(mlb_p4)

png(filename = "avplots/p7.png", width = 720, height = 720)
avPlots(mlb_p7)
. = dev.off()

png(filename = "avplots/p4.png", width = 720, height = 720)
avPlots(mlb_p4)
. = dev.off()

png(filename = "influence/p7.png", width = 720, height = 720)
influencePlot(mlb_p4)
. = dev.off()

png(filename = "influence/p4.png", width = 720, height = 720)
influencePlot(mlb_p4)
. = dev.off()

mlb_train_adjusted = mlb_train[!(row.names(mlb_train) %in% c(3, 21, 67)), ]
nrow(mlb_train_adjusted)

mlb_p7_adj = lm(as.formula(formula_p7), data = mlb_train_adjusted)
summary(mlb_p7_adj)

mlb_p4_adj = lm(as.formula(formula_p4), data = mlb_train_adjusted)
summary(mlb_p4_adj)

png(filename = "avplots/p7_adj.png", width = 720, height = 720)
avPlots(mlb_p7_adj)
. = dev.off()

png(filename = "avplots/p4_adj.png", width = 720, height = 720)
avPlots(mlb_p4_adj)
. = dev.off()
