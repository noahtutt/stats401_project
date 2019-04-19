#!/usr/bin/R -q --slave -f

library(corrplot)
library(HH)
library(leaps)
library(car)
library(ggplot2)

set.seed(11)

gen_images = FALSE

mlb2016 = read.csv("mlb_team_data_2016.csv")
mlb2017 = read.csv("mlb_team_data_2017.csv")
mlb2018 = read.csv("mlb_team_data_2018.csv")

dirs = c("influence", "avplots", "individual_scatterplots")
for (dir in dirs) {
	if (!dir.exists(dir)) {
		dir.create(dir)
	}
}


mlb = rbind(mlb2016, mlb2017, mlb2018)

trimmed_features = c(
	"win_percentage",
	"average_batter_age",
	"average_pitcher_age",
	"batting_average",
	"earned_runs_against",
	"hits", "hits_allowed",
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

if (gen_images) {

	for (feature in trimmed_features) {
	
		# plot(mlb_trimmed$win_percentage ~ mlb_trimmed[[feature]],
		# 	xlab = feature,
		# 	ylab = "win_percentage"
		# )
		ggplot(mlb_trimmed, mapping = aes_string(x = feature, y = "win_percentage")) + geom_point() +
			xlab(feature) + ylab("win_percentage")
		ggsave(filename = paste("individual_scatterplots/", feature, ".png", sep = ""))
	}
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

candidates = list(mlb_p7, mlb_p4, mlb_p7_adj, mlb_p4_adj)

# r_squared = c(0, 0, 0, 0)
# adj_r_squared = c(0, 0, 0, 0)
# shrinkage = c(0, 0, 0, 0)
# 
# num_trials = 10

	# cv_shrinkage = function(model, test) {
	#  
	#  	predicted = predict(model, test)
	#  	test_corr = cor(predicted, test$win_percentage)
	# 	summary(model)$r.squared - test_corr ** 2 	
	# }
	# 
	# for (i in 1:num_trials) {
	# 
	# 	test_observations = sample(90, 60, replace = FALSE)
	# 	mlb_train = mlb_trimmed[test_observations, ]
	# 	mlb_test = mlb_trimmed[-test_observations, ]
	# 	
	# 	mlb_train_adjusted = mlb_train[!(row.names(mlb_train) %in% c(3, 21, 67)), ]
	# 	
	# 	candidates = list(
	# 		lm(as.formula(formula_p7), data = mlb_train),
	# 		lm(as.formula(formula_p4), data = mlb_train),
	# 		lm(as.formula(formula_p7), data = mlb_train_adjusted),
	# 		lm(as.formula(formula_p4), data = mlb_train_adjusted)
	# 	)	
	# 
	# 	for (j in 1:4) {
	# 
	# 		r_squared[j] = r_squared[j] + summary(candidates[j])$r.squared
	# 		adj_r_squared[j] = adj_r_squared[j] + summary(candidates[j])$adj.r.squared
	# 		shrinkage[j] = shrinkage[j] + cv_shrinkage(candidates[j], mlb_test)	
	# 
	# 	}
	# }
	# 
	# r_squared = r_squared / num_trials
	# adj_r_squared = adj_r_squared / num_trials
	# shrinkage = shrinkage / num_trials
	# 
	# 
	# for (i in 1:4) {
	# 
	# 	writeLines(paste("Adjusted R^2:", adj_r_squared[i], "R^2", r_squared[i], "Shrinkage:", shrinkage[i]))
	# 
	# }

nrow(mlb_test)

for (model in candidates) {

	predicted = predict(model, mlb_test)
	test_corr = cor(predicted, mlb_test$win_percentage)
	print(test_corr ** 2)
}

conference_features = c(trimmed_features, "abbreviation")
conference = read.csv("conference.csv", stringsAsFactors = FALSE)
mlb_conference = merge(mlb[conference_features], conference, by = "abbreviation")
mlb_conference = mlb_conference[, -1]

if (gen_images) {

	for (feature in trimmed_features) {
	
		# plot(mlb_trimmed$win_percentage ~ mlb_trimmed[[feature]],
		# 	xlab = feature,
		# 	ylab = "win_percentage"
		# )
		ggplot(mlb_conference, mapping = aes_string(x = feature, y = "win_percentage", color = "conference")) + geom_point() +
			xlab(feature) + ylab("win_percentage")
		ggsave(filename = paste("individual_scatterplots/", feature, "_conference.png", sep = ""))
	}
}

# true for AL
bools = mlb_conference$conference == "AL"
mlb_conference$conference = bools


png(filename = "individual_scatterplots/conference.png", width = 720, height = 720)
plot(mlb_conference$conference, mlb_conference$win_percentage,
	xlab = "Conference",
	ylab = "Win Percentage",
	main = "Win Percentage vs Conference"
)
. = dev.off()

conference_all = regsubsets(win_percentage ~ . - win_percentage, data = mlb_conference)
summaryHH(mlb_all)


# final model diagnostics
# qqplot

png(filename = "qqplot.png", width = 720, height = 720)
qqnorm(mlb_p7$resid)
qqline(mlb_p7$resid, col = "purple")
. = dev.off()

vif(mlb_p7)
vif(mlb_p4)





