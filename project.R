#!/usr/bin/R -q --slave -f

mlb2016 = read.csv("mlb_team_data_2016.csv")
mlb2017 = read.csv("mlb_team_data_2017.csv")
mlb2018 = read.csv("mlb_team_data_2018.csv")

mlb = rbind(mlb2016, mlb2017, mlb2018)

head(mlb)
length(mlb[[1]])


trimmed_features = c(
	"name",
	"abbreviation",
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
	"win_percentage",
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


	
