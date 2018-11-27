library(statsr)
library(dplyr)
library(ggplot2)


# load 2011 mlb data
data(mlb11)

" A scatterplot would be used for showing the relationship between runs the other numerical variables. "

#  relationship between runs and at_bats
x11()
ggplot(data = mlb11, aes(x = at_bats, y = runs)) + geom_point()

" The relationship is linear. "

# correlation coefficient
mlb11 %>%
  summarise(cor(runs, at_bats))

" The relationship is positive, linear, and moderately strong. 
  One of the potential outliers is a team with approximately 5520 at bats. "

# interactive plot to check sum of squares
x11()
plot_ss(x = at_bats, y = runs, data = mlb11, showSquares = TRUE)

" Lowest sum of squares was found to be 126557.4 through trial and error. "

# generate regression line for at_bats and runs
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

# regression model for homeruns and runs
m1 <- lm(runs ~ homeruns, data = mlb11)
summary(m1)

"  For each additional home run, the model predicts 1.83 more runs, on average. "

# scatterplot with m1 regression line
x11()
ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# observed number of runs for the team with 5,579 at bats
mlb11 %>%
  filter(at_bats == 5579) %>%
  select(runs)

" Residual for the prediction of runs for a team with 5,579 at-bats is 15.32. "

# check for linearity
# residuals vs fitted values
x11()
ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

" FALSE:  The residuals show a curved pattern. "

# check for nearly normal residuals

# histogram
x11()
ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

# normal probability plot
x11()
ggplot(data = m1, aes(sample = .resid)) +
  stat_qq()


" The residuals are fairly symmetric, with only a slightly longer tail on the right, 
  hence it would be appropriate to deem the the normal distribution of residuals condition met. "

# constant variability
" Based on the residuals plot from earlier, the constant variability condition appears to be met. "

# relationship between bating average and runs
x11()
ggplot(data = mlb11, aes(x = bat_avg, y = runs)) +
  geom_point()

" There seems to be a linear relationship between batting average and runs. "

# regression model for batting_average vs runs
m2 <- lm(runs ~ bat_avg, data = mlb11)

# compare the two models
sm1 <-summary(m1)
summary(m2)

" The model based on batting average is better as it has a higher R-square value (0.6561 vs 0.6266). "

# comparing all models based on traditional variables
old_rsq <- c()

for(i in names(mlb11[,3:9]))
{
  rsq <- summary(lm(runs ~ get(i), data = mlb11))$r.squared
  old_rsq <- c(old_rsq, rsq)
  print(paste(i, round(rsq,digits = 4)))
  
}

" Batting average has the highest R-squared value (0.6561), so it is the best explanatory variable. "

# comparing all models based on new variables
new_rsq <- c()

for(i in names(mlb11[,10:12]))
{
  rsq <- summary(lm(runs ~ get(i), data = mlb11))$r.squared
  new_rsq <- c(new_rsq, rsq)
  print(paste(i, round(rsq,digits = 4)))
  
}


# compare strength of fit of models based on old and new variables
cmp <- data.frame(vals = c(old_rsq, new_rsq),
                  type = factor(names(mlb11[,3:12])),
                  cat = factor(c(c(rep("old", length(old_rsq))), c(rep("new", length(new_rsq))))))

# means of R-squares for models based on old and new variables
cmp %>% 
  group_by(cat) %>% 
  summarise(avg_rsq = mean(vals))

# rearrange factors
cmp$type <- relevel(cmp$type, "new_obs")
cmp$type <- relevel(cmp$type, "new_onbase")
cmp$type <- relevel(cmp$type, "new_slug")

# visualise R-squared values
x11()
ggplot(data = cmp, aes(y = vals, x = type, fill = cat)) +
  geom_col()

" On-base plus slugging is the best predictor for runs. "

# model diagnostics for the regression model with on-base plus slugging
summary(lm(runs ~ new_obs, data = mlb11))


