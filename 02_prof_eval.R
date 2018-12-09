library(statsr)
library(dplyr)
library(ggplot2)
library(GGally)

# load student evaluation data
data(evals)

" This is an observational study as there are no control and treatment groups; There has been no external influence. "

" The question should be, 'Is there an association between beauty and course evaluations?' "

# distribution of score
evals %>% summarise(med_scr = median(score),
                    iqr_25 = quantile(score, 0.75),
                    u3 = sum(score < 3))

x11()
ggplot(data = evals, aes(x = score)) + geom_histogram(binwidth = 1)

" FALSE: The left skewness of the data suggests that the students are less likely to rate the professors highly. "

# relationship between age and outfit of professor in photo
x11()
ggplot(data = evals, aes(y = age,x = pic_outfit)) + geom_boxplot()

" The older professors are in formal outfits in their photos, while the youngest ones are in non-formal. "

# relationship between beauty rating and score
x11()
ggplot(data = evals, aes(x = bty_avg, y = score)) + geom_point()

# plot with random variation
x11()
ggplot(data = evals, aes(x = bty_avg, y = score)) + geom_jitter()

# fit a linear model
x11()
ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

# summary of linear model
lm_bty <- lm(score ~ bty_avg, data = evals) 
summary(lm_bty)

" As the P-value for beauty score is very low (5.08e-05), it is indeed a significant predictor.
  However, the adjusted R-squared value is very low (0.03293), so the model is not very useful. "

# condition check for least squares line

# linearity and constant variability
x11()
ggplot(data = lm_bty, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

" Randomly scattered residuals. Constant Variability. "

# normal distribution of residuals

# histogram
x11()
ggplot(data = lm_bty, aes(x = .resid)) +
  geom_histogram(binwidth = 1) +
  xlab("Residuals")

" Slight left skew, but almost normal. "

# normal probability plot
x11()
ggplot(data = lm_bty, aes(sample = .resid)) + stat_qq()
" Mostly normal, but slighly skewed to the left at the end. "

" FALSE: Nearly normal residuals: Residuals are right skewed, but the sample size 
  is large, so this may not be an important violation of conditions. "

# plot beauty score ratings by each student
x11()
ggplot(data = evals, aes(x = bty_f1lower, y = bty_avg)) +
  geom_jitter()

# check correlation between the variables
evals %>% 
  summarise(cor(bty_avg, bty_f1lower))

# relationship between all beauty variables
x11()
ggpairs(evals, columns = 13:19)


# create model with gender and average beauty score as predictors
m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

# check conditions for this model

# linearity and constant variability
x11()
ggplot(data = m_bty_gen, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

" Randomly scattered residuals. Constant Variability. "

# normal distribution of residuals

# histogram
x11()
ggplot(data = m_bty_gen, aes(x = .resid)) +
  geom_histogram(binwidth = 1) +
  xlab("Residuals")

# normal probability plot
x11()
ggplot(data = m_bty_gen, aes(sample = .resid)) + stat_qq()

" Slightly left skewed, but nearly normal. "

" We can conclude that the conditions for this model are reasonable. "

summary(lm_bty)
summary(m_bty_gen)

" bty_avg is the most significant predictor of score. The addition of gender to the model has 
  made bty_avg even more significant, as its P-value is now lower than before. "

" Since gender:female has been chosen as the reference level, male professors are 
  predicted to have higher course evaluation scores. "

# model for rank vs score
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)

" Correct order of the three levels of rank from lowest to highest course evaluation scores would be:
  Tenure Track, Tenured, Teaching "


# prediction

# new observation
newprof <- data.frame(gender = "male", bty_avg = 3)

# prediction using the model based on score and gender
predict(m_bty_gen, newprof)

# prediction with confidence interval 
predict(m_bty_gen, newprof, interval = "prediction", level = 0.95)


# model selection using backwards elimination

# full model
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

" Number of professors teaching the class is the least significant variable, with a p-value of 0.77806. "

" Non-minority professors are expected on average to score 0.12 points higher 
  than minority professors, all else held constant. "

# model without cls_profs
m_full_1 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)

summary(m_full)
summary(m_full_1)

" There was a negligible change in the p-values and slopes of other variables.
  The dropped variable wasn't collinear with the other explanatory variables. "


# model selection using adjusted R-squared

# copy dataset
modldat <- evals[,1:21]

# Adjusted R-squared for full model
Rsq <- summary(lm(score ~ ., data = modldat))$adj.r.squared

flag <- TRUE
run <- 0

while(flag == TRUE)
{
  run <- run + 1  
  print("----------------------------------------------------------------------------------------")
  print(paste("PASS : ", run))
  print(paste("Current R-squared: ", Rsq))
  
  rvals <- data.frame(exp_var = character(),
                    Rsq = numeric(),
                    stringsAsFactors = FALSE)

  for(i in 2:ncol(modldat))
  {
      modl <- lm(score ~ ., data = modldat[,-i])
      rvals[i-1,1] <- names(modldat[,i])
      rvals[i-1,2] <- summary(modl)$adj.r.squared
  }

  max_index <- which.max(rvals[,2])
  maxval <- rvals[max_index,2]
  maxvar <- rvals[max_index,1] 
  print(paste("Highest R-squared was attained by removal of: ", maxvar, " , new R-squared: ", maxval))

  if(max(rvals[,2]) > Rsq)
  {
    print("New R-squared is greater than previous. Dropping variable. ")
    Rsq <- maxval
    modldat <- modldat[,-(max_index + 1)]
    flag <- TRUE
  
  } else
  {
    print("New R-squared is not greater than previous. Stopping. ")
    print("Final model: ")
    print(summary(lm(score ~ ., data = modldat)))
    flag <- FALSE
  }
} 


" Elimination of cls_profs from the full model yielded the highest adjusted R-squared. "

" Each course is independent of each other, so the evaluations have no correlation. "
" 
  Based on the final model, a professor with a high evaluation score would have: 
  rank: teaching
  ethnicity: not minority
  gender: male
  language: english
  age: (lower number)
  cls_perc_eval: (high number) 
  cls_credit: one credit
  bty_f1lower: (high number)
  bty_f1upper: (high number) 
  bty_f2upper: (high number) 
  bty_avg: (low number)  
  pic_outfit: formal    
  pic_color: black&white 
"

" The conclusions cannot be generalised because the sample size is very small, 
  and preferences may vary from place to place. "
