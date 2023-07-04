library(data.table)
library(ggplot2)

dtable = fread("C:/Users/ASUS/Desktop/HW1-regression/iranprovs_mortality_monthly.csv")

dtable$ym_number = dtable$y + (dtable$m -1)/12


dtable[, age_category := ifelse(age_group=="0" |age_group=="01-04" | age_group=="05-09"
                                | age_group == "10-14" | age_group == "15-19","child",
                                ifelse (age_group=="20-24" |age_group=="25-29" | age_group=="30-34"
                                        | age_group == "35-39" | age_group == "40-44" | age_group == "45-49"
                                        | age_group == "50-54" | age_group == "55-59","young","old"))]

dsTable2 = dtable[, .(n = sum(n)), .(y, m, ym_number,prov,age_category)]


#start of corona in bahman 98
corona_start = 1398 + 10/12
model_start = corona_start - 5
deaths_table = dsTable2[ym_number>=corona_start]
deaths_table[, upper_bound := 0 ]
deaths_table[, predicted_deaths := 0]


#calculating predicted values for deaths and the upper bounds for these predicted values.
for (p in 1:31){
  for(a in 1:3){
    for(m in 1:12){
      PROV = unique(dsTable2$prov)[p]
      AGE = unique(dsTable2$age_category)[a]
      M = m
      province_month = dsTable2[prov == PROV & m == M & ym_number>model_start & age_category==AGE]
      fit = lm(n ~ ym_number, province_month[ym_number< corona_start] )
      p_value = summary(fit)$coefficients[2,4]
      if (p_value < 0.1){
        predictions = predict(fit, data.frame(ym_number = province_month[ym_number>=corona_start,ym_number]),
                              interval = "prediction")
        deaths_table[prov == PROV & m == M  & age_category==AGE, predicted_deaths := predictions[,1]]
        deaths_table[prov == PROV & m == M  & age_category==AGE, upper_bound := predictions[,3]]
      }
      else{
        predicted = mean(province_month[ym_number<corona_start ,n])
        confidence = confint(lm(n ~ 1, province_month[ym_number< corona_start]),level = 0.95)
        deaths_table[prov == PROV & m == M  & age_category==AGE, predicted_deaths := predicted]
        deaths_table[prov == PROV & m == M  & age_category==AGE, upper_bound := confidence[2]]
        deaths_table[prov == PROV & m == M  & age_category==AGE]
      }
    }
  }
}

#calculating excess mortality if the difference is significant
deaths_table[, excess_mortality := ifelse(n>upper_bound,n - predicted_deaths, 0)]
deaths_table[, excess_mortality_percentage := excess_mortality/predicted_deaths * 100]
#creating the heatmap
ggp =ggplot(deaths_table, aes(ym_number, prov)) +                           
  geom_tile(aes(fill = excess_mortality_percentage) )
ggp + scale_fill_gradient2(low = "white", high = "dark red" , mid = "white" , name = "excess mortality percentage" ,
                           limit = c(0,500))
#calculating total excess mortality for each province and in total
total_deaths_prov = deaths_table[, .(n =floor( sum(excess_mortality))) , prov]
total_deaths = sum(total_deaths_prov$n)
total_deaths
# we create a 99% confidece interval of excess mortality percentage. if a provinces percentage was below this confidence interval it means that province had excepcionaly better performance 
percentage_excess_death = deaths_table[, .(excess_mortality =  sum(excess_mortality) , 
                                           predicted = sum(predicted_deaths)) , prov]
percentage_excess_death = percentage_excess_death[, percentage := excess_mortality/predicted]
lower = confint(lm(percentage ~ 1, percentage_excess_death),level = 0.99)[1]
percentage_excess_death = percentage_excess_death[, better_performance := ifelse(percentage<lower,TRUE,FALSE)]
