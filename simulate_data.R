#simulates diabetes data 
library(tidyverse)
patient_no = 1500
beta_dist = rbeta(10000,2,5)
diabetes = data.frame(patient_id = 1:patient_no, 
                      hba1c_normal = sample(c("Normal", "Abnormal"),
                                            size = patient_no, 
                                            replace = T, 
                                            prob = c(0.3, 0.7)))

diabetes$hba1c_value = sapply(diabetes$hba1c_normal, function(x) { 
  if(x == "Normal") { 
    round(sample(seq(4, 5.6, 0.1) + sample(seq(0, 0.1, 0.01), size = 1), size = 1), digits = 2)
  } else { 
    round(5.6 + 10*sample(beta_dist, size = 1), digits = 2)  
  }
  })

diabetes$hba1c_months_ago = rnorm(patient_no, 8, 2)
diabetes$foot_exam_result = sample(c("Ulcers", "No Ulcers"), size = patient_no, 
                                   prob = c(0.1, 0.9), replace = T)
diabetes$foot_exam_months_ago = diabetes$hba1c_months_ago
diabetes = diabetes %>%  mutate(foot_exam_past_due = ifelse(foot_exam_months_ago > 6, 
                                                                "Past Due", "Not Due"), 
                                hba1c_past_due = ifelse(hba1c_months_ago > 3, 
                                                        "Past Due", "Not Due"))
write_csv(diabetes, "diabetes_dataset.csv")


#simulate data over time
diabetes = read_csv("diabetes_dataset.csv")

library(lubridate)
df_start = ymd("2017-01-01")
df_end = ymd("2022-05-26")
date_range = seq.Date(df_start, df_end, by = "month")
patients_due = data.frame(patient_id = diabetes$patient_id, 
                          ab = diabetes$hba1c_normal, 
                          due = "Due", 
                          last_test = ymd(NA))

normal_sample = rnorm(10000, mean = 4, sd = 2)
normal_sample = normal_sample[normal_sample > 2]

abnormal_sample = rnorm(10000, mean = 8, sd = 2)
abnormal_sample = abnormal_sample[abnormal_sample > 5]
diabetes_time = vector("list", length(date_range))
for(i in 1:length(date_range)) { 
  cat(i, "of", length(date_range), "\n")
  month_data = tibble()
  for(j in 1:length(patients_due$patient_id)) {
    #change to "Due" if more than 90 days have passed without a test
    if(patients_due$due[j] != "Due" & !is.na(patients_due$last_test[j])) { 
      if(as.numeric(date_range[i] - patients_due$last_test[j]) >= 90) { 
        patients_due$due[j] = "Due"
        }
    }
    if(patients_due$due[j] == "Due") {
      test = sample(c(0, 1), size  = 1)
      #50/50 chance that any patient received a test
      if(test == 1) { 
        if(patients_due$ab[j] == "Normal") { 
          a1c = sample(normal_sample, size = 1)
        } else{ 
          a1c = sample(abnormal_sample, size = 1)  
        }
        temp = tibble(patient_id = patients_due$patient_id[j], 
                   test_date = ymd(paste0(year(date_range[i]), "-", 
                                          month(date_range[i]), "-", 
                                          sample(1:28, size = 1))),
                   a1c_value = a1c
        )
        
        month_data = bind_rows(month_data, 
                          temp)
        
        patients_due$due[j] = "Not Due"
        patients_due$last_test[j] = temp$test_date
        
      } else { 
          next
        }
    } else { 
      next
    
      }
    
  }
  diabetes_time[[i]] = month_data
}

diabetes_time = purrr::reduce(diabetes_time, bind_rows) %>%  
  left_join(diabetes %>%  
              select(patient_id, hba1c_normal))


write_csv(diabetes_time, "a1c_time_series.csv")

#simulate readmission


