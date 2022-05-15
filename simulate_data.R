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
