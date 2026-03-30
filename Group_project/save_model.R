# ── Run this script ONCE in RStudio to generate the pre-saved model files ──
# After running, you will have:
#   - df_clustering.rds
#   - lca_model.rds
# Place both files in the same folder as app.R before deploying.

library(poLCA)
library(tidyverse)

df <- read_csv("customer_data.csv")

df_clustering <- df %>%
  mutate(
    savings_account    = ifelse(savings_account    %in% c(TRUE, "TRUE"), 2, 1),
    credit_card        = ifelse(credit_card        %in% c(TRUE, "TRUE"), 2, 1),
    personal_loan      = ifelse(personal_loan      %in% c(TRUE, "TRUE"), 2, 1),
    investment_account = ifelse(investment_account %in% c(TRUE, "TRUE"), 2, 1),
    insurance_product  = ifelse(insurance_product  %in% c(TRUE, "TRUE"), 2, 1),
    bill_payment_user  = ifelse(bill_payment_user  %in% c(TRUE, "TRUE"), 2, 1),
    auto_savings       = ifelse(auto_savings_enabled %in% c(TRUE, "TRUE"), 2, 1)
  ) %>%
  mutate(income_cat = case_when(
    income_bracket == "Low"       ~ "Low",
    income_bracket == "Medium"    ~ "Medium",
    income_bracket == "High"      ~ "High",
    income_bracket == "Very High" ~ "Very_High",
    TRUE ~ "Unknown"
  )) %>%
  mutate(segment_cat = case_when(
    customer_segment == "inactive"   ~ "Inactive",
    customer_segment == "occasional" ~ "Occasional",
    customer_segment == "regular"    ~ "Regular",
    customer_segment == "power"      ~ "Power",
    TRUE ~ "Unknown"
  )) %>%
  mutate(clv_cat = recode(clv_segment,
                          `Bronze` = "Bronze", `Silver` = "Silver",
                          `Gold` = "Gold", `Platinum` = "Platinum"
  )) %>%
  mutate(login_cat = cut(app_logins_frequency,
                         breaks = c(-Inf, 10, 20, 35, Inf),
                         labels = c("Low", "Medium", "High", "Very_High"))) %>%
  mutate(feature_cat = cut(feature_usage_diversity,
                           breaks = c(-Inf, 1, 3, 5, Inf),
                           labels = c("1", "2-3", "4-5", "6+"))) %>%
  mutate(tx_count_cat = cut(tx_count,
                            breaks = c(-Inf, 15, 30, 60, Inf),
                            labels = c("Low", "Medium", "High", "Very_High"))) %>%
  mutate(avg_tx_cat = cut(avg_tx_value,
                          breaks = c(-Inf, 1000000, 3000000, 8000000, Inf),
                          labels = c("Low", "Medium", "High", "Very_High"))) %>%
  mutate(tx_freq_cat = cut(transaction_frequency,
                           breaks = c(-Inf, 0.03, 0.06, 0.15, Inf),
                           labels = c("Infrequent", "Occasional", "Regular", "Frequent"))) %>%
  mutate(weekend_cat = cut(weekend_transaction_ratio,
                           breaks = c(-Inf, 0.2, 0.35, 0.5, Inf),
                           labels = c("Low", "Medium", "High", "Very_High"))) %>%
  mutate(satisfaction_cat = cut(satisfaction_score,
                                breaks = c(-Inf, 3, 4, 5, Inf),
                                labels = c("Low", "Medium", "High", "Very_High"))) %>%
  mutate(nps_cat = cut(nps_score,
                       breaks = c(-Inf, -50, 0, Inf),
                       labels = c("Detractor", "Passive", "Promoter"))) %>%
  mutate(sentiment_cat = case_when(
    feedback_sentiment == "Positive" ~ "Positive",
    feedback_sentiment == "Neutral"  ~ "Neutral",
    feedback_sentiment == "Negative" ~ "Negative",
    TRUE ~ "Neutral"
  )) %>%
  mutate(churn_cat = cut(churn_probability,
                         breaks = c(-Inf, 0.25, 0.40, Inf),
                         labels = c("Low", "Medium", "High"))) %>%
  select(customer_id,
         income_cat, clv_cat,
         savings_account, credit_card, personal_loan,
         investment_account, insurance_product,
         bill_payment_user, auto_savings,
         login_cat, feature_cat,
         tx_count_cat, tx_freq_cat, weekend_cat,
         satisfaction_cat, nps_cat, sentiment_cat,
         churn_cat,
         segment_cat, avg_tx_cat) %>%
  drop_na()

df_clustering[-1] <- lapply(df_clustering[-1], factor)

# Save prepared dataset
saveRDS(df_clustering, "df_clustering.rds")
cat("✓ df_clustering.rds saved\n")

# Run LCA model (this takes 1-2 minutes)
set.seed(1234)

f <- as.formula(cbind(
  income_cat, clv_cat,
  savings_account, credit_card, personal_loan,
  investment_account, insurance_product,
  bill_payment_user, auto_savings,
  login_cat, feature_cat,
  tx_count_cat, tx_freq_cat, weekend_cat,
  satisfaction_cat, nps_cat, sentiment_cat,
  churn_cat
) ~ 1)

cat("Running LCA model — this takes 1-2 minutes...\n")
lca_model <- poLCA(f, df_clustering, nclass = 5, nrep = 5, maxiter = 5000)

# Save model
saveRDS(lca_model, "lca_model.rds")
cat("✓ lca_model.rds saved\n")
cat("Done! You can now run app.R\n")

