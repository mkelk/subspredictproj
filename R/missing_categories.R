predict(new_model, newdata = churntable)
num_previous_months_breaks <- c(0, 1, 2, 3, 5, 8, 11, 14, 26, 38)

all_combinations <- subscriptions %>%
  expand(siteverkey_cat,  num_previous_months, months, paymentperiodchosenatstart) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(num_previous_months_binned = as.numeric(as.character(cut(num_previous_months, 
                                                                  breaks = c(-Inf, num_previous_months_breaks, Inf), 
                                                                  labels = c(num_previous_months_breaks, max(num_previous_months_breaks) + 1)))),
         siteverkey_cat2 = if_else(num_previous_months_binned <= age_to_join_sitevers, siteverkey_cat, 'MUT'),
         chosen_subs_length = if_else( (siteverkey_cat == "SS" & num_previous_months == 0) | (siteverkey_cat != "SS" & num_previous_months == 1), 
                                       as.character(paymentperiodchosenatstart),
                                       'gen')) %>%
  group_by(siteverkey_cat2, num_previous_months_binned, months, chosen_subs_length) %>%
  summarise() %>%
  mutate(subscription_summary_no_market = sprintf("ssc-%s_ac-%d_m-%d_ccsl-%s", siteverkey_cat2, num_previous_months_binned, months, chosen_subs_length)) %>%
  mutate(is_in_training = subscription_summary_no_market %in% churntable_no_zeros$subscription_summary_no_market)

all_combinations %>%
  group_by(months, is_in_training) %>%
  summarise(cnt = n()) %>%
  mutate(percent = cnt/sum(cnt),
         text = sprintf('%.0f%%', percent*100)) %>%
  ggplot(aes(is_in_training, percent, fill = is_in_training)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = text), nudge_y = 0.05) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Percent of missing categories',
       subtitle = 'Splitted by subscription length') +
  facet_wrap(~months) +
  theme_bw() +
  theme(legend.position = 'bottom')

all_combinations %>%
  write_csv('categories_matrix.csv')

View(all_combinations)
