final_pairs <-readRDS("Research/RA/colon-cancer/pair_assignment_big_68_balanced_after_check_first.rds")
first_m_freq <- get_freq_of_messages(final_pairs)
hist(first_m_freq[[1]][, 2])
hist(first_m_freq[[2]][, 2])

pairs_selected <- final_pairs[sample(1:length(final_pairs), 500, replace = FALSE)]
selected_first <- get_freq_of_messages(pairs_selected)
par(mfrow=c(1,2))
hist(selected_first[[1]][, 2])
hist(selected_first[[2]][, 2])

final_pairs <-readRDS("Research/RA/colon-cancer/pair_assignment_big_68_balanced_after_check_second.rds")
second_m_freq <- get_freq_of_messages(final_pairs)
hist(second_m_freq[[1]][, 2])
hist(second_m_freq[[2]][, 2])

pairs_selected <- final_pairs[sample(1:length(final_pairs), 500, replace = FALSE)]
selected_second <- get_freq_of_messages(pairs_selected)
par(mfrow=c(1,2))
hist(selected_second[[1]][, 2])
hist(selected_second[[2]][, 2])


