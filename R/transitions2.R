# get_ref_rank <- function(proposer, rejecter, window_size) {
#   ranks <-
#     do.call(
#       cbind,
#       sapply(proposer,
#         function(y) {
#           rejecter[order(abs(y - rejecter))]
#         },
#         simplify = FALSE
#       )
#     )
#
#   distances <-
#     do.call(
#       cbind,
#       sapply(proposer,
#         function(y) {
#           abs(y - rejecter)[order(abs(y - rejecter))]
#         },
#         simplify = FALSE
#       )
#     )
#
#   ranks[distances > window_size] <- NA
#   return(ranks)
# }
# get_m_rank <- function(test_array) {
#   all_m <-
#     unique(c(test_array)[!is.na(c(test_array))])
#
#   result <- data.frame(
#     do.call(
#       rbind,
#       sapply(
#         all_m,
#         function(x) {
#           # x <- all_m[1]
#           sapply(
#             data.frame(test_array),
#             function(y) {
#               sum(x == y, na.rm = TRUE)
#             }
#           )
#         },
#         simplify = FALSE
#       )
#     )
#   )
#
#   drop <-
#     sapply(result, function(x) all(is.na(x) | x == 0))
#   result <- result[ ,!drop]
#   result$count  <- rowSums(result, na.rm = TRUE)
#   result$m <- all_m
#   result
# }
# assign_sole_and_first <- function(results, test_array, rank_array) {
#
#   # Remove entries that are too far away
#   test_array <-
#     ifelse(rank_array$distance > window_size, NA, rank_array$rank)
#
#   # Remove entries that have already been assigned
#   existing_check <- !is.na(results$assigned_m[seq(nrow(test_array))])
#
#   if (any(existing_check)) {
#     test_array <- test_array[-which(existing_check), ]
#   }
#
#   # Table all m and the ranks they occupy
#   m_array <- get_m_rank(test_array)
#
#   # Assign any that occupy only index 1 for a single n
#   to_assign <-
#     (m_array$count == 1) & (m_array[ ,1] == 1)
#   to_assign <- m_array[to_assign, "m"]
#
#   matches <-
#     match(to_assign, test_array[ ,1])
#
#   results$assigned_m[results$n == matches] <- to_assign
#   return(results)
# }
#
# # Number of transitions
#   n <- sum(reference)
#   m <- sum(prediction)
#
#
# # Data frame to keep track of assignments
#   results <-
#     data.frame(
#       n = seq(ni),
#       assigned_m = NA,
#       stringsAsFactors = FALSE,
#       row.names = NULL
#   )
#
# # Arrays giving ranks and distances
#   rank_array <- get_n_rank(ni, mi)
#
# # Remove entries that are too far away
#   test_array <-
#     ifelse(rank_array$distance > window_size, NA, rank_array$rank)
#
# # Remove cells, table m and rank, and assign where possible
#   counter1 <- 0
#   repeat {
#
#     counter1 <- counter1 + 1
#
#     new_results <-
#       assign_sole_and_first(results, test_array, rank_array)
#
#     if (!identical(new_results, results)) {
#       results <- new_results
#     } else {
#       rm(new_results)
#       test_array <- test_array[is.na(results$assigned_m), ]
#       rank_array <-
#         lapply(rank_array, function(x) x[is.na(results$assigned_m), ])
#       break
#     }
#
#   }
#
#   while (nrow(test_array) > 0) {
#
#   }
