submission = data.frame(UserID = old.test$UserID, Probability1 = predictions)
write.csv(submission, "submissionImputedAllLog.csv", row.names=FALSE)