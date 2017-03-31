# Replace categorical character fields with text fields

# example
df <- rbind("very bad", "bad", "ok","good", "excellent")
df <- as.data.frame(df)
names(df) <- "rating"
df$number_rating[df$rating=="very bad"] <- "1" 
df$number_rating[df$rating=="bad"] <- "2" 
df$number_rating[df$rating=="ok"] <- "3" 
df$number_rating[df$rating=="good"] <- "4" 
df$number_rating[df$rating=="excellent"] <- "5" 

