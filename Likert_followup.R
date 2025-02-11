require(likert)

survey <- read.csv("/Users/is339/Downloads/Survey_follow_up.csv", header = T)
colnames(survey) <- c("Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q4_7", "Q4_8")

# set levels and their ordering
mylevels <- c("Discordo fortemente", "Discordo", "Neutro", "Concordo", "Concordo fortemente")

# columns to generate the chart
columns = c("Q4_1", "Q4_2", "Q4_3", "Q4_4", "Q4_5", "Q4_6", "Q4_7", "Q4_8")

# likert scale
filtered = survey[columns]

# make sure all columns have all the levels (otherwise 'likert' function breaks)
for (i in seq_along(filtered)) {
  filtered[, i] <- factor(filtered[, i], levels = mylevels)
}

x <- likert(filtered)

### GENERATE DEFINITIONS.TEX
completely=5
to_a_great_extent=4
somewhat=3
very_little=2
not_at_all=1



### GENERATE FIGURES

# figure 1
columns_fig1 = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7") # order in the paper
filtered = survey[columns_fig1]

# make sure all columns have all the levels (otherwise 'likert' function breaks)
for (i in seq_along(filtered)) {
  filtered[, i] <- factor(filtered[, i], levels = mylevels)
}


x <- likert(filtered)
x$results$Item <- c("OSS contributors who experience interpersonal challenges tend to feel unwelcome in their communities",
                    "The lack of response or rejection of contributions is a prevalent interpersonal challenge that affects all demographics",	
                    "Gender minorities and people with disabilities experience interpersonal challenges more frequently than their counterparts and feel even less welcome than their counterparts when they experience those interpersonal challenges.",	
                    "Sexual harassment and stalking are particularly significant for gender minorities, with much higher odds of experiencing these challenges compared to men.",	
                    "Conflicts or tensions with other contributors often correlate with unwelcoming language and stereotyping of minority groups.",	
                    "Contributors from minority groups feel unwelcome due to discrimination, microaggressions, and biases during in-person and online interactions.",	
                    "Majority groups (e.g. men, white, straight) perceive that diversity initiatives might lead to more segregation and their own contributions being undervalued because they are not minorities.")
colnames(x$items) <- c("OSS contributors who experience interpersonal challenges tend to feel unwelcome in their communities",
                       "The lack of response or rejection of contributions is a prevalent interpersonal challenge that affects all demographics",	
                       "Gender minorities and people with disabilities experience interpersonal challenges more frequently than their counterparts and feel even less welcome than their counterparts when they experience those interpersonal challenges.",	
                       "Sexual harassment and stalking are particularly significant for gender minorities, with much higher odds of experiencing these challenges compared to men.",	
                       "Conflicts or tensions with other contributors often correlate with unwelcoming language and stereotyping of minority groups.",	
                       "Contributors from minority groups feel unwelcome due to discrimination, microaggressions, and biases during in-person and online interactions.",	
                       "Majority groups (e.g. men, white, straight) perceive that diversity initiatives might lead to more segregation and their own contributions being undervalued because they are not minorities.")

# x$results$Item <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")
# colnames(x$items) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")

pdf("survey-likert-contributors.pdf", width = 12, height = 3.5)
likert.bar.plot(x, centered = T, group.order = x$results$Item, legend.position = "") + theme(text = element_text(size = rel(4.5)), axis.title.x = element_blank())
dev.off()