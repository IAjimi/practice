#### Data Visualization, Healy ####
### Chapter 5 ###
#Prompt: Use subset() to only labels elections since 1992 subsetting by political party,
#or changing the colors of the points to reflect the winning party.

#some code based off the book!

party_colors <- c("#a17de8", "#2E74C0", "#CB454A", "#009933")

p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"

ggplot(elections_historic, aes(popular_pct, ec_pct)) + 
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +  
  geom_point(aes(color = win_party), size = 2, alpha = 0.8) +
  geom_text_repel(data = subset(elections_historic, year > 1992), aes(label = winner_label)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = x_label, y = y_label, title = p_title, subtitle = p_subtitle,
       caption = p_caption) +  scale_color_manual(values = party_colors) + guides(color = FALSE)



random <- sample(c(1:58), 5)

ggplot(elections_historic, aes(year, popular_margin)) + 
  geom_point(data = elections_historic, aes(year, popular_margin), color = "grey", size = 2) +
  geom_point(data = subset(elections_historic, election %in% random), aes(year, popular_margin, 
                                                                          color = win_party), size = 2) +
  geom_text_repel(data = subset(elections_historic, election %in% random), aes(label = winner_label)) +
  scale_y_continuous(labels = scales::percent) +  scale_color_manual(values = party_colors[-1]) +
  labs(x = "Year", y = "Popular Margin", title = "U.S. Presidential Elections",
       subtitle = "Change in Popular Margin Wins Over Time") + 
  guides(color = FALSE)
