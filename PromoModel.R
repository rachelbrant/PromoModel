#Promo Optimization Model 
# 4-9-24
#the data file is promo_data_old
detach(promo)
detach(promo_data_old)
summary(promo)
promo$Competing_Event_Binary<-as.factor(promo$Competing_Event_Binary)
promo$Number_Items_Given<-as.numeric(promo$Number_Items_Given)
promo$Opponent_Rank<-as.factor(promo$Opponent_Rank)
weekend<-subset(promo,WeekendYN=="No")
### october doesnt have any replicates so excluding oct
promo<-subset(promo,Month!="October")
promoyes<-subset(promo, Promo=="Y")
promono<-subset(promo,Promo=="N")

## Team performance
library(ggplot2)
ggplot(weekend) +
 aes(x = Winning_Percentage, y = GrossRev, colour = Promo) +
 geom_jitter() +
  geom_smooth() +
 scale_color_hue(direction = 1) +
 theme_minimal()

library(ggplot2)
ggplot(weekend) +
 aes(x = Winning_Percentage, y =PerCap, colour = Promo) +
 geom_point() +
geom_smooth()+
 scale_color_hue(direction = 1) +
 theme_minimal()
###Promos matter, but even more so when the team is playing worse, these two plots indicate that
model2 <- glm(PerCap ~ Winning_Percentage*Promo, data =weekend,family=Gamma(link="log"))
summary(model2)
par(mfrow = c(2, 2))
plot(model2)


##Day of week 
ggplot(promo) +
  aes(x = Day_of_Week, y = PerCap, fill = Promo) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

Days<-glm(Revenue~Day_of_Week*Promo,data=promo,family=Gamma(link="log"))
summary(Days)

ggplot(weekend) +
  aes(x = TMIN, y = PerCap, colour = Promo) +
  geom_smooth(se = TRUE) +
  scale_color_hue(direction = 1) +
  theme_minimal()
model3 <-glm(PerCap ~ TMIN * Promo, data = weekend,family=Gamma(link="log"))
summary(model3)
par(mfrow = c(2, 2))
plot(model3)
anova(model3, test = "Chisq")
##Temp min matters, more people come in cold weather when promo offered


ggplot(promoyes) +
  aes(x = CostperItem, y = Scans) +
  geom_point(colour = "#112446") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()
model_cost <- glm(Revenue ~ CostperItem, data = promoyes,family=Gamma(link="log"))
summary(model_cost)
##cost is significant, more scans the more expensive an item is

ggplot(weekend) +
  aes(x = Opponent_Rank, y =GrossRev, fill = Promo) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
model5 <-glm(GrossRev ~ Opponent_Rank*Promo, data =weekend,family=Gamma(link="log"))
summary(model5)



ggplot(weekend) +
  aes(x = Month, y = PerCap, fill = Promo) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()
model6 <-glm(Revenue ~ Month*Promo, data = promo,family=Gamma(link="log"))
summary(model6)
install.packages("emmeans")
library(emmeans)
emmeans(model5, pairwise ~ Promo | Opponent_Rank)
###Month matters but october doesnt have any replicates so excluding oct




library(ggplot2)
library(scales)
weekend$Promo <- factor(weekend$Promo, levels = c("N", "Y"))
ggplot(weekend, aes(x = Winning_Percentage, y = Revenue, color = Promo)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.3) +
  scale_color_manual(
    values = c("N" = "black", "Y" = "#d62728"),
    labels = c("No promo", "Promo")
  ) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "🎯 Promotions Drive Higher Revenue When Team Performance Slips",
    subtitle = "Revenue trends higher with promotions, especially at lower win rates",
    x = "Winning Percentage",
    y = "Revenue ($)",
    color = "Promotion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Cardinals Custom ggplot2 Theme + Slide Layout Tools

library(ggplot2)
library(scales)
#install.packages("ggtext")
library(ggtext)
#install.packages("showtext")
library(showtext)
#install.packages("patchwork")
library(patchwork)

# ✅ Font Setup (Google font: Lato)
font_add_google("Lato", "lato")
showtext_auto()

# 🎨 Cardinals Color Palette
cardinals_colors <- list(
  red = "#B71234",
  navy = "#0C2340",
  gold = "#85714D",
  gray = "#DDDDDD"
)

# ✅ Custom ggplot Theme
theme_cardinals <- function() {
  theme_minimal(base_family = "lato", base_size = 14) +
    theme(
      plot.title = element_markdown(face = "bold", size = 20),
      plot.subtitle = element_text(size = 14, color = "gray30"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = cardinals_colors$gray, size = 0.4),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# ✅ Demo Plot with Cardinals Style
ggplot(weekend, aes(x = Winning_Percentage, y = Revenue, color = Promo)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.5) +
  scale_color_manual(
    values = c("N" = cardinals_colors$red, "Y" = cardinals_colors$navy),
    labels = c("No Promo", "Promo")
  ) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "<span style='color:#1f77b4;'>Promo Nights</span> Outperform When the Team Struggles",
    subtitle = "Revenue trends higher with promotions, especially at lower win rates",
    x = "Winning Percentage",
    y = "Revenue ($)",
    color = "Promotion"
  ) +
  theme_cardinals()

# ✅ Demo Plot: Temperature vs Scans
promo$Promo <- factor(promo$Promo, levels = c("Y", "N"))

ggplot(promo, aes(x = TMIN, y = Revenue, color = Promo)) +
  geom_smooth(se = TRUE, linewidth = 1.5) +
  scale_color_manual(
    values = c("Y" = cardinals_colors$red, "N" = cardinals_colors$navy),
    labels = c("Promo", "No Promo")
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "<span style='color:#1f77b4;'>Colder Games</span> Show Stronger Promo Response",
    subtitle = "Scans increase with promotional offers in low temperatures",
    x = "Minimum Temperature (°F)",
    y = "Total Revenue ($)",
    color = "Promotion"
  ) +
  theme_cardinals()

# 🧱 Patchwork Layout Example
# plot1 <- ggplot(...) + theme_cardinals()
# plot2 <- ggplot(...) + theme_cardinals()
# (plot1 | plot2) / plot3


library(ggplot2)

ggplot(promo, aes(x = Scans, y = Revenue)) +
  geom_point(color = "#B22222", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(
    title = "Ticket Scans vs. Revenue",
    subtitle = "Do More Fans Always Mean More Revenue?",
    x = "Ticket Scans (Attendance)",
    y = "Total Ticket Revenue ($)",
    caption = "Source: Cardinals Promo Night Data"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

library(dplyr)
newdata<-subset(weekend,Year!="2022")
weekend %>%
  group_by(Promo) %>%
  summarise(mean_NetRev = mean(NetRev, na.rm = TRUE))%>%
summarise(diff_Promo = diff(mean_NetRev))
208177/121
(208177-300000)/121
1720/3
