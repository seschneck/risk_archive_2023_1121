# roc_1_week <- roc_data
# roc_1day <- roc_data
# roc_1hour <- roc_data



roc_1week <- roc_1week %>% 
  mutate(model = "1week")

roc_1day <- roc_1day %>% 
  mutate(model = "1day")


roc_1hour <- roc_1hour %>% 
  mutate(model = "1hour")


roc_data <- roc_1hour %>% 
  rbind(roc_1day) %>% 
  rbind(roc_1week)


model_colors <- c("1week" = "purple", "1day"="blue", "1hour"="orange")

# week only
roc_data %>% 
  filter(model == "1week") %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(size = 1.25) +
  geom_abline(lty = 3) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Specificity",
       y = "Sensitivity") +
  scale_x_continuous(breaks = seq(0,1,.25),
                     labels = sprintf("%.2f", seq(1,0,-.25))) +
  scale_color_manual(values = model_colors) +
  theme(axis.text = element_text(size = rel(1.50)), 
        axis.title = element_text(size = rel(1.75)))


ggsave(file.choose(), width = 6.5, height = 6.5, units = "in", device = "png",  dpi = 100)


roc_data %>% 
  filter(model == "1week" | model == "1day") %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(size = 1.25) +
  geom_abline(lty = 3) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Specificity",
       y = "Sensitivity") +
  scale_x_continuous(breaks = seq(0,1,.25),
                     labels = sprintf("%.2f", seq(1,0,-.25))) +
  scale_color_manual(values = model_colors) +
  theme(axis.text = element_text(size = rel(1.50)), 
        axis.title = element_text(size = rel(1.75)))


ggsave(file.choose(), width = 6.5, height = 6.5, units = "in", device = "png",  dpi = 100)


roc_data %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(size = 1.25) +
  geom_abline(lty = 3) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Specificity",
       y = "Sensitivity") +
  scale_x_continuous(breaks = seq(0,1,.25),
                     labels = sprintf("%.2f", seq(1,0,-.25))) +
  scale_color_manual(values = model_colors) +
  theme(axis.text = element_text(size = rel(1.50)), 
        axis.title = element_text(size = rel(1.75)))


ggsave(file.choose(), width = 6.5, height = 6.5, units = "in", device = "png",  dpi = 100)


