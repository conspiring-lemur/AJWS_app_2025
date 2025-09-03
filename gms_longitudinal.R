library(ggplot2)
library(plotly)
library(dplyr)

gms_region_dat <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "gms_longitudinal", startRow = 1, rows = 1:6)


region_long <- gms_region_dat %>%
  pivot_longer(
    cols = starts_with("FY"), # Select columns starting with "Sales_"
    names_to = "Year",          # New column for the variable names
    values_to = "Amount"           # New column for the values
  )

region_long <- region_long %>% mutate(Year = str_remove(Year, "FY") %>% as.numeric,
                                      Dollar.Amount = scales::dollar(Amount),
                                      tooltip_text = paste0(
                                        "Year: ", Year, "<br>",
                                        "Amount: ", scales::dollar(Amount), "<br>",
                                        "Region: ", Region
                                      ))


g_region <- ggplot(region_long) +
  geom_line(aes(x = Year, y = Amount, group = Region, color = Region, text = tooltip_text), linewidth = 2) +
  theme(title = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(0, 10000000), 
                     breaks = c(0, 2000000, 4000000, 6000000, 8000000, 10000000),
                     labels = scales::dollar_format()) +
  scale_x_continuous(limits = c(2013, 2025), 
                     breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
  theme_bw() +
  ggtitle("Core Grants by Region")

p_region <- ggplotly(g_region, tooltip = "text") %>% 
  layout(
  yaxis = list(
    tickformat = "$,",
    exponentformat = "none"
  )
)

p_region


gms_issue_area_dat <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "gms_longitudinal", startRow = 10, rows = 10:15)

issue_long <- gms_issue_area_dat %>%
  pivot_longer(
    cols = starts_with("FY"), # Select columns starting with "Sales_"
    names_to = "Year",          # New column for the variable names
    values_to = "Amount"           # New column for the values
  )

issue_long <- issue_long %>% mutate(Year = str_remove(Year, "FY") %>% as.numeric,
                                      Dollar.Amount = scales::dollar(Amount),
                                      tooltip_text = paste0(
                                        "Year: ", Year, "<br>",
                                        "Amount: ", scales::dollar(Amount), "<br>",
                                        "Issue Area: ", Issue.Area
                                      ))


g_issue <- ggplot(issue_long) +
  geom_line(aes(x = Year, y = Amount, group = Issue.Area, color = Issue.Area, text = tooltip_text), linewidth = 2) +
  theme(title = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(0, 8000000), 
                     breaks = c(0, 500000, 2000000, 3500000, 5000000, 6500000, 8000000),
                     labels = scales::dollar_format()) +
  scale_x_continuous(limits = c(2013, 2025), 
                     breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
  theme_bw() +
  ggtitle("Core Grants by Issue Area")

p_issue <- ggplotly(g_issue, tooltip = "text") %>% 
  layout(
    yaxis = list(
      tickformat = "$,",
      exponentformat = "none"
    )
  )

p_issue


gms_locality_dat <- read.xlsx("FY25 AJWS Data_3JUL25.xlsx", sheet = "gms_longitudinal", startRow = 18, rows = 18:22)

locality_long <- gms_locality_dat %>%
  pivot_longer(
    cols = starts_with("FY"), # Select columns starting with "Sales_"
    names_to = "Year",          # New column for the variable names
    values_to = "Amount"           # New column for the values
  )

locality_long <- locality_long %>% mutate(Year = str_remove(Year, "FY") %>% as.numeric,
                                    Dollar.Amount = scales::dollar(Amount),
                                    tooltip_text = paste0(
                                      "Year: ", Year, "<br>",
                                      "Amount: ", scales::dollar(Amount), "<br>",
                                      "locality Area: ", Locality
                                    ))

locality_long %>% print(n = Inf)

g_locality <- ggplot(locality_long) +
  geom_line(aes(x = Year, y = Amount, group = Locality, color = Locality, text = tooltip_text), linewidth = 2, alpha = .5) +
  theme(title = element_text(hjust = .5)) +
  scale_y_continuous(limits = c(0, 10000000), 
                     breaks = c(0, 2000000, 4000000, 6000000, 8000000, 10000000),
                     labels = scales::dollar_format()) +
  scale_x_continuous(limits = c(2017, 2025), 
                     breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)) +
  theme_bw() +
  ggtitle("Core Grants by Locality")

p_locality <- ggplotly(g_locality, tooltip = "text") %>% 
  layout(
    yaxis = list(
      tickformat = "$,",
      exponentformat = "none"
    )
  )

p_locality
