library(tidyverse)
library(plotly)
library(ggtext)
library(scales)
library(ggstream)


# Salary ------------------------------------------------------------------


salary_data <- tibble(
  order = seq(6),
  label = c(
    'B-1',
    'B-Max',
    'M-1',
    'M-Max',
    'Max-1',
    'Max-Max'
  ),
  type = c('Bachelors, Step 1', 
           # 'Bachelors Step 5', 
           'Bachelors, Highest Step', 
           'Masters, Step 1', 
           # 'Masters Step 5', 
           'Masters, Highest Step', 
           'Highest, Lane Step 1', 
           # 'Highest Lane Step 5', 
           'Highest Lane, Highest Step'),
  lowest = c(42800, 
             # 48151,
             49500, 
             45684, 
             # 51082, 
             70920, 
             49500, 
             # 56290, 
             76197),
  average = c(52825,
              # 62881, 
              83746, 
              57181, 
              # 67849, 
              92297, 
              64964, 
              # 76320,
              102938),
  highest = c(70061,
              # 91072,
              113002, 
              75465, 
              # 96951, 
              119510,
              87157,
              # 112607,
              135065), 
)

p <- ggplot(salary_data) +
  geom_segment(aes(
    x = reorder(label, order),
    xend = reorder(label, order),
    y = lowest,
    yend = highest
  ),
  color = "grey") +
  geom_point(
    aes(x = reorder(label, order), y = lowest),
    color = '#4E0F53D9' ,
    size = 6.5
  ) +
  geom_point(
    aes(x = reorder(label, order), y = highest),
    color = '#DC9542D9',
    size = 6.5
  ) +
  geom_text(
    aes(x = reorder(label, order),
        y = highest+8000, 
        family = "Arial Rounded MT Bold",
        label = label)
  )+
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Arial Rounded MT Bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank()
        ) +
  xlab("") +
  ylab("Salary") +
  # annotate(
  #   'text',
  #   x = 2.2,
  #   y = 32000,
  #   family = "Arial Rounded MT Bold",
  #   label = 'Click on the dots to learn more!'
  #          )+
  coord_cartesian(clip="off") +
  scale_y_continuous(labels = numberFormattR::suffix_formatter_0, 
                     limits = c(32000,145000))+
  scale_x_discrete(labels = label_wrap(12),
                   guide = guide_axis(n.dodge = 2))+
  expand_limits(x= c(0, length(levels(as.factor(salary_data$label))) + 1))




plotly_plot <- ggplotly(p) %>% 
  plotly::layout(
    xaxis = list(title = "", 
                 fixedrange = T, 
                 automargin = T
                 ),
    margin = list(l = 10, r = 10, b = 40, t = 10),
    yaxis = list(title = "", fixedrange = T, automargin = T)
    ) %>% add_annotations(
      x=7,
      y=26500,
      xref = "x",
      yref = "y",
      text = "Click on the dots to learn more!",
      xanchor = 'right',
      showarrow = F
    )

plotly_plot$x$data[[2]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
"Lowest:", scales::dollar(salary_data$lowest))

plotly_plot$x$data[[3]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Highest:", scales::dollar(salary_data$highest))

plotly_plot$x$data[[4]]$hovertext <- paste(
  salary_data$type
  # "Highest:", scales::dollar(salary_data$highest)
  )



htmlwidgets::saveWidget(config(plotly_plot, showLink = F, displayModeBar = FALSE),
                        "~/Documents/Github/demo/assets/plots/graph.html")


# Salary Trend ------------------------------------------------------------


trend_data <- tibble(
  year = c(
    '2016-17',
    '2017-18',
    '2018-19',
    '2019-20',
    '2020-21',
    '2021-22',
    '2022-23',
    '2023-24',
    '2024-25'
  ),
  lab1 = c(
    '16-17',
    '',
    '18-19',
    '',
    '20-21',
    '',
    '22-23',
    '',
    '24-25'
  ),
  lab2 = c(
    '',
    '17-18',
    '',
    '19-20',
    '',
    '21-22',
    '',
    '23-24',
    ''
  ),
  low = c(
    34151,
    34380,
    35904,
    35904,
    37705,
    40300,
    40703,
    42801,
    44252
  ),
  high = c(
    117327,
    117327,
    122964,
    122964,
    125116,
    128244,
    131450,
    135065,
    136932
  )
) %>% 
  mutate(
    increase_low = low/dplyr::lag(low)-1,
    increase_high = high/dplyr::lag(high)-1
  )

p <- ggplot(trend_data, aes(x = year, group = 1)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, fill = '#27396A') +
  geom_line(aes(y = high)) +
  geom_line(aes(y = low))+
  geom_point(aes(y = high),
             size = 3,
             color = '#DC9542' ,
             ) +
  geom_point(aes(y = low),
             size = 3,
             color = '#4E0F53' ,
             ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Arial Rounded MT Bold"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_blank(),
        # panel.grid.minor.x = element_blank()
        panel.grid.major.x = element_blank()
  ) +
  coord_cartesian(clip="off") +
  scale_y_continuous(labels = numberFormattR::suffix_formatter_0,
                     limits = c(28000, 145000))+
  geom_text(
    aes(y = high+6000, 
        family = "Arial Rounded MT Bold",
        label = lab1)
  )+
  # geom_text(
  #   aes(y = low-4000, 
  #       family = "Arial Rounded MT Bold",
  #       label = lab2)
  # )+
  expand_limits(x= c(0.1, length(levels(as.factor(trend_data$year))) + 0.8))


plotly_plot <- ggplotly(p) %>% 
  plotly::layout(
    xaxis = list(title = "", 
                 fixedrange = T, 
                 automargin = T
    ),
    margin = list(l = 10, r = 10, b = 40, t = 10),
    yaxis = list(title = "", fixedrange = T, automargin = T)
  ) %>% 
  add_annotations(
    x=9.8,
    y=122000,
    xref = "x",
    yref = "y",
    text = "+16.7%",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#156e1b', size = 17)
  ) %>%
  add_annotations(
    x=9.8,
    y=52000,
    xref = "x",
    yref = "y",
    text = "+29.7%",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#156e1b', size = 17)
  ) %>% 
  add_annotations(
    x=9.8,
    y=26500,
    xref = "x",
    yref = "y",
    text = "Click on the dots to learn more!",
    xanchor = 'right',
    showarrow = F
  )

plotly_plot$x$data[[2]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Highest:", scales::dollar(trend_data$high), "<br>",
  'Increase:', scales::percent(round(trend_data$increase_high, 3))
  )

plotly_plot$x$data[[3]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Lowest:", scales::dollar(trend_data$low), "<br>",
  'Increase:', scales::percent(round(trend_data$increase_low, 3))
  )

plotly_plot$x$data[[4]]$hovertext <- paste(
  # "Step:",salary_data$type, "<br>",
  trend_data$year)


htmlwidgets::saveWidget(config(plotly_plot, showLink = F, displayModeBar = FALSE),
                        "~/Documents/Github/demo/assets/plots/trend.html")


# Low and high -----------------------------------------------------------

lowe_high_data <- read_csv('salary_info_full_june.csv') %>% 
  filter(year %in% c('2020-21', '2021-22', '2022-23', '2023-24', '2024-25')) %>% 
  group_by(year) %>% 
  summarise(
    starting_b = min(lowest_step_bachelors, na.rm = T),
    starting_m = min(lowest_step_masters, na.rm = T),
    starting_h = min(lowest_step_highest_lane, na.rm = T),
    top_b = max(highest_step_bachelors, na.rm = T),
    top_m = max(highest_step_masters, na.rm = T),
    top_h = max(highest_step_highest_lane, na.rm = T),
  ) %>% 
  mutate(
    lab1 = c(
      '2020-21',
      '',
      '',
      '',
      '2024-25'
    ),
    increase_bmin = starting_b/lag(starting_b) -1,
    increase_mmin = starting_m/lag(starting_m) -1,
    increase_hmin = starting_h/lag(starting_h) -1,
    increase_bmax = top_b/lag(top_b) -1,
    increase_mmax = top_m/lag(top_m) -1,
    increase_hmax = top_h/lag(top_h) -1
  )



p <- ggplot(lowe_high_data, aes(x = year, group = 1)) +
  geom_line(aes(y = starting_b),
            color = '#4E0F53'
            ) +
  geom_line(aes(y = starting_m),
            color = '#4E0F53' ,
            )+
  geom_line(aes(y = starting_h),
            color = '#DC9542' 
            )+
  geom_point(aes(y = starting_b),
             size = 3,
             color = '#4E0F53' ,
  ) +
  geom_point(aes(y = starting_m),
             size = 3,
             color = '#27396A' ,
  ) +
  geom_point(aes(y = starting_h),
             size = 3,
             color = '#DC9542' 
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Arial Rounded MT Bold"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_blank(),
        # panel.grid.minor.x = element_blank()
        panel.grid.major.x = element_blank()
  ) +
  coord_cartesian(clip="off") +
  scale_y_continuous(labels = numberFormattR::suffix_formatter_0)+
  geom_text(
    aes(y = starting_h+1000, 
        family = "Arial Rounded MT Bold",
        label = lab1)
  )+
  # geom_text(
  #   aes(y = low-4000, 
  #       family = "Arial Rounded MT Bold",
  #       label = lab2)
  # )+
  expand_limits(x= c(0.1, length(levels(as.factor(lowe_high_data$year))) + 1.5))



plotly_plot <- ggplotly(p) %>% 
  plotly::layout(
    xaxis = list(title = "", 
                 fixedrange = T, 
                 automargin = T
    ),
    margin = list(l = 10, r = 10, b = 40, t = 10),
    yaxis = list(title = "", fixedrange = T, automargin = T)
  ) %>% 
  add_annotations(
    x=6,
    y=49814,
    xref = "x",
    yref = "y",
    text = "Highest Lane\nStep 1",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#DC9542', size = 13)
  ) %>%
  add_annotations(
    x=6,
    y=46068,
    xref = "x",
    yref = "y",
    text = "Masters\nStep 1",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#27396A', size = 13)
  ) %>% 
  add_annotations(
    x=6,
    y=42752,
    xref = "x",
    yref = "y",
    text = "Bachelors\nStep 1",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#4E0F53', size = 13)
  ) 

plotly_plot$x$data[[1]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Starting Bachelors:", scales::dollar(lowe_high_data$starting_b), "<br>",
  'Increase:', scales::percent(round(lowe_high_data$increase_bmin, 3))
)

plotly_plot$x$data[[2]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Starting Masters:", scales::dollar(lowe_high_data$starting_m), "<br>",
  'Increase:', scales::percent(round(lowe_high_data$increase_mmin, 3))
)

plotly_plot$x$data[[3]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Starting Highest Lane:", scales::dollar(lowe_high_data$starting_h), "<br>",
  'Increase:', scales::percent(round(lowe_high_data$increase_hmin, 3))
)

plotly_plot$x$data[[4]]$hovertext <- paste(
  # "Step:",salary_data$type, "<br>",
  lowe_high_data$year)

htmlwidgets::saveWidget(config(plotly_plot, showLink = F, displayModeBar = FALSE),
                        "~/Documents/Github/demo/assets/plots/starting.html")


# highest -----------------------------------------------------------------


p <- ggplot(lowe_high_data, aes(x = year, group = 1)) +
  geom_line(aes(y = top_b),
            color = '#4E0F53'
  ) +
  geom_line(aes(y = top_m),
            color = '#4E0F53' ,
  )+
  geom_line(aes(y = top_h),
            color = '#DC9542' 
  )+
  geom_point(aes(y = top_b),
             size = 3,
             color = '#4E0F53' ,
  ) +
  geom_point(aes(y = top_m),
             size = 3,
             color = '#27396A' ,
  ) +
  geom_point(aes(y = top_h),
             size = 3,
             color = '#DC9542' 
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Arial Rounded MT Bold"),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_blank(),
        # panel.grid.minor.x = element_blank()
        panel.grid.major.x = element_blank()
  ) +
  coord_cartesian(clip="off") +
  scale_y_continuous(labels = numberFormattR::suffix_formatter_0)+
  geom_text(
    aes(y = top_h+3000, 
        family = "Arial Rounded MT Bold",
        label = lab1)
  )+
  # geom_text(
  #   aes(y = low-4000, 
  #       family = "Arial Rounded MT Bold",
  #       label = lab2)
  # )+
  expand_limits(x= c(0.1, length(levels(as.factor(lowe_high_data$year))) + 1.5))



plotly_plot <- ggplotly(p) %>% 
  plotly::layout(
    xaxis = list(title = "", 
                 fixedrange = T, 
                 automargin = T
    ),
    margin = list(l = 10, r = 10, b = 40, t = 10),
    yaxis = list(title = "", fixedrange = T, automargin = T)
  ) %>% 
  add_annotations(
    x=6,
    y=134432,
    xref = "x",
    yref = "y",
    text = "Highest Lane\nHighest Step",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#DC9542', size = 13)
  ) %>%
  add_annotations(
    x=6,
    y=118596,
    xref = "x",
    yref = "y",
    text = "Masters\nHighest Step1",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#27396A', size = 13)
  ) %>% 
  add_annotations(
    x=6,
    y=110490,
    xref = "x",
    yref = "y",
    text = "Bachelors\nHighest Step",
    xanchor = 'right',
    showarrow = F,
    font = list(color = '#4E0F53', size = 13)
  ) 


plotly_plot$x$data[[1]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Max Pay Bachelors:", scales::dollar(lowe_high_data$top_b), "<br>",
  'Increase:', scales::percent(round(lowe_high_data$increase_bmax, 3))
)

plotly_plot$x$data[[2]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Max Pay Masters:", scales::dollar(lowe_high_data$top_m), "<br>",
  'Increase:', scales::percent(round(lowe_high_data$increase_mmax, 3))
)

plotly_plot$x$data[[3]]$text <- paste(
  # "Step:",salary_data$type, "<br>",
  "Max Pay Highest Lane:", scales::dollar(lowe_high_data$top_h), "<br>",
  'Increase:', scales::percent(round(lowe_high_data$increase_hmax, 3))
)

plotly_plot$x$data[[4]]$hovertext <- paste(
  # "Step:",salary_data$type, "<br>",
  lowe_high_data$year)

htmlwidgets::saveWidget(config(plotly_plot, showLink = F, displayModeBar = FALSE),
                        "~/Documents/Github/demo/assets/plots/max.html")

