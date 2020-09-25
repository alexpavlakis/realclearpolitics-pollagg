library(tidyverse)
library(realclearpolitics)
library(pollagg)

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# Links to RCP sites
natl <- list("National" = "https://www.realclearpolitics.com/epolls/2020/president/us/general_election_trump_vs_biden-6247.html")
battleground_states <- list(
  "Arizona" = "https://www.realclearpolitics.com/epolls/2020/president/az/arizona_trump_vs_biden-6807.html",
  "New Hampshire" = "https://www.realclearpolitics.com/epolls/2020/president/nh/new_hampshire_trump_vs_biden-6779.html",
  "Michigan" = "https://www.realclearpolitics.com/epolls/2020/president/mi/michigan_trump_vs_biden-6761.html",
  "Wisconsin" = "https://www.realclearpolitics.com/epolls/2020/president/wi/wisconsin_trump_vs_biden-6849.html",
  "North Carolina" = "https://www.realclearpolitics.com/epolls/2020/president/nc/north_carolina_trump_vs_biden-6744.html",
  "Florida" = "https://www.realclearpolitics.com/epolls/2020/president/fl/florida_trump_vs_biden-6841.html",
  "Pennsylvania" = "https://www.realclearpolitics.com/epolls/2020/president/pa/pennsylvania_trump_vs_biden-6861.html",
  "Minnesota" = "https://www.realclearpolitics.com/epolls/2020/president/mn/minnesota_trump_vs_biden-6966.html",
  "Ohio" = "https://www.realclearpolitics.com/epolls/2020/president/oh/ohio_trump_vs_biden-6765.html",
  "Georgia" = "https://www.realclearpolitics.com/epolls/2020/president/ga/georgia_trump_vs_biden-6974.html",
  "Maine" = "https://www.realclearpolitics.com/epolls/2020/president/me/maine_trump_vs_biden-6922.html",
  "Texas" = "https://www.realclearpolitics.com/epolls/2020/president/tx/texas_trump_vs_biden-6818.html",
  "Iowa" = "https://www.realclearpolitics.com/epolls/2020/president/ia/iowa_trump_vs_biden-6787.html",
  "Nevada" = "https://www.realclearpolitics.com/epolls/2020/president/nv/nevada_trump_vs_biden-6867.html",
  "Virginia" = "https://www.realclearpolitics.com/epolls/2020/president/va/virginia_trump_vs_biden-6988.html"
)

# Empty lists for storing plots and results
plots <- list()
results <- list()

# Loop over states, pulling data and fitting model for each.
for(state in sort(unique(names(battleground_states)))) {
  
  # Scrape polls
  raw_polls <- get_rcp_polls(battleground_states[[state]])
  
  # Format (RCP doesn't include year, so we need to be careful that we are only taking polls from the cal year)
  raw_polls <- raw_polls %>% 
    filter(Sample != '--') %>%
    mutate(month = as.numeric(sapply(strsplit(Date, '/'), head, 1)),
           month_prog = month - lead(month))
  if(any(raw_polls$month_prog < 0)) {
    raw_polls <- raw_polls[c(1:which(raw_polls$month_prog == min(raw_polls$month_prog, na.rm = T))), ]
  }
  
  # Now we can 'tidy' the remaining polls
  rcp_polls <- raw_polls %>%
    select(-c(month, month_prog)) %>%
    tidy() %>%
    mutate(poll_end = as.Date(paste0(poll_end, "/2020"), '%m/%d/%Y')) %>%
    filter(poll_end < Sys.Date(), sample_size > 0) %>%
    select(-poll_start)
  
  # Conver to modeling data (we do a likely voter adjustment of 1 pct for Trump in accordance with empirical estimates)
  modeling_data <- rcp_polls %>% 
    mutate(pct = case_when(
      str_detect(answer, 'Biden') & sample_type == 'RV' ~ pct - .005,
      str_detect(answer, 'Trump') & sample_type == 'RV' ~ pct + .005,
      TRUE ~ pct
    )) %>%
    mutate(y = floor(pct*sample_size)) %>%
    select(pollster, sample_type, poll_end, n = sample_size, answer, y) %>% 
    spread(answer, y) %>%
    arrange(poll_end) %>%
    select(-pollster) %>%
    na.omit()
  
  # Fit the model
  y <- modeling_data[, c(4:5)]
  n <-  modeling_data$n
  dates <-  modeling_data$poll_end
  all_dates <- unique(c(dates, Sys.Date()))
  fit <- yapa(y = y, n = n, dates = dates, all_dates = all_dates, iter = 1000)
  
  # Extract results 
  res <- fit$trend %>%
    filter(date == max(date)) %>%
    mutate(pct = paste0(format(round(mean*100, 1), nsmall = 1), "%"))
  results[[state]] <- res
  
  # Determine who's ahead and by how much for the most recent day
  ahead <- res$answer[res$mean == max(res$mean)]
  margin <- paste0("+", format((round(res$mean[res$answer == ahead], 3) - round(res$mean[res$answer != ahead], 3))*100, 
                               nsmall = 1), "%")
  
  # Build plot
  plots[[state]] <- plot(fit, model_alpha = 0, poll_alpha = 0.5) + 
    geom_hline(yintercept = 0.5, lty = 2, col = 'grey') +
    geom_text(data = res, aes(x = date, y = mean, label = pct, col = answer),
              hjust = -.1, show.legend = FALSE, size = 3.5)  +
    scale_x_date(date_breaks = '1 month', date_labels = "%b %Y", 
                 limits = c(min(fit$all_dates), as.Date("2020-11-10"))) +
    scale_y_continuous( breaks = seq(0, 1, 0.05), 
                       labels = paste0(seq(0, 100, 5), '%')) +
    scale_fill_manual(values = c('blue', 'red')) +
    scale_color_manual(values = c('blue', 'red')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          plot.subtitle = element_text(size = 9),
          legend.position = 'bottom',
          panel.grid.minor = element_blank(),
          plot.title = element_text(color = ifelse(ahead == 'Biden (D)', 'blue', 'red'),
                                    size = 14)) +
    labs(x = NULL, y = NULL, col = NULL, fill = NULL,
         title = paste0(state, ": ", ahead, " ", margin)) +
    guides(col = F)
}

save(plots, file = 'results/plots.rda')
save(results, file = 'results/res.rda')


rmarkdown::render_site('docs')
system("open docs/_site/index.html")

