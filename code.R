library('tidyverse')
library('tidycensus')
library('waffle')
library('stringr')

# https://factfinder.census.gov/bkmk/table/1.0/en/ACS/17_5YR/B03002/0400000US24.05000

md <- get_acs("county", 
              table = "B03002", 
              summary_var = "B03002_001",
              state = "24", 
              year = 2017)

md.state <- get_acs("state", 
              table = "B03002", 
              summary_var = "B03002_001",
              state = "24", 
              year = 2017)

combined <- rbind(md, md.state)

combined <- combined %>% select(-summary_moe) %>%
  mutate(cat = case_when(
    variable == 'B03002_003' ~ "white",
    variable == 'B03002_004' ~ "black",
    variable == 'B03002_005' | variable == 'B03002_007' | variable == 'B03002_008' | variable == 'B03002_009' ~ "other",
    variable == 'B03002_006' ~ "asian",
    variable == 'B03002_012' ~ "hisp")) %>% 
  filter(!is.na(cat)) %>% group_by(GEOID, NAME, cat, 
                                   summary_est) %>% summarise(estimate = sum(estimate)) %>% 
  mutate(perc.raw = estimate/summary_est * 100,
         perc.round = round(estimate/summary_est * 100),
         chart.perc = 100-perc.round) # where chart.perc is everybody else

combined$cat.f <- factor(combined$cat, 
                         levels = c('other',
                                    'hisp',
                                    'asian',
                                    'white',
                                    'black'))

## Bars

combined <- combined %>% 
  separate(NAME, c("county", "state"), ", ")

combined <- combined %>% mutate(county.f = case_when(
  county == 'Baltimore County' ~ 'Baltimore Co.',
  county == 'Baltimore city' ~ 'Baltimore City',
  TRUE ~ gsub(' County', '', county)))

md.black <- combined %>% filter(cat == 'black')

combined$county.f.format <- factor(combined$county.f, 
                      levels = md.black$county.f[order(md.black$perc.raw)])


ggplot(combined, aes(x = county.f.format,
               y = perc.raw,
               fill = cat.f)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_manual(values = c('#B32454',
                               '#D8AA27',
                               '#663267',
                               '#77973E',
                               '#087D79')) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12),
        panel.background = element_blank(),
        legend.title = element_blank()) + labs(x  = '', y = '')

ggsave('bar.eps', device = 'eps', height = 8, width = 6)


## pies

ggplot(combined, aes(x = factor(1), 
                     y = perc.raw, 
                     fill = cat.f)) +
  geom_bar(stat = "identity", 
           color = 'white', size = .1) +
  facet_wrap(~county.f) +
  coord_polar(theta = "y", start = 1) +
  scale_fill_manual(values = c('#B32454',
                               '#D8AA27',
                               '#663267',
                               '#77973E',
                               '#087D79')) + 
  theme(panel.grid.minor.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(face = 'bold'),
        strip.background = element_blank()) + 
  labs(x  = '', y = '')

ggsave('pie.eps', device = 'eps', width = 8, height = 6)


## Waffles
# https://github.com/hrbrmstr/waffle

make_chart_asian <- function(cty) {
  waffle(combined %>% filter(GEOID == cty & 
                         cat == 'asian') %>% ungroup() %>% select(perc.round, 
                                                                  chart.perc),
         rows = 10, size = 0.2, flip = T, legend_pos = 'none',
         colors = c("#663267", '#E7E7E7'))
}

make_chart_black <- function(cty) {
  waffle(combined %>% filter(GEOID == cty & 
                         cat == 'black') %>% ungroup() %>% select(perc.round, 
                                                                  chart.perc),
         rows = 10, size = 0.3, flip = T, legend_pos = 'none',
         colors = c("#087D79", '#E7E7E7'))
}

make_chart_hisp <- function(cty) {
  waffle(combined %>% filter(GEOID == cty & 
                         cat == 'hisp') %>% ungroup() %>% select(perc.round, 
                                                                  chart.perc),
         rows = 10, size = 0.3, flip = T, legend_pos = 'none',
         colors = c("#D8AA27", '#E7E7E7'))
}

make_chart_other <- function(cty) {
  waffle(combined %>% filter(GEOID == cty & 
                         cat == 'other') %>% ungroup() %>% select(perc.round, 
                                                                 chart.perc),
         rows = 10, size = 0.3, flip = T, legend_pos = 'none',
         colors = c("#B32454", '#E7E7E7'))
}

make_chart_white <- function(cty) {
  waffle(combined %>% filter(GEOID == cty & 
                         cat == 'white') %>% ungroup() %>% select(perc.round, 
                                                                 chart.perc),
         rows = 10, size = 0.3, flip = T, legend_pos = 'none',
         colors = c("#77973E", '#E7E7E7'))
}

for (i in unique(combined$GEOID)) { 
 make_chart_asian(i)
  ggsave(paste0( 
                tolower(unique(combined[combined$GEOID == i, ]$county)), 'asian.eps'), 
         device = 'eps')
 make_chart_black(i)
 ggsave(paste0( 
               tolower(unique(combined[combined$GEOID == i, ]$county)), 'black.eps'), 
        device = 'eps')
 make_chart_hisp(i)
 ggsave(paste0( 
               tolower(unique(combined[combined$GEOID == i, ]$county)), 'hisp.eps'), 
        device = 'eps')
 make_chart_other(i)
 ggsave(paste0( 
               tolower(unique(combined[combined$GEOID == i, ]$county)), 'other.eps'), 
        device = 'eps')
 make_chart_white(i)
 ggsave(paste0( 
               tolower(unique(combined[combined$GEOID == i, ]$county)), 'white.eps'), 
        device = 'eps')
} 


