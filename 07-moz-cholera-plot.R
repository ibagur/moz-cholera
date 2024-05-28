# MAPS -------------------------------------------------------------------

## Cholera outbreak district map -----

cholera_adm2_map <- adm2_map %>% 
  left_join(cholera_data_adm2, by=c("adm2_pcode"="ADM2_PCODE")) 

# # Create pattern sfc layer
# cholera_adm2_sfc <- cholera_adm2_map %>% 
#   filter(!is.na(zq) | !is.na(zar)) %>% 
#   hatchedLayer("right2left", density = 7, mode= "sfc")
# 
# # Create sf object for map plot
# cholera_adm2_patterns <- st_sf(geometry = cholera_adm2_sfc)

tmap_mode("plot")

map <- tm_shape(cholera_adm2_map) + 
  tm_fill(
    col = "fill_color",
    textNA = "No data",
  ) +
  # tm_shape(cholera_adm2_patterns) +
  # tm_lines(col = "grey40", lwd = 0.5) +
  # tm_text(
  #   text = "adm1_pt",
  #   size = 0.9,
  #   col = "#505050",
  #   shadow = F,
  #   remove.overlap = F) +
  tm_shape(africa_adm0_map) +
  tm_polygons(col = "grey90", lwd = 0.5) +
  tm_shape(mwi_adm2_map) +
  tm_borders(lwd = 0.3) +
  tm_text(
    text = "ADM2_EN",
    size = 0.4,
    col = "grey40"
  ) +
  tm_shape(zwe_adm2_map) +
  tm_borders(lwd = 0.3) +
  tm_text(
    text = "ADM2_EN",
    size = 0.4,
    col = "grey40"
  ) +
  tm_shape(zmb_adm2_map) +
  tm_borders(lwd = 0.3) +
  tm_text(
    text = "ADM2_EN",
    size = 0.4,
    col = "grey40"
  ) +
  tm_shape(adm1_map) +
  tm_borders(lwd = 1.3) +
  tm_shape(cholera_adm2_map) + 
  tm_borders(lwd = 0.3) +
  tm_bubbles(
    #size = "total_partners_lbl",
    size = "total_ngo_lbl",
    col = UNICEF_PALETTE[1],
    border.col = "white",
    border.lwd = 0.3,
    scale = 1.5,
    perceptual = TRUE,
    #palette = palette_partners,
    #n = 4,
    title.size = "# Partners",
    legend.col.show = F,
    legend.size.show = F,
    xmod = -0.3*cholera_adm2_map$mod_factor
  ) +
  tm_text(
    text = "total_ngo_lbl",
    col = "white",
    #size = "total_unicef_lbl",
    size = 0.6,
    #shadow = T,
    xmod = -0.3*cholera_adm2_map$mod_factor,
    remove.overlap = F,
    legend.col.show = F,
    legend.size.show = F) +
  tm_shape(cholera_adm2_map) + 
  tm_bubbles(
    #size = "total_partners_lbl",
    size = "total_gov_lbl",
    col = UNICEF_PALETTE[2],
    border.col = "white",
    border.lwd = 0.3,
    scale = 1.5,
    perceptual = TRUE,
    #palette = palette_partners,
    #n = 4,
    title.size = "# Partners",
    legend.col.show = F,
    legend.size.show = F,
    xmod = 0.3*cholera_adm2_map$mod_factor,
    size.max = max(cholera_adm2_map$total_ngo_lbl, na.rm = TRUE)
  ) +
  tm_text(
    text = "total_gov_lbl",
    col = "white",
    size = 0.6,
    #size = "total_non_unicef_lbl",
    #shadow = T,
    xmod = 0.3*cholera_adm2_map$mod_factor,
    remove.overlap = F,
    legend.col.show = F,
    legend.size.show = F) +
  tm_shape(cholera_adm2_map) +  
  tm_text(
    text = "adm2_pt_filtered",
    size = 0.5,
    ymod = 0.6,
    remove.overlap = F) +
  tm_shape(adm1_map) +
  tm_text(
    text = "adm1_pt",
    col = "grey30",
    size = 1,
    ymod = case_when(
      adm1_map$adm1_pt == "Cabo Delgado" ~ 0.5 + 0.6,
      adm1_map$adm1_pt == "Manica" ~ 2 + 0.6,
      adm1_map$adm1_pt == "Sofala" ~ 0.3 + 0.6,
      .default = 0.6
    ),
    shadow = F,
    remove.overlap = T) +
  tm_shape(africa_adm0_map) +
  tm_text(
    text = "adm0_name_wrap",
    col = "grey40",
    size = 0.7,
    xmod = case_when(
      africa_adm0_map$ISO3 == "ZWE" ~ 4,
      africa_adm0_map$ISO3 == "ZMB" ~ 11,
      africa_adm0_map$ISO3 == "MWI" ~ -1,
      africa_adm0_map$ISO3 == "ZAF" ~ 18,
      africa_adm0_map$ISO3 == "TZA" ~ 8,
      africa_adm0_map$ISO3 == "SWZ" ~ 0,
      .default = 0
    ),
    ymod = case_when(
      africa_adm0_map$ISO3 == "ZAF" ~ 13,
      africa_adm0_map$ISO3 == "TZA" ~ -15,
      africa_adm0_map$ISO3 == "ZMB" ~ 3,
      .default = 0
    )) +
  tm_scale_bar(lwd=0.5) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_add_legend("symbol", 
                # labels = c("Number of UNICEF partners in current and recently affected districts", "Districts with usual cholera hotspots (prior to 2022)", "Districts recently affected by cholera, but currently inactive", paste0("Active cholera districts (", max_date_1,")")),  # Replace with actual labels
                # col = c(UNICEF_PALETTE[1], "white", UNICEF_PALETTE_YELLOWS[3], UNICEF_PALETTE[6]),
                # shape = c(21,22,22,22),
                labels = c("# of NGO partners in active and recently affected districts",
                           "# of Government partners in active and recently affected districts", 
                           "Districts recently affected by cholera, but currently inactive", 
                           paste0("Active cholera districts (", max_date_1,")")),  # Replace with actual labels
                col = c(UNICEF_PALETTE[1], UNICEF_PALETTE[2], UNICEF_PALETTE_YELLOWS[3], UNICEF_PALETTE[6]),
                shape = c(21,21,22,22),
                size = 1,
                border.lwd = 0.5,
                border.col = "white",
                reverse = T
  ) + 
  tm_layout(
    frame = FALSE,
    legend.text.size=0.7,
    legend.width = 1,
    legend.position = c(0.45, 0.05), 
    fontface = "plain",
    fontfamily = font_name, 
    attr.color = UNICEF_PALETTE[11])

#print(map)

tmap_save(map, filename = paste(plot_dir, "cholera_partners_map.pdf", sep = "/"), width  = 8.27, height= 11.69, dpi=600)
tmap_save(map, filename = paste(plot_dir, "cholera_partners_map.png", sep = "/"), width  = 8.27, height= 11.69, dpi=150)

# Leaflet
# map_leaflet <- tmap_leaflet(map)
# saveWidget(map_leaflet, selfcontained = TRUE, file = paste(plot_dir, "map_mpc.html", sep = "/"))


## Cholera outbreak district map (no partners) -----

cholera_adm2_map <- adm2_map %>% 
  left_join(cholera_data_adm2, by=c("adm2_pcode"="ADM2_PCODE")) 

# # Create pattern sfc layer
# cholera_adm2_sfc <- cholera_adm2_map %>% 
#   filter(!is.na(zq) | !is.na(zar)) %>% 
#   hatchedLayer("right2left", density = 7, mode= "sfc")
# 
# # Create sf object for map plot
# cholera_adm2_patterns <- st_sf(geometry = cholera_adm2_sfc)

tmap_mode("plot")

map <- tm_shape(cholera_adm2_map) + 
  tm_fill(
    col = "fill_color",
    textNA = "No data",
  ) +
  # tm_shape(cholera_adm2_patterns) +
  # tm_lines(col = "grey40", lwd = 0.5) +
  # tm_text(
  #   text = "adm1_pt",
  #   size = 0.9,
  #   col = "#505050",
  #   shadow = F,
  #   remove.overlap = F) +
  tm_shape(africa_adm0_map) +
  tm_polygons(col = "grey90", lwd = 0.5) +
  tm_shape(mwi_adm2_map) +
  tm_borders(lwd = 0.3) +
  tm_text(
    text = "ADM2_EN",
    size = 0.4,
    col = "grey40"
  ) +
  tm_shape(zwe_adm2_map) +
  tm_borders(lwd = 0.3) +
  tm_text(
    text = "ADM2_EN",
    size = 0.4,
    col = "grey40"
  ) +
  tm_shape(zmb_adm2_map) +
  tm_borders(lwd = 0.3) +
  tm_text(
    text = "ADM2_EN",
    size = 0.4,
    col = "grey40"
  ) +
  tm_shape(adm1_map) +
  tm_borders(lwd = 1.3) +
  tm_shape(cholera_adm2_map) + 
  tm_borders(lwd = 0.3) +
  tm_text(
    text = "adm2_pt_filtered",
    size = 0.5,
    ymod = 0.6,
    remove.overlap = F) +
  tm_shape(adm1_map) +
  tm_text(
    text = "adm1_pt",
    col = "grey30",
    size = 1,
    ymod = case_when(
      adm1_map$adm1_pt == "Cabo Delgado" ~ 0.5 + 0.6,
      adm1_map$adm1_pt == "Manica" ~ 2 + 0.6,
      adm1_map$adm1_pt == "Sofala" ~ 0.3 + 0.6,
      .default = 0.6
    ),
    shadow = F,
    remove.overlap = T) +
  tm_shape(africa_adm0_map) +
  tm_text(
    text = "adm0_name_wrap",
    col = "grey40",
    size = 0.7,
    xmod = case_when(
      africa_adm0_map$ISO3 == "ZWE" ~ 4,
      africa_adm0_map$ISO3 == "ZMB" ~ 11,
      africa_adm0_map$ISO3 == "MWI" ~ -1,
      africa_adm0_map$ISO3 == "ZAF" ~ 18,
      africa_adm0_map$ISO3 == "TZA" ~ 8,
      africa_adm0_map$ISO3 == "SWZ" ~ 0,
      .default = 0
    ),
    ymod = case_when(
      africa_adm0_map$ISO3 == "ZAF" ~ 13,
      africa_adm0_map$ISO3 == "TZA" ~ -15,
      africa_adm0_map$ISO3 == "ZMB" ~ 3,
      .default = 0
    )) +
  tm_scale_bar(lwd=0.5) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_add_legend("symbol", 
                labels = c(
                  "Districts recently affected by cholera, but currently inactive", 
                  paste0("Active cholera districts (", max_date_1,")")),  # Replace with actual labels
                col = c(UNICEF_PALETTE_YELLOWS[3], UNICEF_PALETTE[6]),
                shape = c(22,22),
                size = 1,
                border.lwd = 0.5,
                border.col = "white",
                reverse = T
  ) + 
  tm_layout(
    frame = FALSE,
    legend.text.size=0.7,
    legend.width = 1,
    legend.position = c(0.45, 0.05), 
    fontface = "plain",
    fontfamily = font_name, 
    attr.color = UNICEF_PALETTE[11])

tmap_save(map, filename = paste(plot_dir, "cholera_map.pdf", sep = "/"), width  = 8.27, height= 11.69, dpi=600)
tmap_save(map, filename = paste(plot_dir, "cholera_map.png", sep = "/"), width  = 8.27, height= 11.69, dpi=150)

# PLOTS -------------------------------------------------------------------

## Loess-smoothed trendlines Province daily -----

data <- province_daily_cumul_tbl %>% 
  filter(!is.na(ADM1_PCODE))

# Find a point to place the label
label_point <- national_daily_cumul_tbl %>%
  arrange(desc(date)) %>%
  slice(4)
label_point_mean <- ceiling(mean(national_daily_cumul_tbl$total_cases)/5)*5

# smooth factor
loess_span = 0.3

province_daily_trend_plot <- ggplot() +
  geom_smooth(data = data, aes(x = date, y = cases, group = ADM1_PT, color = ADM1_PT), se = F, method = "loess", span=loess_span, size = 0.7) +
  geom_line(data = data, aes(x = date, y = cases, group = ADM1_PT, color = ADM1_PT), stat="smooth", method = "lm", linetype = "dashed", size = 0.5, alpha = 0.5) +
  geom_smooth(data = national_daily_cumul_tbl, aes(x = date, y = total_cases), se = F, method = "loess",span=loess_span, size = 0.7) + # can also use geom_line + loess
  geom_line(data = national_daily_cumul_tbl, aes(x = date, y = total_cases), stat="smooth", method = "lm", linetype = "dashed", size = 0.5, alpha = 0.5) +
  #geom_label(data = label_point, aes(x = date, y = label_point_mean, label = "National "), vjust = -2, hjust = 10.5, family = font_name) +
  geom_label(data = label_point, aes(x = date, y = 75, label = "National "), vjust = -2, hjust = 10.5, family = font_name) +
  scale_color_manual(values = UNICEF_PALETTE) +
  scale_x_date(date_labels = "%d/%m/%Y") +
  scale_y_continuous(labels = label_comma(), limits = c(0, ceiling(0.7*max(national_daily_cumul_tbl$total_cases)/5)*5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 10),
        text = element_text(family = font_name),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        #panel.grid.major.x = element_blank(),
        panel.grid.major.x = element_line(linewidth = .1, color = "grey80"), 
        panel.grid.major.y = element_line(linewidth = .1, color = "grey80"), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(colour = UNICEF_PALETTE[1], size = 24, family = font_name),
        plot.subtitle = element_text(colour = UNICEF_PALETTE[10], size = 16, family = font_name)) +
  labs(color = "Province", title = paste0("Daily incidence in number of cases per affected province. Date: ", max_date_1), subtitle = "Loess-smoothed and linear model trends")

#output$province_daily_cumul_plot <- province_daily_cumul_plot
#print(province_daily_trend_plot)

name_plot <- substitute(province_daily_trend_plot)
p <- province_daily_trend_plot
plot_file <-  paste(plot_dir, paste0(name_plot, "_", max_date_2, ".pdf"), sep = "/")
plot_file_nodate <-  paste(plot_dir, paste0(name_plot, ".pdf"), sep = "/")

# export plot to pdf
#ggsave(plot = p, filename = plot_file, device = cairo_pdf, width = 12, height = 8)
ggsave(plot = p, filename = plot_file_nodate, device = cairo_pdf, width = 12, height = 8)
# export plot to png
plot_file_nodate_png <-  paste(plot_dir, paste0(name_plot, ".png"), sep = "/")
ggsave(plot = p, filename = plot_file_nodate_png, width = 12, height = 8, dpi = 72)

## Stacked barchart Province total weekly -----

data <- province_weekly_tbl %>% 
  filter(n_days == 7) %>%
  filter(!is.na(ADM1_PCODE)) %>% 
  filter(total_cases !=0) %>% 
  filter(grepl("2024", week))

max_week <- max(data$week)

province_weekly_bar_plot <- ggplot(data, aes(x = week, y = total_cases, fill = ADM1_PT)) +
  geom_bar(stat = "identity", color = "white", size = 0.6) +
  geom_text(aes(label=total_cases),
            position=position_stack(vjust=0.5), colour="white", size = 4, family = font_name, fontface = "bold") +
  theme_minimal() +
  scale_fill_manual(values = UNICEF_PALETTE) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 9),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.text.y = element_text(size = 10),
        axis.text.y = element_blank(),
        text = element_text(family = font_name),
        #legend.position = "none",
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        #panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        #panel.grid.major.y = element_line(linewidth=.1, color="grey80" ), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(colour = UNICEF_PALETTE[1], size = 24, family = font_name)
  ) +
  labs(fill = "Province", title = paste0("Weekly incidence in number of cases per affected province"))  # Set custom legend title

#print(province_weekly_bar_plot)

name_plot <- substitute(province_weekly_bar_plot)
p <- province_weekly_bar_plot
plot_file <-  paste(plot_dir, paste0(name_plot, "_", max_week, ".pdf"), sep = "/")
plot_file_nodate <-  paste(plot_dir, paste0(name_plot, ".pdf"), sep = "/")

# export plot to pdf
#ggsave(plot = p, filename = plot_file, device = cairo_pdf, width = 14, height = 8) 
ggsave(plot = p, filename = plot_file_nodate, device = cairo_pdf, width = 14, height = 8)
# export plot to png
plot_file_nodate_png <-  paste(plot_dir, paste0(name_plot, ".png"), sep = "/")
ggsave(plot = p, filename = plot_file_nodate_png, width = 14, height = 8, dpi = 72)

## Scatter-plot Bulletin active data -----

data <- bulletin_active_data_adm2_tbl

cholera_active_occupancy_district_plot <- ggplot(data, aes(x = overall_incidence_rate, y = occupancy_rate, size = capacity, label = ADM2_PT, fill = ADM1_PT)) +
  geom_point(alpha=0.85, shape=21, color="black") +
  geom_text_repel(size = 5, box.padding = 0.35, point.padding = 0.5, max.overlaps = 20, family = font_name) +
  scale_fill_manual(values = UNICEF_PALETTE, name = "Province") +
  theme_ipsum() +
  scale_size(range = c(2, 15)) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  scale_x_continuous() +
  scale_y_continuous(labels = label_percent()) +
  geom_hline(yintercept = 0.75, color = UNICEF_PALETTE[6], linetype = 2, size = 0.5) +
  annotate(geom="label", x=-10, y=0.75, label="75%", fill="white", color=UNICEF_PALETTE[6], family = font_name) +
  #annotate("text", x = Inf, y = 0.75, label = "75%", hjust = 1.1, color = UNICEF_PALETTE[6]) + 
  theme(
    legend.position = "bottom",
    legend.box.spacing = grid::unit(2, "cm"),
    axis.title.x = element_text(size = 12, family = font_name),
    axis.title.y = element_text(size = 12, family = font_name),
    text = element_text(family = font_name),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(colour = UNICEF_PALETTE[1], size = 24, family = font_name), 
    plot.subtitle = element_text(colour = UNICEF_PALETTE[10], size = 16, family = font_name)
  ) +
  labs(title = "Bed occupancy rate by affected district",
       subtitle = paste0("Display only districts with current hospitalized cases (", max_date_1, ")"),
       x = "Overall incidence rate (cumulative cases per 100,000 inhabitans)",
       y = "Health centers bed occupancy rate",
       size = "Bed capacity (absolute number)")

#print(cholera_active_occupancy_district_plot)

name_plot <- substitute(cholera_active_occupancy_district_plot)
p <- cholera_active_occupancy_district_plot

plot_file <-  paste(plot_dir, paste0(name_plot, "_", max_date_2, ".pdf"), sep = "/")
plot_file_nodate <-  paste(plot_dir, paste0(name_plot, ".pdf"), sep = "/")

# export plot to pdf
#ggsave(plot = p, filename = plot_file, device = cairo_pdf, width = 12, height = 10)
ggsave(plot = p, filename = plot_file_nodate, device = cairo_pdf, width = 12, height = 10)
# export plot to png
plot_file_nodate_png <-  paste(plot_dir, paste0(name_plot, ".png"), sep = "/")
ggsave(plot = p, filename = plot_file_nodate_png, width = 12, height = 10, dpi = 72)

## Reactable Bulletin active data -----

options(reactable.theme = reactableTheme(
  style = list(
    fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
))

data <- bulletin_active_data_adm2_tbl %>% 
  select(Province = ADM1_PT, District = ADM2_PT, occupancy_rate, overall_incidence_rate, cases_24h = cases_now, hospitalized, bed_capacity = capacity) 

cholera_active_occupancy_district_react <- data %>% 
  arrange(-occupancy_rate) %>% 
  # mutate(total_reached_flag = fifelse(total_reached < 100, 1, 0)) %>% 
  mutate(occupancy_flag = fifelse(occupancy_rate > 0.75, 1, 0)) %>% 
  mutate(occupancy_color = fifelse(occupancy_rate > 0.75, UNICEF_PALETTE[4], "white")) %>% 
  reactable(
    defaultPageSize = nrow(.),
    defaultColDef = colDef(minWidth = 50),
    style = list(fontFamily = font_name, fontSize = "1rem"),
    columns = list(
      # Province = colDef(align = "center", cell = reactablefmtr::color_tiles(. ,colors = c('white', UNICEF_PALETTE[4]), color_by = 'occupancy_flag')),
      Province = colDef(align = "center", cell = reactablefmtr::color_tiles(. , color_ref = 'occupancy_color')),
      # District = colDef(align = "center", cell = reactablefmtr::color_tiles(. ,colors = c('white', UNICEF_PALETTE[6]), color_by = 'occupancy_flag')),
      District = colDef(align = "center", cell = reactablefmtr::color_tiles(. , color_ref = 'occupancy_color')),
      # occupancy_rate = colDef(name = "Bed occupancy rate", align = "center", cell = color_tiles(. ,colors = c('white', UNICEF_PALETTE[6]), color_by = 'occupancy_flag', number_fmt = label_percent())),
      occupancy_rate = colDef(name = "Bed occupancy rate", align = "center", cell = reactablefmtr::color_tiles(. ,colors = c('white', UNICEF_PALETTE[6]), number_fmt = label_percent())),
      overall_incidence_rate = colDef(name = "Overall incidence rate (per 100,000)", align = "center", cell = reactablefmtr::color_tiles(.,  number_fmt = scales::comma, colors = c('white', UNICEF_PALETTE[5]))),
      cases_24h = colDef(name = "Cases (last 24h)", align = "center"),
      hospitalized = colDef(name = "Currently hospitalized", align = "center"),
      # bed_capacity = colDef(name = "Beds capacity", align = "center", cell = color_tiles(., colors = c('white', UNICEF_PALETTE[2]))),
      bed_capacity = colDef(name = "Beds capacity", align = "center"),
      occupancy_flag = colDef(show = F),
      occupancy_color = colDef(show = F)
    )
  )

#print(cholera_active_occupancy_district_react)

name_plot <- substitute(cholera_active_occupancy_district_react)
p <- cholera_active_occupancy_district_react

html_file <-  paste(plot_dir, paste0(name_plot, ".html"), sep = "/")
plot_file <-  paste(plot_dir, paste0(name_plot, "_", max_date_2, ".png"), sep = "/")
plot_file_nodate <-  paste(plot_dir, paste0(name_plot, ".png"), sep = "/")

# Save as html and take screenshot in PNG
saveWidget(widget = p, file = html_file, selfcontained = TRUE)
#webshot(url = html_file, file = plot_file, delay = 0.1, vwidth = 865, vheight = 700)
webshot(url = html_file, file = plot_file_nodate, delay = 0.1, vwidth = 865, vheight = 700)


## Stacked barchart Bi-weekly ------

data <- district_weekly_tbl %>% 
  filter(week <= max_week) %>% 
  arrange(desc(week)) %>%
  mutate(rank = dense_rank(desc(week))) %>%
  filter(rank <= 2) %>%
  select(-rank) %>% 
  filter(!is.na(ADM1_PT)) %>% 
  group_by(ADM2_PT) %>% 
  filter(sum(total_cases) > 0) %>%
  ungroup() %>% 
  mutate(total_cases = if_else(total_cases == 0, 0.01, total_cases)) %>% 
  arrange(ADM1_PT, ADM2_PT) %>%
  mutate(ADM1_PT_numeric = as.integer(factor(ADM1_PT))) %>% 
  mutate(week_numeric = as.integer(factor(week))) %>% 
  mutate(color_map = UNICEF_PALETTE[ADM1_PT_numeric]) %>% 
  mutate(color_map = if_else(week_numeric == 1, lighten(color_map, 0.4), color_map)) %>% # lighten the color for previous week
  mutate(ADM2_PT = factor(ADM2_PT, levels = unique(ADM2_PT))) %>%
  mutate(color_map = factor(color_map, levels = rev(unique(color_map)))) # reverse week order in the bars by ordering color

#  Make interleaved labels with empty strings
interleaved_labels <- c(rbind(rep("", length(rev(unique(data$ADM1_PT)))), rev(unique(data$ADM1_PT))))

district_biweekly_plot <- ggplot(data, aes(x = ADM2_PT, y = total_cases, fill = color_map)) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_identity(guide = guide_legend(nrow = 2, reverse = TRUE, override.aes = list(fill = unique(data$color_map))), labels = rep(rev(unique(factor(data$ADM1_PT))), each =2) ) +
  scale_fill_identity(guide = guide_legend(nrow = 2, reverse = TRUE, override.aes = list(fill = unique(data$color_map))), labels = interleaved_labels ) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y = element_text(size = 12),
        text = element_text(family = font_name),
        legend.position = "bottom",
        legend.key.spacing.x = unit(1.5, 'cm'),
        legend.text = element_text(size = 14, vjust=-0.5),
        legend.title = element_text(size = 16, vjust=0.9, lineheight = 1.4),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth=.1, color="grey80" ),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(colour = UNICEF_PALETTE[1], size = 24, family = font_name), 
        plot.subtitle = element_text(colour = UNICEF_PALETTE[10], size = 16, family = font_name, margin=margin(0,0,60,0)), 
        legend.box.spacing = grid::unit(2, "cm")) + labs(fill = paste0(unique(data$week)[1],"\n", unique(data$week)[2]), title = paste0("Change in weekly number of cases per affected district: ", unique(data$week)[1], " vs ", unique(data$week)[2]), subtitle = "Districts with no cases in the last two weeks are not shown")

name_plot <- substitute(district_biweekly_plot)
p <- district_biweekly_plot
#print(p)

plot_file <-  paste(plot_dir, paste0(name_plot, "_", max_week, ".pdf"), sep = "/")
plot_file_nodate <-  paste(plot_dir, paste0(name_plot, ".pdf"), sep = "/")

# export plot to pdf
#ggsave(plot = p, filename = plot_file, device = cairo_pdf, width = 18, height = 10)
ggsave(plot = p, filename = plot_file_nodate, device = cairo_pdf, width = 18, height = 10)
# export plot to png
plot_file_nodate_png <-  paste(plot_dir, paste0(name_plot, ".png"), sep = "/")
ggsave(plot = p, filename = plot_file_nodate_png, width = 18, height = 10, dpi = 72)