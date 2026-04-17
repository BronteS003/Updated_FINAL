################################################################################
##                 MAPPING POLYGONS IN R - KK                                 ##
################################################################################
# Creating maps of subdistrict Khok Kruat and Tha Chang, highlighting polygons #
################################################################################
# Created March 10, 2026 by Bronte Slote, last edited March 10, 2026           #
################################################################################

##LOAD LIBRARIES##
library(sf)
library(osmdata)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggspatial)
library(ggnewscale)


################################################################################

##LOAD DATA##

#country level geometry
tha_adm1 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/tha_adm_rtsd_itos_20210121_shp/tha_admbnda_adm1_rtsd_20220121.shp")

#subdistrict geometry
tha_adm3 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/tha_adm_rtsd_itos_20210121_shp/tha_admbnda_adm3_rtsd_20220121.shp")

#streets geometry
kk_major <- getbb(place_name = "Khok Kruat, Nakhon Nayok, Thailand") %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

tc_major <- getbb(place_name = "Tha Chang, Nakhon Nayok, Thailand") %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

#polygon geometry
kk01 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/polygon_maps/Khok+Kruat+01_Khok+Kruat+01_enGB_20251125101654.kml")
kk06 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/polygon_maps/Khok+Kruat+06_Khok+Kruat+06_enGB_20251125101719.kml")
kk07 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/polygon_maps/Khok+Kruat+07_Khok+Kruat+07_enGB_20251125101745.kml")

tc12 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/polygon_maps/Tha+Chang+12_Tha+Chang+12_enGB_20251125101825.kml")
tc16 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/polygon_maps/Tha+Chang+16_Tha+Chang+16_enGB_20251125101837.kml")
tc20 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/polygon_maps/Tha+Chang+20_Tha+Chang+20_enGB_20251125101843.kml")
tc24 <- st_read("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data Visualizations/polygon_maps/Tha+Chang+24_Tha+Chang+24_enGB_20251125101857.kml")

################################################################################

##ORGANIZE DATA FOR MAPPING##

#subset subdistricst (admin_3) of Khok Kruat & Tha Chang
kk <- tha_adm3 %>% filter(ADM3_PCODE == "TH260204")
tc <- tha_adm3 %>% filter(ADM3_PCODE == "TH260102")

#subset province of Nakhon Nayok for mapping
nnayok <- tha_adm3 %>% filter(ADM1_EN == "Nakhon Nayok")

#create structure for keeping maps in same scale
bb_kk <- st_bbox(kk)
bb_tc <- st_bbox(tc)
#set map size
width  <- 0.09
height <- 0.06
#center each map
center_bbox <- function(bb, width, height){
  cx <- (bb["xmin"] + bb["xmax"]) / 2
  cy <- (bb["ymin"] + bb["ymax"]) / 2
  
  list(
    xlim = c(cx - width/2, cx + width/2),
    ylim = c(cy - height/2, cy + height/2)
  )
}

kk_lim <- center_bbox(bb_kk, width, height)
tc_lim <- center_bbox(bb_tc, width, height)


################################################################################

##MAP KK WITH POLYGONS##

kk_major$osm_lines <- st_crop(kk_major$osm_lines, xmin = 101.13, xmax = 101.34, ymin = 14.16, ymax = 14.22)

polygons_kk <- ggplot() +
  geom_sf(data = kk, fill = "#3182bd", color = "black", linewidth = 1) +
  geom_sf(data = kk01, aes(fill = "Polygon 1"), color = "black", size = 0.2) +
  geom_sf(data = kk06, aes(fill = "Polygon 6"), color = "black", size = 0.2) +
  geom_sf(data = kk07, aes(fill = "Polygon 7"), color = "black", size = 0.2) +
  geom_sf(data = kk_major$osm_lines, inherit.aes = FALSE, color = "black", size = 0.2) +
  labs(
    fill = "Polygon"
  ) +
  coord_sf(xlim = kk_lim$xlim,
           ylim = kk_lim$ylim,
           expand = FALSE)+
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

##MAP TC WITH POLYGONS##

tc_major$osm_lines <- st_crop(tc_major$osm_lines, xmin = 101.13, xmax = 101.34, ymin = 14.16, ymax = 14.22)

polygons_tc <- ggplot() +
  geom_sf(data = tc, fill = "#de2d26", color = "black", linewidth = 1) +
  geom_sf(data = tc12, aes(fill = "Polygon 12"), color = "black", size = 0.2) +
  geom_sf(data = tc16, aes(fill = "Polygon 16"), color = "black", size = 0.2) +
  geom_sf(data = tc20, aes(fill = "Polygon 20"), color = "black", size = 0.2) +
  geom_sf(data = tc24, aes(fill = "Polygon 24"), color = "black", size = 0.2) +
  geom_sf(data = tc_major$osm_lines, inherit.aes = FALSE, color = "black", size = 0.2) +
  labs(
    fill = "Polygon"
  ) +
  coord_sf(xlim = tc_lim$xlim,
           ylim = tc_lim$ylim,
           expand = FALSE)+
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)



################################################################################

##MAP Districts with Subdistricts and Thailand inset##

#Plot ggplot - province of Nahkon Nayok, divided by subdistrict
#Highlight subdistricts and districts of focus
nnayok <- nnayok %>%
  mutate(highlight = case_when(
    ADM3_EN == "Khok Kruat" ~ "Khok Kruat",
    ADM3_EN == "Tha Chang"  ~ "Tha Chang",
    ADM2_EN == "Mueang Nakhon Nayok" ~ "Mueang Nakhon Nayok",
    ADM2_EN == "Pak Phli" ~ "Pak Phli",
    ADM2_EN == "Ban Na" ~ "Ban Na",
    ADM2_EN == "Ongkharak" ~ "Ongkharak",
    TRUE ~ "Other"
  ))

districts <- ggplot() +
  
  #Districts
  geom_sf(
    data = subset(nnayok, highlight %in% c(
      "Ban Na", "Mueang Nakhon Nayok", "Ongkharak", "Pak Phli"
    )),
    aes(fill = highlight),
    color = "black",
    size = 0.2
  ) +
  scale_fill_manual(
    name = "District",
    values = c(
      "Mueang Nakhon Nayok" = "#fc9272",
      "Pak Phli" = "#9ecae1",
      "Ongkharak" = "#a1d99b",
      "Ban Na" = "#bdbdbd"
    )
  ) +
  
  ggnewscale::new_scale_fill() +
  
  #Subdistricts
  geom_sf(
    data = subset(nnayok, highlight %in% c(
      "Khok Kruat", "Tha Chang"
    )),
    aes(fill = highlight),
    color = "black",
    size = 0.2
  ) +
  scale_fill_manual(
    name = "Subdistricts of Focus",
    values = c(
      "Khok Kruat" = "#3182bd",
      "Tha Chang"  = "#de2d26"
    )
  ) +
  
  labs(
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.07, "in"),
    pad_y = unit(0.35, "in"),
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  ) 

################################################################################

## FULL THAILAND MAP ##

#highlight Nahkon Nayok province on Thailand map
tha_adm1 <- tha_adm1 %>%
  mutate(highlight = case_when(
    ADM1_EN == "Nakhon Nayok" ~ "Nakhon Nayok",
    TRUE ~ "Other"
  ))

thailand <- ggplot() +
  geom_sf(data = tha_adm1, aes(fill = highlight), color = "black", size = 0.2) +
  scale_fill_manual(
    values = c(
      "Nakhon Nayok" = "purple",
      "Other" = "lightgray"
    )
  ) +
  labs(
    fill = "Province"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.07, "in"),
    pad_y = unit(0.35, "in"),
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering
  )


################################################################################

##ASSEMBLE MAPS IN PANEL##

final_map <- thailand + districts + polygons_kk + polygons_tc +
  plot_annotation(tag_levels = 'A')

ggsave(file = "final_maps.png", plot = final_map, width = 16, height = 4, dpi = 300)

################################################################################

