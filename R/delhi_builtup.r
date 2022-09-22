################################################################################
#                 Mapping OSM and satelitte data with terra in R
#                 Milos Popovic
#                 2022/09/22
################################################################################

# libraries we need
libs <- c(
    "tidyverse", "sf",
    "osmdata", "terra",
    "httr", "XML", "lwgeom"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Montserrat font
sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# 1. GET DELHI OSM DATA
#----------------------

city <- "Delhi, India"

get_bounding_box <- function() {
    bbox <- osmdata::getbb(city)
    return(bbox)
}

bbox <- get_bounding_box()

road_tags <- c(
    "motorway", "trunk",
    "primary", "secondary",
    "tertiary", "motorway_link",
    "trunk_link", "primary_link",
    "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
    roads <- bbox %>%
        opq() %>%
        add_osm_feature(
            key = "highway",
            value = road_tags
        ) %>%
        osmdata_sf()

    return(roads)
}

roads <- get_osm_roads()

# plot roads
plot(sf::st_geometry(roads$osm_lines))

# 2. GET BUILT-UP DATA
#----------------------

# website
url <- "https://glad.umd.edu/users/Potapov/GLCLUC2020/Built-up_change_2000_2020/"

get_raster_links <- function() {
    # make http request
    res <- GET(url)
    # parse data to html format
    parse <- XML::htmlParse(res)
    # scrape all the href tags
    links <- XML::xpathSApply(parse, path = "//a", xmlGetAttr, "href")
    # grab links
    lnks <- links[-c(1:5)]
    # make all links and store in a list
    for (l in lnks) {
        rlinks <- paste0(url, lnks)
    }

    return(rlinks)
}

rlinks <- get_raster_links()

# bbox values
delhi_ext <- unname(c(
    bbox[1, ][1], bbox[1, ][2],
    bbox[2, ][1], bbox[2, ][2]
))

delhi_ext

# create Delhi extent
de <- c(77.06194, 77.38194, 28.49172, 28.81172)

get_builtup_data <- function() {
    l <- rlinks[grepl("30N_070E", rlinks)]
    ras <- terra::rast(l)
    delhi_ras <- terra::crop(ras, de)
    df <- terra::as.data.frame(delhi_ras, xy = T)
    names(df)[3] <- "value"
    # define categorical values
    df$cat <- round(df$value, 0)
    df$cat <- factor(df$cat,
        labels = c("no built-up", "new", "existing")
    )

    return(df)
}

df <- get_builtup_data()

colrs <- c(
    "grey20", "#FCDD0F", "#287DFC"
)

p <- ggplot() +
    geom_raster(
        data = df,
        aes(x = x, y = y, fill = cat),
        alpha = 1
    ) +
    geom_sf(
        data = roads$osm_lines,
        color = "grey20",
        size = .1,
        alpha = 1,
        fill = "transparent"
    ) +
    scale_fill_manual(
        name = "",
        values = colrs,
        drop = F
    ) +
    guides(
        color = "none",
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.5, units = "mm"),
            keywidth = unit(35, units = "mm"),
            title.position = "top",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "bottom"
        )
    ) +
    theme_minimal() +
    theme(
        text = element_text(family = "Montserrat"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.5, .9),
        legend.text = element_text(size = 60, color = "white"),
        legend.title = element_text(size = 80, color = "white"),
        legend.spacing.y = unit(0.25, "cm"),
        panel.grid.major = element_line(color = "grey20", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            size = 80, color = "grey80", hjust = .5, vjust = -30
        ),
        plot.caption = element_text(
            size = 40, color = "grey90", hjust = .5, vjust = 65
        ),
        plot.subtitle = element_text(
            size = 22, color = "#f5de7a", hjust = .5
        ),
        plot.margin = unit(
            c(t = -5, r = -4.5, b = -4.5, l = -5), "lines"
        ),
        plot.background = element_rect(fill = "grey20", color = NA),
        panel.background = element_rect(fill = "grey20", color = NA),
        legend.background = element_rect(fill = "grey20", color = NA),
        legend.key = element_rect(colour = "white"),
        panel.border = element_blank()
    ) +
    labs(
        x = "",
        y = NULL,
        title = "Built-up Land Change in Delhi, 2000-2020",
        subtitle = "",
        caption = "©2022 Milos Popovic (https://milospopovic.net) | Data: GLAD Buil-up Change Data & ©OpenStreetMap contributors"
    )

ggsave(
    filename = "delhi_built_up2.png",
    width = 6, height = 9, dpi = 600,
    device = "png", p
)
