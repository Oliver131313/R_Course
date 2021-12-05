rm(list = ls()) # Clear workspace


### Na zahrati
library(tidyverse)


seatbelts <- datasets::Seatbelts %>%
    as.matrix()

seatbelts <-
    data.frame(as.matrix(seatbelts),
               year = time(seatbelts),
               month = cycle(seatbelts)) %>%
    as_tibble() %>%
    mutate(
        year = as.character(year) %>% str_extract("^\\d{4}"),
        month = ifelse(month < 10, str_c("0", month), as.character(month)),
        date = str_c(year, "-", month, "-01") %>% as.Date()
    )

# Casova rada
seatbelts %>%
    ggplot(aes(x = date, y = drivers)) + 
        geom_line()

# Farebne rozlisenie a kustomizacia legendy
seatbelts %>%
    ggplot(aes(x = date, y = drivers, color=as.character(law))) + 
    geom_line() + 
    scale_color_discrete(name = "Zakon o pasech", breaks=c(0, 1), labels = c("Ano", "Ne"))


# Pridanie rectu pre intervenciu
seatbelts %>%
    ggplot(aes(x = date, y = drivers)) + 
    annotate("rect", 
             xmin=as.Date('1983-01-31'), 
             xmax=max(seatbelts$date), 
             ymin=-Inf,
             ymax=Inf) + 
    geom_line()

# Pridanie trendovych linii pre rozne obdobia
seatbelts %>%
    ggplot(aes(x = date, y = drivers)) + 
    geom_line() +
    geom_smooth(aes(group=law), method="loess")


### Cviceni A
oly12 <- VGAMdata::oly12 %>% 
    as_tibble()

# Scatter-plot
oly12 %>% 
    ggplot(aes(x=Height, y=Weight, color=Sex)) + 
    geom_jitter(alpha=0.3, na.rm = TRUE)  # alebo + geom_point(alpha=0.3)

# 2D histogram
oly12 %>% 
    ggplot(
        aes(x = Height, y = Weight)
    ) +
    geom_hex(na.rm=TRUE)

# 2D histogram
oly12 %>% 
    ggplot(
        aes(x = Height, y = Weight)
    ) +
    geom_hex(na.rm=TRUE) + 
    facet_grid("Sex", scales="free") + 
    scale_fill_gradientn(colours = viridis::inferno(5))


# 2D histogram s priemermy jednotlivych skupin
oly12_stats <- oly12 %>% 
    mutate(
        Sport = str_extract(Sport, "[:alnum:]*")
    ) %>% 
    group_by(Sex,Sport) %>% 
    summarise(
        Height = mean(Height, na.rm = TRUE),
        Weight = mean(Weight, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    drop_na() 


oly12 %>% 
    ggplot(
        aes(x = Height, y = Weight)
    ) +
    geom_hex() +
    geom_point(
        data = oly12_stats, # Tu vidno, ze pridavame data!
        aes(shape = Sport),
        size = 3,
        color = "red"
    ) +
    scale_shape_manual(values = c(65:90)) +
    facet_wrap("Sex", scales = "free")


# Kernel Desnity Plot
oly12 %>% 
    ggplot(
        aes(x = Height, y = Weight, color=Sex)
    ) +
    geom_density_2d(na.rm=FALSE, bins=10)


### Cviceni B: Rozdelenie spojitej premennej
datax <- tibble(
    x = rnorm(1000)
)

datax %>% 
    ggplot(aes(x)) + 
    geom_histogram(aes(y=..density..), bins=50) + 
    geom_density() + 
    stat_function(fun=dnorm,
                  color="red")

### Cviceni C
library(readxl)
library(countrycode)

cpi <- read_excel("gapminder_cpi.xlsx") %>% 
    rename(country = 1) %>% 
    gather(year,cpi, -country, convert = TRUE)

hdi <- read_excel("gapminder_hdi.xlsx") %>% 
    rename(country = HDI) %>% 
    gather(year,hdi, -country, convert = TRUE)

plot_data <- left_join(cpi, hdi, by = c("country", "year")) %>% 
    gather(variable,value,-country,-year) %>% 
    drop_na() %>% 
    arrange(desc(year)) %>% 
    group_by(country,variable) %>% 
    slice(1L) %>% 
    select(-year) %>% 
    spread(variable, value) %>% 
    ungroup() %>% 
    drop_na() -> plot_data

plot_data <- read_tsv("country_labels.tsv", col_types = "cc") %>% 
    left_join(plot_data,., by="country")

plot_data$continent <- plot_data$country %>% 
    countrycode("country.name","continent")


plot_data %>% 
    ggplot(aes(x=cpi, y=hdi)) +
    geom_smooth(se=FALSE,
                color="red",
                formula = y ~ log(x),
                method="lm") + 
    geom_point(aes(color=continent),
               shape="o",
               size=4,
               fill="white",
               stroke=1.2) +
    ggrepel::geom_text_repel(
        aes(label = country_label)
    ) -> p
    

# !!! Najprv urobit statisticku transformaciu a az potom obmedzit osi!
p + scale_y_continuous(name="HDI, 2011 (1=Best)",
                       breaks=seq(from=0, to=1.1, by=0.1)) +
    scale_x_continuous(name="CPI, 2011 (10=Least Corrupt)",
                       breaks=0:11) + 
    coord_cartesian(xlim=c(0.99, 10.01), ylim=c(0.2, 1)) +
    labs(
        title = "Corruption and human development",
        caption = "Source: Gapminder"
    ) -> p

p + theme_classic() +
    theme(
        # Přidáme linky na pozadí
        panel.grid.major.y = element_line(size = 0.1),
        # Nastavíme podobu os
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(lineend = "square"),
        # Nastavíme pozici a podobu legendy
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        # nastavíme podobu popisků
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "italic")
    )
    
    

