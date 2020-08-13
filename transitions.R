library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(patchwork)
library(forcats)
library(extrafont)
library(magick)
library(glue)
library(stringr)
library(ggforce)
library(scales)
library(ggtext)
library(grid)

width <- 2.95
height <- 2.5
comic_font <- "Comic Book Commando"

# Data ----

comic_transitions <- readr::read_csv("comic_transitions.csv")

comic_transitions_long <- comic_transitions %>%
  pivot_longer(
    cols = c(`1`:`6`),
    names_to = "transition",
    values_to = "prop"
  ) %>%
  mutate(
    transition = as.numeric(transition),
    across(c(comic, author), toupper),
    comic = fct_inorder(comic),
    prop_zero = coalesce(prop, 0)
  )

# Functions ----

theme_comic <- function(label = c("comic_author", "transitions"), font_size = 16, family = comic_font) {
  label <- match.arg(label)

  theme <- theme_classic(base_family = family) +
    theme(
      panel.background = element_rect(fill = "white", colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.minor.y = element_line(colour = "black", size = 0.25),
      panel.grid.minor.x = element_line(colour = "black", size = 0.25),
      panel.grid.major = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.caption = element_text(colour = "black", family = family, size = font_size - 1, hjust = 0.5, lineheight = 0.75),
      plot.margin = margin(10, 10, 10, 10)
    )

  if (label == "transitions") {
    theme +
      theme(axis.text.x = element_text(colour = "black", family = family, size = font_size, vjust = 1))
  } else if (label == "comic_author") {
    theme +
      theme(
        axis.title.x = element_text(size = font_size, vjust = -1, family = family),
        plot.title = element_text(size = font_size, hjust = 0.5, vjust = 2, family = family)
      )
  }
}

plot_comic_transitions <- function(data, label = c("comic_author", "transitions"), font_size = 16, family = comic_font, alpha = 0.9) {
  label <- match.arg(label)

  ggplot(data, aes(x = transition, y = prop_zero)) +
    geom_col(width = 1, colour = "black", fill = "black", alpha = alpha) +
    scale_y_continuous(minor_breaks = seq(0.1, 1, 0.101), limits = c(0, 1)) +
    scale_x_continuous(minor_breaks = seq(1.5, 5.5, 1), breaks = 1:6, labels = 1:6, position = ifelse(label == "transitions", "top", "bottom")) +
    labs(x = ifelse(label == "comic_author", unique(data[["author"]]), ""), y = NULL, title = ifelse(label == "comic_author", unique(as.character(data[["comic"]])), "")) +
    coord_cartesian(expand = FALSE) +
    theme_comic(label = label, font_size = font_size, family = family)
}

plot_category <- function(data) {
  category_split <- data %>%
    split(.$comic) %>%
    keep(~ nrow(.x) > 0)

  plots <- category_split %>%
    map(plot_comic_transitions, font_size = 10) %>%
    map(~ .x + theme(plot.margin = margin(5, 5, 5, 5)))

  wrap_plots(plots, ncol = length(category_split) / 2)
}

# List of categories ----

categories <- tribble(
  ~number, ~text,
  1, "moment-to-moment",
  2, "action-to-action",
  3, "subject-to-subject",
  4, "scene-to-scene",
  5, "aspect-to-aspect",
  6, "non-sequitur"
) %>%
  mutate(
    text = str_replace_all(text, "-", "-\n"),
    placement = abs(number - max(number))
  )

list_of_categories <- ggplot(categories, aes(x = 1.5, y = placement)) +
  geom_col(aes(x = 2, y = 6), fill = NA) +
  geom_text(aes(label = glue("{number}.")), vjust = -2, hjust = 0, size = 12, family = comic_font, colour = "white") +
  geom_text(aes(label = text), hjust = 0, size = 8, family = comic_font, lineheight = 0.6, colour = "white") +
  coord_cartesian(ylim = c(0, 5.4), xlim = c(1.5, 2.1)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"))

ggsave("plots/categories.png", list_of_categories, width = 1.25, height = 8.5)

# Intro panel ----

bubble_font_size <- 5
bubble_size <- 1

intro <- ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 1.5, b = 1, angle = 0), size = bubble_size) +
  geom_circle(aes(x0 = -0.88, y0 = -0.63, r = 0.27), fill = "white", colour = NA_character_) +
  geom_curve(aes(x = -1.5, xend = -1.15, y = -1, yend = -0.64), curvature = 0.25, size = bubble_size, lineend = "round") +
  geom_curve(aes(x = -1.5, xend = -0.74, y = -1, yend = -0.87), curvature = 0.25, size = bubble_size, lineend = "round") +
  geom_richtext(aes(x = 0, y = 0, label = "Panel-to-panel<br> transitions in<br>comics can be<br>placed in<br>**six categories**."),
    family = comic_font, size = bubble_font_size,
    lineheight = 0.75, label.color = NA
  ) +
  geom_ellipse(aes(x0 = 0.75, y0 = -1.75, a = 1.75, b = 1, angle = 0), size = bubble_size, fill = "white") +
  geom_richtext(aes(x = 0.75, y = -1.75),
    label = "Most mainstream <br>American comics<br>employ techniques<br>introduced by<br>**Jack Kirby** in<br>**Fantastic Four**...", family = comic_font, size = bubble_font_size - 0.25,
    lineheight = 0.75, label.color = NA, fill = NA
  ) +
  theme_void()

ggsave("plots/intro.png", intro, width = width, height = height)

# Fantastic Four viz panel ----

fantastic_four <- comic_transitions_long %>%
  filter(category == "Fantastic Four")

table_font_size <- 6

fantastic_four_table <- fantastic_four %>%
  select(transition, prop) %>%
  mutate(prop_char = case_when(
    is.na(prop) ~ "—",
    TRUE ~ percent(prop, accuracy = 1)
  )) %>%
  mutate(number = abs(transition - max(transition))) %>%
  ggplot() +
  geom_col(aes(x = 2, y = 6), fill = NA) +
  geom_text(aes(x = 1.57, y = number, label = transition), family = comic_font, size = table_font_size, fontface = "bold") +
  geom_text(aes(x = 1.8, y = number, label = prop_char), family = comic_font, size = table_font_size) +
  scale_y_continuous(minor_breaks = seq(-0.5, 5.5, 1), breaks = NULL) +
  scale_x_continuous(minor_breaks = c(1.3, 1.65, 2), breaks = NULL) +
  labs(x = NULL) +
  coord_cartesian(ylim = c(-0.2, 5.2), xlim = c(1.5, 1.9)) +
  theme_comic()

fantastic_four_plot <- plot_comic_transitions(fantastic_four, label = "transitions")

font_size_viz <- 16

fantastic_four_viz_panel <- fantastic_four_table + fantastic_four_plot + plot_layout(widths = c(0.4, 0.5)) + plot_annotation(title = "...where the panel-to-panel<br>transitions break<br>down like this.", theme = theme(plot.title = element_markdown(family = comic_font, hjust = 0.5, size = font_size_viz, margin = margin(0, 0, -10, 0))))

ggsave("plots/fantastic_four_viz.png", fantastic_four_viz_panel, width = width * 0.9, height = height)

# American, European ----

# Only use 6 from each category, easier plotting
set.seed(1234)

sample_comics <- comic_transitions_long %>%
  filter(category != "Fantastic Four") %>%
  distinct(category, comic) %>%
  group_by(category) %>%
  sample_n(6)

comic_categories <- comic_transitions_long %>%
  inner_join(sample_comics, by = c("category", "comic")) %>%
  split(.$category)

comic_categories_plots <- comic_categories %>%
  map(plot_category)

# American

american <- comic_categories_plots[["American"]] +
  plot_annotation(
    title = "A random sample of **American** comics shows<br>the same breakdown pretty consistently.",
    theme = theme(plot.title = element_markdown(family = comic_font, hjust = 0.5, size = font_size_viz))
  )

ggsave("plots/american.png", american, width = width * 1.5, height = height)

# European

european <- comic_categories_plots[["European"]] +
  plot_annotation(
    title = "And **European** artists give similar results.",
    theme = theme(plot.title = element_markdown(family = comic_font, hjust = 0.5, size = font_size_viz))
  )

ggsave("plots/european.png", european, width = width * 1.4, height = height)

# Japanese

# Types explained ----

plot_explanations <- function(data) {
  plot_comic_transitions(data, label = "transitions", alpha = 0.3, font_size = 12)
}

types_24 <- tibble(
  transition = 1:6,
  prop_zero = c(0, 1, 1, 1, 0, 0)
) %>%
  plot_explanations()

type_1 <- tibble(
  transition = 1:6,
  prop_zero = c(1, 0, 0, 0, 0, 0)
) %>%
  plot_explanations()

type_5 <- tibble(
  transition = 1:6,
  prop_zero = c(0, 0, 0, 0, 1, 0)
) %>%
  plot_explanations()

type_6 <- tibble(
  transition = 1:6,
  prop_zero = c(0, 0, 0, 0, 0, 1)
) %>%
  plot_explanations()

transitions_explained <- types_24 + labs(caption = "2-4 show things \nhappening in\nconcise ways.") + type_1 + labs(caption = "type 1 shows \nactions, like type 2,\nbut requires\nmore panels.") + type_5 + labs(caption = "in the 5th type,\nnothing happens.") + type_6 + labs(caption = "type 6 transitions,\nnon-sequiturs, are\n unconcerned with\nevents or narrative.") + plot_layout(nrow = 1) +
  plot_annotation(title = "The predominance of types 2-4 makes sense<br>if we see stories as connected series of events.", theme = theme(plot.title = element_markdown(family = comic_font, hjust = 0.5, size = font_size_viz, margin = margin(0, 0, -10, 0))))

ggsave("plots/transitions_explained.png", transitions_explained, width = width * 2.05, height = height)

# Japanese intro ----

japanese_intro <- ggplot() +
  geom_ellipse(aes(x0 = -3, y0 = 0, a = 1.25, b = 1, angle = 0), size = bubble_size) +
  geom_richtext(aes(x = -3, y = 0, label = "So... that's<br>that?"),
    family = comic_font, size = bubble_font_size,
    lineheight = 0.75, label.color = NA, fill = NA
  ) +
  geom_ellipse(aes(x0 = 0, y0 = -2.5, a = 3.75, b = 2.5, angle = 0), size = bubble_size, fill = "white") +
  geom_richtext(aes(x = 0, y = -2.5),
    label = "Nope! In contrast,<br>
    aspect-to-aspect transitions<br>
    are integral to Japanese comics,<br>
    used to establish a mood where<br>the
                reader must assemble a **single**<br>moment
                using scattered fragments.<br><br>...versus in
                American comics,<br>where the focus
                is on bridging<br>**between** moments.", family = comic_font, size = bubble_font_size - 0.5,
    lineheight = 0.75, label.color = NA, fill = NA
  ) +
  # coord_equal() +
  theme_void()

ggsave("plots/japanese_intro.png", japanese_intro, width = width * 1.15, height = height)

# Japanese ----

japanese <- comic_categories_plots[["Japanese"]] +
  plot_annotation(
    title = "This is reflected in the breakdown of<br>Japanese comics, which have a higher<br>prevalence of type 5 transitions.",
    theme = theme(plot.title = element_markdown(family = comic_font, hjust = 0.5, size = font_size_viz))
  )

ggsave("plots/japanese.png", japanese, width = width * 1.3, height = height)


# Being there over getting there ----

being_there_getting_there <- ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 3, b = 10, angle = 0), size = bubble_size, colour = NA) +
  geom_richtext(aes(x = 0, y = 0),
    label = "The change in proportions<br>is an artifact of the<br>differences between cultures.<br><br>American comics mirror their<br>goal-oriented culture, while<br>Japanese comics pull from<br>their traditional styles of<br>art and storytelling...<br><br>...emphasizing **being there**<br>over **getting there**.", family = comic_font, size = bubble_font_size,
    lineheight = 0.75, label.color = NA, fill = NA
  ) +
  # coord_equal() +
  theme_void()

ggsave("plots/being_there_getting_there.png", being_there_getting_there, width = width * 0.95, height = height)

# Credits ----

ggplot() +
  geom_text(aes(x = 0 , y = 0), label =
"Visualization: Sharla Gelfand; Data and Ideas: Scott McCloud's \"Understanding Comics\"", family = comic_font, size = font_size_viz - 11) +
  theme_void()

ggsave("plots/credits.png", width = 7.5, height = 0.15)

# Assemble panels ----

categories <- image_read("plots/categories.png")

intro <- image_read("plots/intro.png")
fantastic_four_viz <- image_read("plots/fantastic_four_viz.png")
american <- image_read("plots/american.png")

european <- image_read("plots/european.png")
transitions_explained <- image_read("plots/transitions_explained.png")

japanese_intro <- image_read("plots/japanese_intro.png")
japanese <- image_read("plots/japanese.png")
being_there_getting_there <- image_read("plots/being_there_getting_there.png")

credits <- image_read("plots/credits.png")

panels <- list(
  list(
    intro, fantastic_four_viz, american
  ),
  list(european, transitions_explained),
  list(japanese_intro, japanese, being_there_getting_there)
)

blank <- image_blank(width = 12 / 8.5 * image_info(categories)[["height"]], height = image_info(categories)[["height"]], color = "white")

comic <- blank %>%
  image_composite(categories)

height_gap <- 75
width_gap <- 50
height_offset <- height_gap
width_offset <- image_info(categories)[["width"]] + width_gap

for (i in 1:3) {
  for (j in seq_along(panels[[i]])) {
    offset <- glue("+{width_offset}+{height_offset}")

    comic_panel <- panels[[i]][[j]]

    comic <- comic %>%
      image_composite(
        image_border(comic_panel, color = "black", geometry = "5x5"),
        offset = offset
      )

    width_offset <- width_offset + image_info(comic_panel)[["width"]] + width_gap
  }
  width_offset <- image_info(categories)[["width"]] + width_gap
  height_offset <- height_offset + image_info(comic_panel)[["height"]] + height_gap
}

comic <- comic %>%
  image_composite(credits, offset = glue("+{image_info(blank)[['width']]*0.365}+{image_info(categories)[['height']] - 55}")) %>%
  image_noise()

image_write(comic, "comic.png")

image_write(
  image_scale(comic, glue("{image_info(comic)[['width']]/3}x")),
  "comic_small.png"
)
