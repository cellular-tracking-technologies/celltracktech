library(ggplot2)

classic_plot_theme <- theme(
    axis.text.x = element_text(size = 20, vjust = 0.5),
    axis.text.y = element_text(size = 20, vjust = 0.5),
    axis.title.x = element_text(size = 20, vjust = 0.5),
    axis.title.y = element_text(size = 20, vjust = 0.5),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    panel.grid.major = element_line(colour = "grey", linewidth = 0.25),
    panel.grid.minor = element_line(colour = "grey", linewidth = 0.1),
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "inches"),
    plot.title = element_text(
        size = 25,
        face = "bold",
        hjust = 0.5
    ),

)

tag_hist_plot_theme <- theme(
    # axis.text.x = element_text(size = 20, vjust = 0.5),
    axis.text.y = element_text(family = "courier"),
    # axis.title.x = element_text(size = 20, vjust = 0.5),
    # axis.title.y = element_text(size = 20, vjust = 0.5),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    panel.grid.major = element_line(colour = "grey", linewidth = 0.25),
    panel.grid.minor = element_line(colour = "grey", linewidth = 0.1),
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "inches"),
    plot.title = element_text(
        size = 25,
        face = "bold",
        hjust = 0.5
    ),
)
