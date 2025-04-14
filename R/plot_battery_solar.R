library(ggplot2)

# DEFS
source("aos_functions/defs/plot_themes.R")

plot_battery_solar <- function(node_health_df, selected_node_id) {
    selected_records <- subset.data.frame(node_health_df, node_id == selected_node_id)
    plot <- ggplot(selected_records) +
      geom_point(aes(x = time, y = battery, colour = "Battery")) +
      geom_line(aes(x = time, y = solar_volts, colour = "Solar")) +
      scale_color_manual(values=c('blue', 'red')) +
        # geom_point(aes(x = time, y = battery), colour = "blue") +
        # geom_line(aes(x = time, y = solar_volts), colour = "red") +
        ggtitle(paste("Battery & Solar: ", selected_node_id)) +
        xlab("Time (UTC)") +
        ylab("Voltage (V)") +
        classic_plot_theme +
        theme(legend.title = element_blank(),
              legend.position = 'bottom')

    return(plot)
}
