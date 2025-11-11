# Radar Plot Comparison: fmsb vs ggplot2

This project includes two different approaches for creating radar plots:

## 1. fmsb Package (radar_plot.R)

### Pros:
- **Specialized for radar charts** - designed specifically for this purpose
- **Simple syntax** - straightforward data structure (max, min, values)
- **Classic radar chart appearance** - traditional spider/star plot look
- **Minimal dependencies** - only requires fmsb package

### Cons:
- **Less customization** - limited styling options
- **Base R graphics** - doesn't integrate with ggplot2 ecosystem
- **Limited themes** - harder to match with other visualizations

### Usage:
```r
source('R/radar_plot.R')
create_school_radar("Wheat Ridge", save_file = "wheat_ridge_fmsb.png")
```

### Best For:
- Quick, traditional radar charts
- Standalone visualizations
- When you prefer base R graphics

---

## 2. ggplot2 Package (radar_plot_ggplot.R)

### Pros:
- **Full ggplot2 customization** - themes, colors, labels, etc.
- **Tidyverse integration** - works seamlessly with dplyr, tidyr
- **Better for reports** - consistent styling with other ggplot2 charts
- **More modern appearance** - cleaner, publication-ready graphics
- **Returns plot object** - can be further modified after creation
- **Better saving options** - ggsave with high-quality output

### Cons:
- **More complex setup** - requires data transformation
- **Additional dependencies** - needs ggplot2, tidyr
- **Learning curve** - requires understanding of coord_polar()

### Usage:
```r
source('R/radar_plot_ggplot.R')
create_school_radar_ggplot("Wheat Ridge", save_file = "wheat_ridge_ggplot.png")

# Can further customize the returned plot
p <- create_school_radar_ggplot("Wheat Ridge")
p + theme_dark()  # Apply different theme
```

### Best For:
- Integration with existing ggplot2 workflows
- Publication-quality graphics
- When you need extensive customization
- Creating multiple plots with consistent styling

---

## Side-by-Side Comparison

| Feature | fmsb | ggplot2 |
|---------|------|---------|
| **Ease of Use** | ⭐⭐⭐⭐⭐ Simple | ⭐⭐⭐⭐ Moderate |
| **Customization** | ⭐⭐⭐ Limited | ⭐⭐⭐⭐⭐ Extensive |
| **Visual Quality** | ⭐⭐⭐⭐ Good | ⭐⭐⭐⭐⭐ Excellent |
| **Integration** | ⭐⭐ Base R | ⭐⭐⭐⭐⭐ Tidyverse |
| **File Size** | Smaller PNGs | Larger PNGs (higher quality) |
| **Plot Modification** | ❌ Not possible | ✅ Full control |
| **Themes** | ❌ None | ✅ theme_minimal(), theme_bw(), etc. |

---

## Example: Creating Multiple Plots

### fmsb approach:
```r
schools <- c("Wheat Ridge", "Palisade HS", "Pueblo County HS")
for (school in schools) {
    create_school_radar(school, save_file = paste0("plots/", school, "_fmsb.png"))
}
```

### ggplot2 approach:
```r
schools <- c("Wheat Ridge", "Palisade HS", "Pueblo County HS")

# Apply consistent theme to all plots
for (school in schools) {
    p <- create_school_radar_ggplot(school)
    p <- p + theme_classic() +
         theme(plot.background = element_rect(fill = "white"))
    ggsave(paste0("plots/", school, "_ggplot.png"), p, width = 8, height = 8)
}
```

---

## Recommendation

- **Use fmsb** if you need quick, simple radar charts and don't need customization
- **Use ggplot2** if you're already using ggplot2 for other visualizations or need publication-quality output
- **Both work!** Choose based on your workflow and requirements

---

## Advanced ggplot2 Customization Examples

```r
# Create a plot and customize it
p <- create_school_radar_ggplot("Wheat Ridge")

# Option 1: Dark theme
p + theme_dark() +
    theme(plot.background = element_rect(fill = "black"))

# Option 2: Custom colors
p <- ggplot(plot_data, aes(x = Metric, y = Value, group = 1)) +
    geom_polygon(fill = alpha("darkred", 0.4), color = "darkred", linewidth = 2) +
    geom_point(color = "darkred", size = 5) +
    coord_polar() +
    ylim(0, 20) +
    theme_minimal()

# Option 3: Faceted comparison (multiple schools)
# See R/compare_schools_radar.R for full example
```
