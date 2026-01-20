#!/usr/bin/env Rscript
# Quick test to verify texture triangle rendering works

library(ggplot2)

# Test data
test_data <- data.frame(
    Sample_ID = c("S1", "S2", "S3", "S4", "S5"),
    Clay = c(20, 30, 15, 40, 25),
    Sand = c(50, 40, 60, 30, 45),
    Silt = c(30, 30, 25, 30, 30)
)

cat("Testing texture triangle rendering...\n")
cat("Test data:\n")
print(test_data)

# Test if ggtern is available
if (requireNamespace("ggtern", quietly = TRUE)) {
    cat("\n✓ ggtern is available - will use professional ternary plot\n")
    library(ggtern)

    p <- ggplot(test_data, aes(x = Sand, y = Clay, z = Silt)) +
        geom_point(alpha = 0.6, size = 3, color = "#00ff88") +
        theme_bw() +
        theme_showarrows() +
        labs(
            title = "USDA Soil Texture Classification",
            x = "Sand %",
            y = "Clay %",
            z = "Silt %"
        )

    # Save test plot
    ggsave("test_texture_triangle.png", p, width = 6, height = 6, dpi = 150)
    cat("✓ Test plot saved to test_texture_triangle.png\n")
} else {
    cat("\n⚠ ggtern not available - will use base R fallback\n")

    # Test base R version
    png("test_texture_triangle.png", width = 600, height = 600)
    par(mar = c(4, 4, 3, 2), bg = "#2d2d2d")

    plot(c(0, 100, 50, 0), c(0, 0, 86.6, 0),
        type = "l", col = "white", lwd = 2,
        xlim = c(-10, 110), ylim = c(-10, 95),
        xlab = "", ylab = "", axes = FALSE, asp = 1,
        main = "Soil Texture Triangle", col.main = "white"
    )

    for (i in 1:nrow(test_data)) {
        clay <- test_data$Clay[i]
        sand <- test_data$Sand[i]
        x <- 50 - (clay * 0.5) + (sand * 0.5)
        y <- clay * 0.866
        points(x, y, pch = 19, col = "#00ff88", cex = 1.5)
    }

    text(50, -5, "Sand %", col = "white", cex = 1.2, font = 2)
    text(-5, 43, "Clay %", col = "white", cex = 1.2, font = 2, srt = 60)
    text(105, 43, "Silt %", col = "white", cex = 1.2, font = 2, srt = -60)

    dev.off()
    cat("✓ Test plot saved to test_texture_triangle.png (base R version)\n")
}

cat("\n✓ Texture triangle test completed successfully!\n")
