# DeepLearningDemos

Inspired by various academic texts, we use code to help explore
features of machine learning methods.

## Shiny App: Interactive 3×3 ReLU Plot

An interactive Shiny app lets you explore how linear functions,
ReLU activation, and scaling interact across a 3×3 panel grid,
plus a summary plot showing the weighted sum of scaled ReLU outputs.

### Files

| File | Description |
|------|-------------|
| `plot_3x3_relu_scaled.R` | Core plotting function (`plot_3x3_relu_scaled()`) and parameter-chooser helper |
| `shiny/plot_relu_scaled/app.R` | Shiny app that wraps the plotting function with interactive sliders |

### Running the app locally

**Prerequisites:** R with the `shiny` package installed.

```r
install.packages("shiny")   # if not already installed
```

From the **repo root**, run:

```r
shiny::runApp("shiny/plot_relu_scaled")
```

Or open `shiny/plot_relu_scaled/app.R` in RStudio and click **Run App**.

### Controls

| Slider group | Parameters | Range |
| --- | --- | --- |
| Intercepts | `a0[1]`, `a0[2]`, `a0[3]` | −2 to 2 |
| Slopes | `a1[1]`, `a1[2]`, `a1[3]` | −2 to 2 |
| Scale factors | `b[1]`, `b[2]`, `b[3]` | −3 to 3 |
| Summary intercept | `b0` | −3 to 3 |
| x range | `xlim` min/max | −1 to 5 |
| Points | `n` | 50 to 2000 |

Default values correspond to the parameter set
`a0 = c(-0.1, -0.2, 1.1)`, `a1 = c(0.3, 0.3354, -0.7)`,
`b = c(-1, 1.4912, 1.4437)`, `b0 = 0.5`.
Note that `b` values may be negative; the app handles this correctly.

A summary plot below the 3×3 grid shows:

```
y_total(x) = b0 + b[1]*ReLU(a0[1] + a1[1]*x)
                + b[2]*ReLU(a0[2] + a1[2]*x)
                + b[3]*ReLU(a0[3] + a1[3]*x)
```
