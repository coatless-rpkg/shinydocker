# Create a simple Shiny for Python app
app_dir <- tempdir()
writeLines(
  'from shiny import App, ui, render
import numpy as np
import matplotlib.pyplot as plt

app_ui = ui.page_fluid(
    ui.panel_title("Hello Docker"),
    ui.layout_sidebar(
        ui.sidebar(
            ui.input_slider("obs", "Number of observations:", min=1, max=1000, value=500)
        ),
        ui.output_plot("distPlot")
    )
)

def server(input, output, session):
    @output
    @render.plot
    def distPlot():
        data = np.random.normal(size=input.obs())
        fig, ax = plt.subplots()
        ax.hist(data)
        return fig

app = App(app_ui, server)',
file.path(app_dir, "app.py")
)

# Export the app
shinydocker::export(app_dir, run = TRUE, detached = TRUE)

# Stop the container
stop_container(app_dir)

# Restart the container:
shinydocker::run_container(app_dir, detach = TRUE)

## Alternatively, steps can be run separately ----
# Create Docker configuration
# shinydocker::dockerize(app_dir)
# Build Docker image
# shinydocker::build_image(app_dir)
# Run the containerized app
# shinydocker::run_container(app_dir, detached = TRUE)