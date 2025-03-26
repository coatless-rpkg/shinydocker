from shiny import App, ui, render
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

app = App(app_ui, server)
