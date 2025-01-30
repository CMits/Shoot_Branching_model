# modules/network_module.R
networkHomeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$style(HTML(".home-container {
                 text-align: center;
                 padding: 100px 20px;
                 background: linear-gradient(to bottom right, #8e44ad, #2980b9);
                 color: white;
                 border-radius: 10px;
                 height: 100vh;
                 overflow: hidden;
                 position: relative;
               }
               .home-title {
                 font-size: 6rem;
                 font-weight: bold;
                 margin-bottom: 60px;
                 font-family: 'Press Start 2P', cursive;
                 letter-spacing: 5px;
                 text-shadow: 3px 3px 5px rgba(0, 0, 0, 0.4);
               }
               .home-network {
                 margin: 50px auto;
                 display: flex;
                 justify-content: center;
                 align-items: center;
               }
               .network-svg {
                 width: 400px;
                 height: 400px;
               }
               .home-subtitle {
                 font-size: 3rem;
                 font-weight: bold;
                 margin-top: 70px;
                 font-family: 'Bungee', cursive;
                 animation: fadeInOut 3s infinite;
                 text-shadow: 2px 2px 5px rgba(0, 0, 0, 0.5);
                 letter-spacing: 2px;
                 position: relative;
                 cursor: pointer;
               }
               .home-subtitle:hover::after {
                 content: 'Click here for a demo';
                 display: block;
                 font-size: 1rem;
                 margin-top: 10px;
                 color: #f7dc6f;
                 font-weight: bold;
               }
               @keyframes fadeInOut {
                 0% { opacity: 1; }
                 50% { opacity: 0.5; }
                 100% { opacity: 1; }
               }")),
      
      tags$head(
        tags$link(
          href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap", 
          rel = "stylesheet"
        )
      ),
      
      div(class = "home-container",
          h1(class = "home-title", "PSoup"),
          
          div(class = "home-network",
              tags$svg(
                class = "network-svg",
                width = "400", height = "400",
                tags$circle(cx = "100", cy = "100", r = "30", fill = "#f1c40f"),
                tags$circle(cx = "300", cy = "100", r = "30", fill = "#e74c3c"),
                tags$circle(cx = "200", cy = "300", r = "30", fill = "#2ecc71"),
                tags$circle(cx = "100", cy = "250", r = "30", fill = "#3498db"),
                tags$circle(cx = "300", cy = "250", r = "30", fill = "#9b59b6"),
                tags$line(x1 = "100", y1 = "100", x2 = "300", y2 = "100", stroke = "white", `stroke-width` = "4"),
                tags$line(x1 = "300", y1 = "100", x2 = "200", y2 = "300", stroke = "white", `stroke-width` = "4"),
                tags$line(x1 = "200", y1 = "300", x2 = "100", y2 = "250", stroke = "white", `stroke-width` = "4"),
                tags$line(x1 = "100", y1 = "250", x2 = "300", y2 = "250", stroke = "white", `stroke-width` = "4"),
                tags$line(x1 = "300", y1 = "250", x2 = "100", y2 = "100", stroke = "white", `stroke-width` = "4")
              )
          ),
          
          p(class = "home-subtitle", "Get ready to stir your networks")
      )
    )
  )
}

networkHomeServer <- function(input, output, session) {
  # No server-side logic required for static home page
}
