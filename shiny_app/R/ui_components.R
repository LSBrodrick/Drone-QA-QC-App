# ==============================================================================
# UI COMPONENT FUNCTIONS
# Reusable UI components for the Shiny application
# ==============================================================================

# Create a styled value box
create_value_box <- function(value, subtitle, icon_name, color) {
  div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      h3(value),
      p(subtitle)
    ),
    div(
      class = "icon",
      icon(icon_name)
    )
  )
}

# Create a progress indicator
create_progress <- function(value, text, color = "primary") {
  div(
    class = "progress",
    div(
      class = paste0("progress-bar progress-bar-", color),
      role = "progressbar",
      style = paste0("width: ", value, "%"),
      `aria-valuenow` = value,
      `aria-valuemin` = "0",
      `aria-valuemax` = "100",
      paste0(value, "%")
    )
  )
}

# Create an alert box
create_alert <- function(title, message, type = "info") {
  div(
    class = paste0("alert alert-", type, " alert-dismissible"),
    button(
      type = "button",
      class = "close",
      `data-dismiss` = "alert",
      `aria-hidden` = "true",
      HTML("&times;")
    ),
    h4(icon(switch(type,
                  success = "check",
                  info = "info-circle",
                  warning = "exclamation-triangle",
                  danger = "ban")), 
       title),
    message
  )
}

# Create a stat card
create_stat_card <- function(title, value, subtitle = NULL, icon_name = NULL, color = "primary") {
  div(
    class = "info-box",
    span(
      class = paste0("info-box-icon bg-", color),
      icon(icon_name)
    ),
    div(
      class = "info-box-content",
      span(class = "info-box-text", title),
      span(class = "info-box-number", value),
      if (!is.null(subtitle)) {
        div(class = "progress-description", subtitle)
      }
    )
  )
}

# Create a loading spinner
create_loading_spinner <- function(text = "Loading...") {
  div(
    class = "text-center",
    style = "padding: 50px;",
    div(
      class = "spinner-border text-primary",
      role = "status",
      span(class = "sr-only", "Loading...")
    ),
    h4(text, style = "margin-top: 20px;")
  )
}