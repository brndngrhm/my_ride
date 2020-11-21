# Load library
library(profvis)

# Run profiler on shiny app with optional arg to save output
profvis({
  runApp('app')},
  prof_output = 'app/profile_results/'
)


# Assign to variable
p <- profvis(prof_input = 'app/profile_results/file70004ed374c7.Rprof')

# Save as a webpage
htmlwidgets::saveWidget(p, "profile.html")
