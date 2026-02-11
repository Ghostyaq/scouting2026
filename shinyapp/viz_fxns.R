# Visualization functions

# Endgame plot (penciled: Margaret)
# [intended for the match tab and compare teams tab]
# This one should show proportion of none/l1/l2/l3 AS WELL as auto
# use ggplot patterns (https://r-graph-gallery.com/package/ggpattern.html)
# as well as colors to express both variables (auto climb vs endgame climb)
# to be extra awesome, include the % of fast climbs as the color intensity

# Total points stacked bar chart (Mitchell)
# [intended for match tab, compare teams, event summary]
# Avg climb pts + tele fuel pridge + auto fuel pridge
# Should work for 1 team - 6 teams

# Cycles plot (penciled: Chloe)
# [intended for match tab + compare teams tab]
# Show distribution of cycles per match for multiple teams

# Bump/Trench plot (needs someone!)
# [intended for the match tab and the compare teams tab]
# Show the distribution of bump/trench crossings
# Good spot for geom_jitter with an overlaid boxplot plus coloring the jittered
# points to show bump vs trench.
# link: https://ggplot2.tidyverse.org/reference/geom_jitter.html

# Driver rating history (needs someone!)
# See 2025 single team tab; overlay a moving average to the plot; this was super
# useful last year

# Table of team-level summary statistics (needs someone!) 
# [intended for match tab, compare teams]
# Code should compute a table of summary level statistics for each robot.
# Shinyapp side will need a DT to display; ideally we could use gt() to make
# the table look *good*. 