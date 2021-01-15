# Setup -------------------------------------------------------------------

# status message
cat("\nCheck if packages need to be installed and install if needed...")
Sys.sleep(2)

# required packages
requiredpack <- c(
  "tidyverse",
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "plotly",
  "reactable",
  "XML",
  "tictoc"
)
# already installed packages
installedpack <- as.data.frame(installed.packages())$Package
# extract missing packages i.e. required packages that are not installed
missingpack <- requiredpack[!is.element(requiredpack, installedpack)]
# install missing packages if there is more than 0 missing packages
if (length(missingpack) > 0) install.packages(missingpack, repos = "https://cran.rstudio.com/")

# status message
cat("\n\nLoad required packages...\n")
Sys.sleep(2)

# load required packages
library(shiny) # shiny package
library(shinydashboard) # shinydashboard package
library(shinyWidgets) # shinyWidgets package
library(tictoc)
library(readxl) # tidyverse package
library(magrittr) # tidyverse package
library(plotly) # plotly package
library(reactable) # reactable package
library(XML) # XML package
library(tidyverse) # tidyverse package

# working directory
# function to get current file location
# from 'https://stackoverflow.com/questions/47044068/get-the-path-of-current-script'
# see Juan Bernabe's answer
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}
# get this script's directory
sdir <- getCurrentFileLocation()
# set working directory as script's directory
setwd(sdir)

# User defined functions --------------------------------------------------

# reads and parse HTML outputs from FM
# import all data as characters
# uses UTF-8 encoding to allow for special characters such as currency symbols
parse_FMoutput <- function(htmlpath) {
  # read HTML table using UTF-8 encoding
  data <- readHTMLTable(htmlpath, header = TRUE, encoding = "UTF-8") %>% .[[1]]
  # make columns unique in case of duplicate names
  colnames(data) %<>% make.unique(sep = "")
  # return data
  data
}

# position parsing
# example1: "D/M(C)" to "D(C), M(C)"
# example2: "D/WB(L), M/AM(L), ST(C)" to "D(L), WB(L), M(L), AM(L), ST(C)"
pos_parse <- function(pos) {
  # remove all spaces, split using commas as separators
  # transform the list output into a vector
  # split using opening parenthesis as separator
  # remove all closing parenthesis
  pos %<>% str_remove_all(" ") %>% str_split(",") %>% unlist() %>% #trimws() %>%
    str_split("\\(") %>% sapply(function(x) str_remove_all(x, "\\)"))
  # transform into list if necessary
  # special case if position is one string ("GK" or "DM") do nothing
  if((typeof(pos) != "list") & (length(pos) != 1)) pos %<>% split(col(.))
  # transform list into dataframe
  # first column is the vertical position on the pitch (GK/D/WB/DM/M/AM/ST)
  # second column is the horizontal position on the pitch (L/C/R)
  n <- length(pos)
  pos_df <- data.frame(vpos = rep(NA,n), hpos = rep(NA,n))
  for (i in 1:n) {
    pos_df[i, 1] <- pos[[i]][1]
    pos_df[i, 2] <- pos[[i]][2]
  }
  # replace NAs by ""
  pos_df %<>% replace(is.na(.), "")
  # break the vertical position by using "/" as separators
  # remove original row and add a row for each vertical position
  n <- dim(pos_df)[1]
  temp_pos_df <- data.frame()
  cols_to_remove <- vector()
  for (i in 1:n) {
    k <- str_count(pos_df[i, 1], "/") + 1
    if (k-1 > 0) {
      vpos <- rep(pos_df[i, 1], k)
      hpos <- pos_df[i, 2]
      cols_to_remove <- c(cols_to_remove,i)
      for (j in 1:k) vpos[j] %<>% str_split("/") %>% sapply("[", j)
      df <- data.frame(vpos = vpos, hpos = hpos)
      temp_pos_df %<>% rbind(df)
    }
  }
  if (length(cols_to_remove) > 0) pos_df %<>% slice(-cols_to_remove)
  pos_df %<>% rbind(temp_pos_df)
  # break the horizontal position by using "" as separators
  # remove original row and add a row for each horizontal position
  n <- dim(pos_df)[1]
  temp_pos_df <- data.frame()
  cols_to_remove <- vector()
  for (i in 1:n) {
    k <- nchar(pos_df[i, 2])
    if (k > 1) {
      vpos <- pos_df[i, 1]
      hpos <- rep(pos_df[i, 2], k)
      cols_to_remove <- c(cols_to_remove,i)
      for (j in 1:k) hpos[j] %<>% str_split("") %>% sapply("[", j)
      df <- data.frame(vpos = vpos, hpos = hpos)
      temp_pos_df %<>% rbind(df)
    }
  }
  if (length(cols_to_remove) > 0) pos_df %<>% slice(-cols_to_remove)
  pos_df %<>% rbind(temp_pos_df)
  # custom order for vertical and horizontal positions
  vpos_order <- c("GK", "D", "WB", "DM", "M", "AM", "ST")
  hpos_order <- c("L", "C", "R", "")
  # order the dataframe using the custom order
  # add a column for the position = vertical position(horizontal position)
  pos_df %<>%
    mutate(vpos = factor(vpos, levels = vpos_order)) %>%
    mutate(hpos = factor(hpos, levels = hpos_order)) %>%
    arrange(vpos, hpos) %>%
    mutate(pos = paste0(vpos, "(", hpos, ")"))
  # concatenate all positions into a single strings
  # positions are separated by commas
  # replace special cases DM() & GK() by the FM format DM & GK
  pos_df$pos %>%
    paste0(collapse = ", ") %>%
    str_replace_all("DM\\(\\)", "DM") %>%
    str_replace_all("GK\\(\\)", "GK")
}

# converts to percent format; if NA shows NA
formatPct <- function(x) {
  if_else(
    condition = is.na(x),
    true = NA_character_,
    false = paste0(round(x*100, digits = 1), " %")
  )
}

# cleans the raw data extracted from FM
cleandata <- function(data) {
  
  attributes <- c("Aer","Cmd","Com","Ecc","Han","Kic","1v1","Pun","Ref","TRO",
                  "Thr","Agg","Ant","Bra","Cmp","Cnt","Dec","Det","Fla","Ldr",
                  "OtB","Pos","Tea","Vis","Wor","Acc","Agi","Bal","Jum","Nat",
                  "Pac","Sta","Str","Cor","Cro","Dri","Fin","Fir","Fre","Hea",
                  "Lon","L Th","Mar","Pas","Pen","Tck","Tec")
  
  statschalkboard <- c("Hdrs A","Aer A/90","Hdrs","Hdrs W/90","Hdr %",
                       "K Hdrs","Cr A","Cr C/A","Cr C","Gl Mst","Distance",
                       "Dist/90","Drb","DrbPG","Off","Asts/90","CCC","Ch C/90",
                       "K Pas","K Ps/90","Pas A","Ps A/90","Pas %","Ps C",
                       "Ps C/90","Svh","Svp","Svt","Shots","ShT","ShT/90",
                       "Shot %","Shot/90","Itc","Int/90","K Tck","Tck A",
                       "Tck R","Tck W","Tck1")
  
  statsgeneral <- c("AT Apps","AT Gls","AT Lge Apps","AT Lge Gls","Apps",
                    "Ast","Mins/Gl","Av Rat","Clean sheets","Con/90","FA",
                    "Fls","Gwin","D","Lost","G. Mis","Won","Gls","Conc",
                    "Gls/90","Last 5 FT Games","Last 5 Games","Mins",
                    "Mins/Gm","Last C","Last Gl","Pens","Pens Faced",
                    "Pens Saved","Pens Saved Ratio","Pens S","Pen/R","PoM",
                    "Pts/Gm","Red","Starts","Tcon/90","Tgls","Tcon","Tgls/90",
                    "xG", "Yel","Int Apps","Int Ast","Int Av Rat","Int Conc")
  
  financials <- c("Appearance Fee","Assist Bonus","Cln Sheet Bonus","Goal Bonus",
                  "Int Cap Bonus","SLAB","SLGAB","SLGB","Team Year Bonus",
                  "Top Score Bonus","Unused Sub Fee","WaCLG","Injury Rls",
                  "Min Fee Rls","Min Fee Rls Clubs In Cont Comp",
                  "Min Fee Rls Clubs Mjr Cont Comp","Min Fee Rls to Higher Div",
                  "Min Fee Rls to Domestic Clubs","Min Fee Rls to Foreign Clubs",
                  "Non Prom Rls Cls","Non-Playing Rel","Relegation Release",
                  "Wage","Wage After Tax","Wage Contrib.","New Wage","Max AP",
                  "Max WD","Min AP","Min WD","Asking Price","Fee",
                  "Last Trans. Fee","Ovr","Transfer Fees Received","Value")
  
  format_money <- function(x) {
    x %>%
      # remove currency symbols and separators
      str_remove_all("\u20AC|\u00A3|\u0024") %>% str_remove_all(",") %>%
      # remove wage frequency symbols
      str_remove_all("p/w") %>% str_remove_all("p/a") %>% str_remove_all("p/m") %>%
      # replace decimal prefix units by their values
      str_replace_all(" ", " 1") %>% str_replace_all("K", " 1000") %>%
      str_replace_all("M", " 1000000") %>%
      # split string and multiply
      str_split(" ") %>% lapply(as.numeric) %>% lapply(prod) %>% unlist()
  }
  
  data %>%
    # Replace "not available" data by actual NAs
    replace(. == "-", NA) %>%
    replace(. == "Scouting Required", NA) %>%
    replace(. == "N/A", NA) %>%
    replace(. == "Unknown", NA) %>%
    replace(. == "-", NA) %>%
    # Name: Removes the nationality in 'shortlist', doesn't affect 'squad'
    mutate(`Name` = `Name`  %>% str_split("-") %>% sapply("[[", 1) %>% trimws()) %>%
    # Rec; scouting recommandation
    mutate_at(vars(one_of("Rec")), function(x) {
      str_remove_all(x, "-") %>% trimws() %>% as.numeric() %>% replace_na(-1)}) %>%
    # position: add the parsed position
    mutate(
      `PositionParsed` = sapply(`Position`, pos_parse)%>% unname(),
      .after = `Position`
    ) %>%
    # Attributes: mean of upper and lower bound if no exact values known
    mutate_at(attributes, function(x) {
      str_split(x, "-") %>% lapply(as.numeric) %>% lapply(mean) %>% unlist()}) %>%
    # Remove the league from the based nation; league is available in separate datapoint
    mutate(`Based` = str_split(`Based`, "\\(") %>% sapply("[[", 1) %>% trimws()) %>%
    # Stats: remove units and divide ratio type values by 100
    mutate(`Hdr %` = as.numeric(str_remove_all(`Hdr %`, "%"))/100) %>%
    mutate(`Cr C/A` = as.numeric(str_remove_all(`Cr C/A`, "%"))/100) %>%
    mutate(`Distance` = str_remove_all(`Distance`, "km")) %>%
    mutate(`Dist/90` = str_remove_all(`Dist/90`, "km")) %>%
    mutate(`Pas %` = as.numeric(str_remove_all(`Pas %`, "%"))/100) %>%
    mutate(`Shot %` = as.numeric(str_remove_all(`Shot %`, "%"))/100) %>%
    mutate(`Tck R` = as.numeric(str_remove_all(`Tck R`, "%"))/100) %>%
    mutate(`Apps` = str_remove_all(`Apps`,"[//(//)]") %>% str_split(" ") %>%
             lapply(as.numeric) %>% lapply(sum) %>% unlist()) %>% # total appearances
    mutate(`Gwin` = as.numeric(str_remove_all(`Gwin`, "%"))/100) %>%
    mutate(`Pens Saved Ratio` = as.numeric(str_remove_all(`Pens Saved Ratio`, "%"))/100) %>%
    mutate(`Pen/R` = as.numeric(str_remove_all(`Pen/R`, "%"))/100) %>%
    mutate(`Int Starts` = str_remove_all(`Int Apps`,"[//(//)]") %>% str_split(" ") %>%
             lapply(as.numeric) %>% sapply("[[",1), .after = `Int Apps`) %>% # create international starts
    mutate(`Int Apps` = str_remove_all(`Int Apps`,"[//(//)]") %>% str_split(" ") %>%
             lapply(as.numeric) %>% lapply(sum) %>% unlist()) %>% # total int appearances
    # Stats as numeric values
    mutate_at(c(statschalkboard,statsgeneral),
              function(x) {x %>% str_remove_all(",") %>% as.numeric}) %>%
    # Format financial values as numeric
    mutate_at(financials, format_money) %>%
    # Value and Wage: 0 if NA
    mutate(`Value` = replace_na(`Value`, 0)) %>%
    mutate(`Wage` = replace_na(`Wage`, 0)) %>%
    # Club: free player/amateur if NA
    mutate(`Club` = replace_na(`Club`, "Free player")) %>%
    # Division: free player/unknown if NA
    mutate(`Division` = replace_na(`Division`, "Free player/unknown"))
}

# assigns a level to the scout recommendation
# used to determine the marker color in the visualizations
# and the less saturated color in the reactables, depending on the variable 'plot'
colorcatRec <- function(rec, plot = TRUE) {
  case_when(
    rec == -1 ~ if_else(plot == TRUE, "-1", "#bf3eff"), # darkorchid1
    between(rec, 0, 49) ~ if_else(plot == TRUE, "1", "#ff0000"), # red
    between(rec, 50, 59) ~ if_else(plot == TRUE, "2", "#ffa500"), # orange
    between(rec, 60, 69) ~ if_else(plot == TRUE, "3", "#ffff00"), # yellow
    between(rec, 70, 84) ~ if_else(plot == TRUE, "4", "#00ee00"), # green2
    between(rec, 85, 100) ~ if_else(plot == TRUE, "5", "#008b00") # green4
  )
}

# assigns a numeric value to Minutes/90
# used to determine the marker size in the visualizations
sizemapMinsp90 <- function(minsp90) {
  case_when(
    minsp90 <= 5 ~ 1.5,
    minsp90 > 5 & minsp90 <= 10 ~ 2,
    minsp90 > 10 & minsp90 <= 20 ~ 2.5,
    minsp90 > 20 & minsp90 <= 30 ~ 3,
    minsp90 > 30 ~ 3.5
  )
}

# generates a plotly scatter plot
plot_it <- function(data, x, y, xlabel, ylabel, targetratio) {
  plot_ly(
    # add a column of "1"s to avoid the duplicate legends for the ratio trace
    data = data %>% mutate(`ratiotracecolor` = "1"), x = ~x, y = ~y,
    type = "scatter", mode = "markers",
    # adds text when marker is hovered on
    hoverinfo = "text",
    # define text logic: name, age, position, mins, ratio and marker value
    text = ~paste0(
      "<b>Name: ", Name,
      "<br>Age: </b>", Age,
      "<br><b>Club: </b>", Club,
      "<br><b>Nation: </b>", Nat1,
      "<br><b>Position: </b>", Position,
      "<br><b>Games played: </b>", Mins,
      "<br><b>", xlabel, ": </b>", round(x, digits = 1),
      "<br><b>", ylabel, ": </b>", round(y, digits = 1),
      # add the targetratio*0 to transform the entire expression to NA is targetration = 0
      "<br><b>Ratio: </b>", formatPct(targetratio*0 + y/x)
    ),
    # size determined by game played/mins per 90
    # mapped with sizemapMinsp90()
    size = ~sizemapMinsp90(Mins), fill = "",
    # color determined by scout recommendation value
    color = ~colorcatRec(Rec),
    # color mapping
    colors = c(
      "-1" = "darkviolet",
      "1" = "red",
      "2" = "orange",
      "3" = "yellow",
      "4" = "green2",
      "5" = "green4"
    ),
    # outline color
    strokes = "black",
    # hide legend for the markers
    showlegend = FALSE
  ) %>%
    # add name on top of the marker
    add_text(
      text = ~Name, textposition = "top right", textfont = list(
        color = "black", size = 11
      )
    ) %>%
    # add a trend line. if targetratio = NA it won't show on the plot
    add_trace(
      y = ~targetratio*x,
      mode = "lines", line = list(width = 1, dash = "dash", color = "grey"),
      name = paste0(round(targetratio*100,digits = 0), " % ratio"),
      hoverinfo = "none", showlegend = TRUE, color = ~ratiotracecolor
    ) %>%
    # add axis labels, no title
    layout(
      xaxis = list(title = xlabel), yaxis = list(title = ylabel),
      showlegend = TRUE
    )
}

# checks if any substring from a vector appears in a string
grepl_op <- function(pattern_array, x) {
  check <- 0
  for (i in 1:length(pattern_array)) {
    check <- check + grepl(pattern_array[i], x, fixed = TRUE)
  }
  as.logical(check)
}

# style item to freeze a left row in a reactable table
sticky_style <- function(left = TRUE) {
  style <- list(position = "sticky", background = "#fff", zIndex = 1)
  if (left) {
    style <- c(style, list(left = 0, borderRight = "1px solid #eee"))
  } else {
    style <- c(style, list(right = 0, borderLeft = "1px solid #eee"))
  }
  style
}

tabulate_it <- function(data, dataset) {
  dataset <- if_else(dataset == "squad", "DNA Rating", "Scouting Rec")
  reactable(
    data,
    defaultSortOrder = "desc",
    columns = list(
      Name = colDef(style = sticky_style(), headerStyle = sticky_style()),
      Rec = colDef(style = function(value) {
        color <- colorcatRec(value, FALSE)
        list(background = color)
      }, name = dataset),
      Value = colDef(format = colFormat(
        digits = 0, separators = TRUE, prefix = "\u20ac "
      )),
      Wage = colDef(format = colFormat(
        digits = 0, separators = TRUE, prefix = "\u20ac "
      ))
    ),
    defaultSorted = c("GamesPlayed", "Value", "Wage", "Rec", "Age"),
    highlight = TRUE,
    pagination = FALSE, height = 500
  )
}

# creates a subset of the data using and index
# Name, Rec, Age, Position, Mins always part of the data: used in the plot
# {...} represents columns that are selecting depending of the submenu
subsetdata <- function(data, reactindex, ...) {
  as.data.frame(data) %>%
    dplyr::filter(Name %in% reactindex) %>%
    dplyr::select(
      Name,
      Rec,
      Age,
      Position,
      Mins,
      Nat1,
      Club,
      ...
    ) %>%
    dplyr::mutate(
      Mins = (Mins/90) %>% round(digits = 2)
    )
}

# Data Import -------------------------------------------------------------

# import squad data and measure execution time
cat("\nImporting squad data...")
tic("\nImported squad data")
squad <- parse_FMoutput("data/squad.html")
toc()
# import shortlist data and measure execution time
cat("\nImporting shortlist data...")
tic("\nImported shortlist data")
shortlist <- parse_FMoutput("data/shortlist.html")
toc()

# Data cleaning & formatting ----------------------------------------------

# clean & format squad data and measure execution time
cat("\nCleaning & formatting squad data...")
tic("\nCleaned & formatted squad data")
squad %<>% mutate(Rec = NA, .after = Name) %>% cleandata()
toc()
# clean & format shortlist data and measure execution time
cat("\nCleaning & formatting shortlist data...")
tic("\nCleaned & formatted shortlist data")
shortlist %<>% cleandata()
toc()

# App inputs --------------------------------------------------------------

# status message
cat("\nLoading app data...")
tic("\nLoaded app data")

# unique clubs appearing in the squad
# i.e. the managed clubs + clubs with players on loan
# managed club first on the list
sqclubs <- squad$Club %>%
  # get frequencies of Club in the list
  cbind(freq = ave(squad$Club, squad$Club, FUN = length)) %>%
  unique() %>%
  as.data.frame() %>%
  # sort by desc order using the frequencies: managed club in first position
  dplyr::arrange(desc(freq)) %>%
  # pull list of clubs as vector
  dplyr::pull(1)
# unique divisions appearing in the shortlist (put free player at the end)
divisions <- shortlist$Division %>% unique() %>% sort() %>%
  .[. != "Free player/unknown"] %>% c("Free player/unknown")
# unique FM positions
positions <- c("GK", # goalkeepers
               "D(L)", "D(C)", "D(R)", # defenders
               "WB(L)", "WB(R)", # wingbacks
               "DM", # defensive midfielders
               "M(L)", "M(C)", "M(R)", # midfielders
               "AM(L)", "AM(C)", "AM(R)", # attacking midfielders
               "ST(C)" # strikers
               )


# App ui: header ----------------------------------------------------------

Header <- dashboardHeader(
  title = "FM Data Analytics"
)

# App ui: sidebar ---------------------------------------------------------

Sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Squad Analysis", tabName = "squad", icon = icon("users"),
      startExpanded = TRUE,
      menuSubItem(
        HTML("DNA Model"), tabName = "sqmodel", icon = icon("sliders-h")
      ),
      menuSubItem(
        "Filters", tabName = "sqfilter", icon = icon("filter"),
        selected = TRUE
      ),
      menuSubItem(
        "General", tabName = "sqgen"
      ),
      menuSubItem(
        "Passing", tabName = "sqpass"
      ),
      menuSubItem(
        "Shooting", tabName = "sqshoot"
      ),
      menuSubItem(
        "Movement", tabName = "sqmove"
      ),
      menuSubItem(
        "Aerial Challenges", tabName = "sqaerial"
      ),
      menuSubItem(
        "Tackling & Intercepting", tabName = "sqtackle"
      )
    ),
    
    menuItem(
      "Scouting", tabName = "scouting", icon = icon("search"),
      startExpanded = FALSE,
      menuSubItem(
        "Filters", tabName = "scfilter", icon = icon("filter")
      ),
      menuSubItem(
        "General", tabName = "scgen"
      ),
      menuSubItem(
        "Passing", tabName = "scpass"
      ),
      menuSubItem(
        "Shooting", tabName = "scshoot"
      ),
      menuSubItem(
        "Movement", tabName = "scmove"
      ),
      menuSubItem(
        "Aerial Challenges", tabName = "scaerial"
      ),
      menuSubItem(
        "Tackling & Intercepting", tabName = "sctackle"
      ),
      menuSubItem(
        "Shortlist", tabName = "sclist", icon = icon("list")
      )
    ),
    
    menuItem(
      "About/more info", tabName = "aboutapp", icon = icon("info-circle")
    )
  )
)

# App ui: body ------------------------------------------------------------

Body <- dashboardBody(
  tabItems(
    #### Squad: DNA Model ####
    tabItem(
      tabName = "sqmodel",
      box(
        title = "Attributes Selection",
        width = 12, height = "85vh",
        background = "purple",
        HTML(
          "
            <p>Constitute your DNA model by selecting key attributes for your playing style. The selected attributes will be used to calculate your players DNA rating according to the model.</p>
            "
        ),
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          # technical attributes
          checkboxGroupInput(
            inputId = "sqmodel_technical",
            label = "Technical",
            choices = c(
              "Corners" = "Cor",
              "Crossing" = "Cro",
              "Dribbling" = "Dri",
              "Finishing" = "Fin",
              "First Touch" = "Fir",
              "Free Kicks" = "Fre",
              "Heading" = "Hea",
              "Long Shots" = "Lon",
              "Long Throws" = "L Th",
              "Marking" = "Mar",
              "Passing" = "Pas",
              "Penalty Taking" = "Pen",
              "Tackling" = "Tck",
              "Technique" = "Tec"
            ),
            selected = NULL
          ),
          # mental attributes
          checkboxGroupInput(
            inputId = "sqmodel_mental",
            label = "Mental",
            choices = c(
              "Aggression" = "Agg",
              "Anticipation" = "Ant",
              "Bravery" = "Bra",
              "Composure" = "Cmp",
              "Concentration" = "Cnt",
              "Decisions" = "Dec",
              "Determination" = "Det",
              "Flair" = "Fla",
              "Leadership" = "Ldr",
              "Off The Ball" = "OtB",
              "Positioning" = "Pos",
              "Teamwork" = "Tea",
              "Vision" = "Vis",
              "Work Rate" = "Wor"
            ),
            selected = NULL
          ),
          # physical attributes
          checkboxGroupInput(
            inputId = "sqmodel_physical",
            label = "Physical",
            choices = c(
              "Acceleration" = "Acc",
              "Agility" = "Agi",
              "Balance" = "Bal",
              "Jumping Reach" = "Jum",
              "Natural Fitnes" = "Nat",
              "Pace" = "Pac",
              "Stamina" = "Sta",
              "Strength" = "Str"
            ),
            selected = NULL
          ),
          # goalkeeping attributes
          checkboxGroupInput(
            inputId = "sqmodel_gk",
            label = "Goalkeeping",
            choices = c(
              "Aerial Reach" = "Aer",
              "Command Of Area" = "Cmd",
              "Communication" = "Com",
              "Eccentricity" = "Ecc",
              "Handling" = "Han",
              "Kicking" = "Kic",
              "One On Ones" = "1v1",
              "Punching (Tendency)" = "Pun",
              "Reflexes" = "Ref",
              "Rushing Out (Tendency)" = "TRO",
              "Throwing" = "Thr"
            ),
            selected = NULL
          )
        ),
        # deselect buttons for each of the 4 attributes categories
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          actionButton(
            inputId = "sqmodel_deselect_tec", label = " Deselect all technical",
            width = '100%', style = 'margin-bottom:0.5px'
          ),
          actionButton(
            inputId = "sqmodel_deselect_men", label = " Deselect all mental",
            width = '100%', style = 'margin-bottom:0.5px'
          ),
          actionButton(
            inputId = "sqmodel_deselect_phy", label = " Deselect all physical",
            width = '100%', style = 'margin-bottom:0.5px'
          ),
          actionButton(
            inputId = "sqmodel_deselect_gk", label = " Deselect all goalkeeping",
            width = '100%', style = 'margin-bottom:0.5px'
          )
        ),
        # Remarks about the DNA rating
        HTML(
          "
          <br><p><em>* The rating is calculated as follows: Average(key attributes)/20 rounded to the nearest integer. For instance 15(or 12 or 8) on all key attributes yield a DNA rating of 75(or 60 or 40).</em></p>
          <p><em>** Outfield players rating do not use Goalkeeping attributes. Goalkeepers ratings use the Goalkeeping attributes instead of the Technical attributes. No attributes selected yield a DNA rating of -1 (same as scouting recommandation for non-scouted players).</em></p>
          "
        )
      )
    ),
    
    #### Squad: Filters submenu ####
    tabItem(
      tabName = "sqfilter",
      fluidRow(
        # filters box
        box(
          title = "Filters",
          width = 3, height = "85vh",
          background = "purple",
          # division filter
          pickerInput(
            inputId = "squad_club", label = "Club(s):",
            choices = sqclubs, selected = sqclubs, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 7,
              `selected-text-format` = "count > 0"
            )
          ),
          # position filter
          pickerInput(
            inputId = "squad_pos", label = "Position(s):",
            choices = positions, selected = positions, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 7,
              `selected-text-format` = "count > 0"
            )
          ),
          # age filter
          sliderInput(
            inputId = "squad_age", label = "Age:",
            min = 16, max = 45, value = c(16,45), step = 1
          ),
          # # maximal value and wage filters
          # splitLayout(
          #   cellWidths = c('50%','50%'),
          #   autonumericInput(
          #     inputId = "scout_maxvalue", label = "Max value:",
          #     value = 50000000, maximumValue = 500000000, minimumValue = 0,
          #     align = "right",
          #     currencySymbol = "\u20ac ", currencySymbolPlacement = "p",
          #     decimalCharacter = ".", digitGroupSeparator = ",",
          #     decimalPlaces = 0
          #   ),
          #   autonumericInput(
          #     inputId = "scout_maxwage", label = "Max wage (p/w):",
          #     value = 500000, maximumValue = 5000000, minimumValue = 0,
          #     align = "right",
          #     currencySymbol = "\u20ac ", currencySymbolPlacement = "p",
          #     decimalCharacter = ".", digitGroupSeparator = ",",
          #     decimalPlaces = 0
          #   )
          # ),
          # minimal total appearances filter
          sliderInput(
            inputId = "squad_minapps", label = "Min total appearances:",
            min = 0, max = 30, value = 0, step = 1
          )
        ),
        # filtered subset overview
        box(
          title = "Filtered subset overview",
          width = 9, height = '85vh',
          status = "primary", solidHeader = FALSE,
          reactableOutput(
            outputId = "squad_filtered",
            height = '100%', width = '100%'
          )
        )
      )
    ),
    
    #### Squad: General submenu ####
    tabItem(
      tabName = "sqgen",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "sqgen_rating",
            label = "1 - Rating Consistency",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqgen_pointsw",
            label = "2 - Team Results Consistency",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqgen_teamggoalass",
            label = "3 - Goals + Assists Contribution",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqgen_goalass",
            label = "4 - Goals and Assists",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqgen_cards",
            label = "5 - Cards",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "sqgen_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "sqgen_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "sqgen_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "sqgen_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "sqgen_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "sqgen_plot5")
          )
        )
      )
    ),
    
    #### Squad: Passing submenu ####
    tabItem(
      tabName = "sqpass",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "sqpass_attcomp",
            label = "1 - Attempted vs Completed Passes",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqpass_compkey",
            label = "2 - Completed vs Key Passes",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqpass_compchan",
            label = "3 - Completed Passes vs Chances Created",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqpass_compass",
            label = "4 - Completed Passes vs Assists",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqpass_teamgass",
            label = "5 - Assists Contribution",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqpass_crossattcomp",
            label = "6 - Attempted vs Completed Crosses",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "sqpass_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "sqpass_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "sqpass_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "sqpass_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "sqpass_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "sqpass_plot5")
          ),
          tabPanel(
            title = "6", value = "panel6",
            plotlyOutput(outputId = "sqpass_plot6")
          )
        )
      )
    ),
    
    #### Squad: Shooting submenu ####
    tabItem(
      tabName = "sqshoot",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "sqshoot_atttarg",
            label = "1 - Attempted vs On Target Shots",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqshoot_targgoal",
            label = "2 - On Target Shots vs Goals",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqshoot_teamggoal",
            label = "3 - Goals Contribution",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqshoot_attxg",
            label = "4 - Attempted Shots vs xG",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqshoot_xggoal",
            label = "5 - xG vs Goals",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "sqshoot_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "sqshoot_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "sqshoot_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "sqshoot_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "sqshoot_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "sqshoot_plot5")
          )
        )
      )
    ),
    
    #### Squad: Movement submenu ####
    tabItem(
      tabName = "sqmove",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "sqmove_drib",
            label = "1 - Dribbling",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqmove_offs",
            label = "2 - Offsides",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqmove_dist",
            label = "3 - Distance",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "sqmove_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "sqmove_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "sqmove_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "sqmove_plot3")
          )
        )
      )
    ),
    
    #### Squad: Aerial submenu ####
    tabItem(
      tabName = "sqaerial",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "sqaerial_attwon",
            label = "1 - Attempted vs Won Headers",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqaerial_wonkey",
            label = "2 - Won vs Key Headers",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "sqaerial_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "sqaerial_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "sqaerial_plot2")
          )
        )
      )
    ),
    
    #### Squad: Tackling submenu ####
    tabItem(
      tabName = "sqtackle",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "sqtackle_attcomp",
            label = "1 - Attempted vs Completed Tackles",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqtackle_compkey",
            label = "2 - Completed vs Key Tackles",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqtackle_attfoul",
            label = "3 - Attempted Tackles vs Fouls",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqtackle_attcard",
            label = "4 - Attempted Tackles vs Yellow/Red Cards",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqtackle_int",
            label = "5 - Interceptions",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sqtackle_mist",
            label = "6 - Mistakes Contribution",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "sqtackle_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "sqtackle_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "sqtackle_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "sqtackle_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "sqtackle_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "sqtackle_plot5")
          ),
          tabPanel(
            title = "6", value = "panel6",
            plotlyOutput(outputId = "sqtackle_plot6")
          )
        )
      )
    ),
    
    #### Scouting: Filters submenu ####
    tabItem(
      tabName = "scfilter",
      fluidRow(
        # filters box
        box(
          title = "Filters",
          width = 3, height = "85vh",
          background = "purple",
          # division filter
          pickerInput(
            inputId = "scout_div", label = "Division(s):",
            choices = divisions, selected = divisions, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 7,
              `selected-text-format` = "count > 0"
            )
          ),
          # position filter
          pickerInput(
            inputId = "scout_pos", label = "Position(s):",
            choices = positions, selected = positions, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 7,
              `selected-text-format` = "count > 0"
            )
          ),
          # age filter
          sliderInput(
            inputId = "scout_age", label = "Age:",
            min = 16, max = 45, value = c(16,45), step = 1
          ),
          # maximal value and wage filters
          splitLayout(
            cellWidths = c('50%','50%'),
            autonumericInput(
              inputId = "scout_maxvalue", label = "Max value:",
              value = 50000000, maximumValue = 500000000, minimumValue = 0,
              align = "right",
              currencySymbol = "\u20ac ", currencySymbolPlacement = "p",
              decimalCharacter = ".", digitGroupSeparator = ",",
              decimalPlaces = 0
            ),
            autonumericInput(
              inputId = "scout_maxwage", label = "Max wage (p/w):",
              value = 500000, maximumValue = 5000000, minimumValue = 0,
              align = "right",
              currencySymbol = "\u20ac ", currencySymbolPlacement = "p",
              decimalCharacter = ".", digitGroupSeparator = ",",
              decimalPlaces = 0
            )
          ),
          # minimal total appearances filter
          sliderInput(
            inputId = "scout_minapps", label = "Min total appearances:",
            min = 0, max = 30, value = 0, step = 1
          )
        ),
        # filtered subset overview
        box(
          title = "Filtered subset overview",
          width = 9, height = '85vh',
          status = "primary", solidHeader = FALSE,
          reactableOutput(
            outputId = "scout_filtered",
            height = '100%', width = '100%'
          )
        )
      )
    ),
    
    #### Scouting: General submenu ####
    tabItem(
      tabName = "scgen",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "scgen_rating",
            label = "1 - Rating Consistency",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scgen_pointsw",
            label = "2 - Team Results Consistency",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scgen_teamggoalass",
            label = "3 - Goals + Assists Contribution",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scgen_goalass",
            label = "4 - Goals and Assists",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scgen_cards",
            label = "5 - Cards",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "scgen_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "scgen_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "scgen_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "scgen_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "scgen_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "scgen_plot5")
          )
        )
      ),
      fluidRow(
        # shorlist builder
        box(
          title = "Shortlist builder",
          width = 12, height = '15vh',
          background = "purple",
          pickerInput(
            inputId = "scgen_shortl", label = NULL,
            choices = shortlist$Name, selected = shortlist$Name, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 10,
              `selected-text-format` = "count > 0"
            )
          )
        )
      )
    ),
    
    #### Scouting: Passing submenu ####
    tabItem(
      tabName = "scpass",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "scpass_attcomp",
            label = "1 - Attempted vs Completed Passes",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scpass_compkey",
            label = "2 - Completed vs Key Passes",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scpass_compchan",
            label = "3 - Completed Passes vs Chances Created",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scpass_compass",
            label = "4 - Completed Passes vs Assists",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scpass_teamgass",
            label = "5 - Assists Contribution",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scpass_crossattcomp",
            label = "6 - Attempted vs Completed Crosses",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "scpass_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "scpass_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "scpass_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "scpass_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "scpass_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "scpass_plot5")
          ),
          tabPanel(
            title = "6", value = "panel6",
            plotlyOutput(outputId = "scpass_plot6")
          )
        )
      ),
      fluidRow(
        # shorlist builder
        box(
          title = "Shortlist builder",
          width = 12, height = '15vh',
          background = "purple",
          pickerInput(
            inputId = "scpass_shortl", label = NULL,
            choices = shortlist$Name, selected = shortlist$Name, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 10,
              `selected-text-format` = "count > 0"
            )
          )
        )
      )
    ),
    
    #### Scouting: Shooting submenu ####
    tabItem(
      tabName = "scshoot",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "scshoot_atttarg",
            label = "1 - Attempted vs On Target Shots",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scshoot_targgoal",
            label = "2 - On Target Shots vs Goals",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scshoot_teamggoal",
            label = "3 - Goals Contribution",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scshoot_attxg",
            label = "4 - Attempted Shots vs xG",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scshoot_xggoal",
            label = "5 - xG vs Goals",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "scshoot_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "scshoot_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "scshoot_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "scshoot_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "scshoot_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "scshoot_plot5")
          )
        )
      ),
      fluidRow(
        # shorlist builder
        box(
          title = "Shortlist builder",
          width = 12, height = '15vh',
          background = "purple",
          pickerInput(
            inputId = "scshoot_shortl", label = NULL,
            choices = shortlist$Name, selected = shortlist$Name, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 10,
              `selected-text-format` = "count > 0"
            )
          )
        )
      )
    ),
    
    #### Scouting: Movement submenu ####
    tabItem(
      tabName = "scmove",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "scmove_drib",
            label = "1 - Dribbling",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scmove_offs",
            label = "2 - Offsides",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scmove_dist",
            label = "3 - Distance",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "scmove_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "scmove_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "scmove_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "scmove_plot3")
          )
        )
      ),
      fluidRow(
        # shorlist builder
        box(
          title = "Shortlist builder",
          width = 12, height = '15vh',
          background = "purple",
          pickerInput(
            inputId = "scmove_shortl", label = NULL,
            choices = shortlist$Name, selected = shortlist$Name, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 10,
              `selected-text-format` = "count > 0"
            )
          )
        )
      )
    ),
    
    #### Scouting: Aerial submenu ####
    tabItem(
      tabName = "scaerial",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "scaerial_attwon",
            label = "1 - Attempted vs Won Headers",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "scaerial_wonkey",
            label = "2 - Won vs Key Headers",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "scaerial_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "scaerial_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "scaerial_plot2")
          )
        )
      ),
      fluidRow(
        # shorlist builder
        box(
          title = "Shortlist builder",
          width = 12, height = '15vh',
          background = "purple",
          pickerInput(
            inputId = "scaerial_shortl", label = NULL,
            choices = shortlist$Name, selected = shortlist$Name, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 10,
              `selected-text-format` = "count > 0"
            )
          )
        )
      )
    ),
    
    #### Scouting: Tackling submenu ####
    tabItem(
      tabName = "sctackle",
      fluidRow(
        # visualization selection box
        box(
          title = "Visualization Selection",
          width = 3, height = '70vh',
          background = "purple",
          actionButton(
            inputId = "sctackle_attcomp",
            label = "1 - Attempted vs Completed Tackles",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sctackle_compkey",
            label = "2 - Completed vs Key Tackles",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sctackle_attfoul",
            label = "3 - Attempted Tackles vs Fouls",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sctackle_attcard",
            label = "4 - Attempted Tackles vs Yellow/Red Cards",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sctackle_int",
            label = "5 - Interceptions",
            width = '100%', style = 'margin-bottom:10px'
          ),
          actionButton(
            inputId = "sctackle_mist",
            label = "6 - Mistakes Contribution",
            width = '100%', style = 'margin-bottom:10px'
          )
        ),
        # visualizations box
        tabBox(
          id = "sctackle_tabbox", title = "Visualizations",
          width = 9, height = '70vh',
          tabPanel(
            title = "1", value = "panel1",
            plotlyOutput(outputId = "sctackle_plot1")
          ),
          tabPanel(
            title = "2", value = "panel2",
            plotlyOutput(outputId = "sctackle_plot2")
          ),
          tabPanel(
            title = "3", value = "panel3",
            plotlyOutput(outputId = "sctackle_plot3")
          ),
          tabPanel(
            title = "4", value = "panel4",
            plotlyOutput(outputId = "sctackle_plot4")
          ),
          tabPanel(
            title = "5", value = "panel5",
            plotlyOutput(outputId = "sctackle_plot5")
          ),
          tabPanel(
            title = "6", value = "panel6",
            plotlyOutput(outputId = "sctackle_plot6")
          )
        )
      ),
      fluidRow(
        # shorlist builder
        box(
          title = "Shortlist builder",
          width = 12, height = '15vh',
          background = "purple",
          pickerInput(
            inputId = "sctackle_shortl", label = NULL,
            choices = shortlist$Name, selected = shortlist$Name, multiple = TRUE,
            width = '100%',
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `size` = 10,
              `selected-text-format` = "count > 0"
            )
          )
        )
      )
    ),
    
    #### Scouting: Shortlist ####
    tabItem(
      tabName = "sclist",
      box(
        title = "Shortlist output",
        width = 12, height = '85vh',
        status = "primary",
        reactableOutput(
          outputId = "scout_listfinal",
          height = '100%', width = '100%'
        )
      )
    ),
    
    #### About/more info tab ####
    tabItem(
      tabName = "aboutapp",
      fluidRow(
        box(
          title = "About",
          width = 6, height = '85vh',
          status = "primary",
          HTML(
            "
            <p>Designed to mirror professional football club's performance analysis tools, this app enables FM managers to use in-game statistics at their full potential. Whether it be to decide which players you&apos;ll target from the hundreds on your shortlist, or to identify sub-performing or over-performing players within your own squad, you will have the possibility to use the app&apos;s interactive visualizations to support your decisions.</p>
            <p>However, don&apos;t forget &mdash; statistics only tell one part of the story!</p>
            <p><br></p>
            <p><strong>Developed by:</strong>
            <br>gam945 (<em>SI forums</em>)/ybenadjal (<em>GitHub</em>)</p>
            <p><strong>Contributors:</strong>
            <br>biglew90(<em>SI Forums</em>)/LBrookes90 (<em>GitHub</em>)</p>
            <p><br></p>
            <p>PM me on the SI forums to contribute ;)</p>
            "
          )
        ),
        box(
          title = "More info",
          width = 6, height = '85vh',
          status = "primary",
          HTML(
            '
            <p>Github link:</p>
            <p><a href="https://github.com/ybenadjal/FM-Data-Analytics"><em>Github</em></a></p>
            <p><br></p>
            <p>For updates, guides, known issues or to report a bug:</p>
            <p><a href="https://community.sigames.com/forums/topic/545140-a-tool-based-on-r-shiny-for-the-fm-data-driven-manager/"><em>SI Forums thread</em></a></p>
            '
          )
        )
      )
    )
    
  )
)

# App ui ------------------------------------------------------------------

ui <- dashboardPage(Header, Sidebar, Body, skin = "purple")

# App server --------------------------------------------------------------

server <- function(input, output, session) {
  
  #### Squad: DNA Model ####
  
  # deselect all attributes from a category when the "Deselect all <category>" button is clicked on
  observeEvent(
    input$sqmodel_deselect_tec, {
      updateCheckboxGroupInput(
        session = session, inputId = "sqmodel_technical", selected = character(0)
      )
    }
  )
  observeEvent(
    input$sqmodel_deselect_men, {
      updateCheckboxGroupInput(
        session = session, inputId = "sqmodel_mental", selected = character(0)
      )
    }
  )
  observeEvent(
    input$sqmodel_deselect_phy, {
      updateCheckboxGroupInput(
        session = session, inputId = "sqmodel_physical", selected = character(0)
      )
    }
  )
  observeEvent(
    input$sqmodel_deselect_gk, {
      updateCheckboxGroupInput(
        session = session, inputId = "sqmodel_gk", selected = character(0)
      )
    }
  )
  
  # calculate ratings
  squad_DNArated <- reactive({
    if (length(c(input$sqmodel_technical, input$sqmodel_mental, input$sqmodel_physical, input$sqmodel_gk)) == 0) {
      squad %>% mutate(`Rec` = -1)
    } else {
      # key outfield attributes
      outf_keyattr <- c(input$sqmodel_technical, input$sqmodel_mental, input$sqmodel_physical)
      outf_n <- length(outf_keyattr)
      # key gk attributes
      gk_keyattr <- c(input$sqmodel_mental, input$sqmodel_physical, input$sqmodel_gk)
      gk_n <- length(gk_keyattr)
      # subset data: outfield players
      outf_players <- squad %>%
        dplyr::select(Name, Position, all_of(outf_keyattr)) %>%
        dplyr::filter(Position != "GK")
      # subset data: goalkeepers
      gk_players <- squad %>%
        dplyr::select(Name, Position, all_of(gk_keyattr)) %>%
        dplyr::filter(Position == "GK")
      # get ratings for outfield players
      outf_players %<>% cbind(Rec = dplyr::select(outf_players, -c(Name, Position)) %>% rowSums()) %>%
        dplyr::select(Name, Position,Rec) %>%
        mutate(Rec = round(Rec*100/20/outf_n, digits = 0))
      # get ratings for outfield players
      gk_players %<>% cbind(Rec = dplyr::select(gk_players, -c(Name, Position)) %>% rowSums()) %>%
        dplyr::select(Name, Position,Rec) %>%
        mutate(Rec = round(Rec*100/20/gk_n, digits = 0))
      # ratings
      ratings <- rbind(outf_players, gk_players)
      # inner join by name and position
      squad %>% dplyr::select(-Rec) %>%
        inner_join(y = ratings, by = c("Name" = "Name", "Position" = "Position"))
    }
  })
  
  #### Squad: Filters ####
  
  # filtered subset data for reactable presentation
  squad_filtsubset <- reactive({
    squad_DNArated() %>% as.data.frame() %>%
      select(Name, Rec, Position, PositionParsed, Division, Club, Age, Value, Wage, Mins) %>%
      # filter using the filters inputs
      dplyr::filter(Club %in% input$squad_club) %>%
      dplyr::filter(
        grepl_op(pattern_array = input$squad_pos, x = PositionParsed)
      ) %>%
      dplyr::filter(between(Age, input$squad_age[1], input$squad_age[2])) %>%
      # dplyr::filter(Value <= input$scout_maxvalue) %>%
      # dplyr::filter(Wage <= input$scout_maxwage) %>%
      dplyr::filter(Mins >= input$squad_minapps*90) %>%
      dplyr::select(-c(Division, PositionParsed)) %>%
      mutate(
        GamesPlayed = round(Mins/90, digits = 1), .keep = "unused", .after = Mins
      ) %>%
      drop_na()
  })
  
  # filtered subset reactable
  output$squad_filtered <- renderReactable({
    tabulate_it(data = squad_filtsubset(), dataset = "squad")
  })
  
  # filtered subset names: used as an index
  squad_index <- reactive({
    squad_filtsubset() %>% as.data.frame() %>% dplyr::select(Name)# %>% as.vector()
  })
  
  # filtered subset reactable for plots
  squad_data <- reactive({
    subsetdata(
      data = squad_DNArated() %>% as.data.frame(),
      reactindex = as.vector(as.data.frame(squad_index())$Name),
      `Av Rat`, `Pts/Gm`, `Clean sheets`, # general
      `Ps A/90`, `Ps C/90`, `K Ps/90`, `Ch C/90`, `Asts/90`, `Cr A`, `Cr C`, # passing
      `Shot/90`, `ShT/90`, `Gls/90`, `Tgls/90`, `xG`, # shooting
      `DrbPG`, `Dist/90`, `Off`, # movement
      `Aer A/90`, `Hdrs W/90`, `K Hdrs`, # aerial
      `Tck R`, `Tck W`, `K Tck`, `Int/90`, `Fls`, `Red`, `Yel`, # tackling 1
      `Tcon/90`, `Gl Mst`#, # tackling 2
    ) %>%
      mutate(`Gls plus Asts` = `Gls/90` + `Asts/90`) %>%
      mutate(`Cr A` = `Cr A`/Mins) %>%
      mutate(`Cr C` = `Cr C`/Mins) %>%
      mutate(`xG` = `xG`/Mins) %>%
      mutate(`Off` = `Off`/Mins) %>%
      mutate(`K Hdrs` = `K Hdrs`/Mins) %>%
      # for some reason no attempted tackles valid data so using ratio and won to calculate it
      mutate(`Tck A` = (`Tck W`/`Tck R`)/`Mins`) %>%
      mutate(`Tck W` = `Tck W`/Mins) %>%
      mutate(`K Tck` = `K Tck`/Mins) %>%
      mutate(`Fls` = `Fls`/Mins) %>%
      mutate(`Red` = `Red`/Mins) %>%
      mutate(`Yel` = `Yel`/Mins) %>%
      mutate(`Cards` = `Red` + `Yel`) %>%
      mutate(`Gl Mst` = `Gl Mst`/Mins) #%>%
    # mutate(`` = ``/Mins)
  })
  
  #### Squad: General ####
  
  # link buttons with tabs
  observeEvent(input$sqgen_rating, {updateTabsetPanel(
    session, inputId = "sqgen_tabbox", selected = "panel1"
  )})
  observeEvent(input$sqgen_pointsw, {updateTabsetPanel(
    session, inputId = "sqgen_tabbox", selected = "panel2"
  )})
  observeEvent(input$sqgen_teamggoalass, {updateTabsetPanel(
    session, inputId = "sqgen_tabbox", selected = "panel3"
  )})
  observeEvent(input$sqgen_goalass, {updateTabsetPanel(
    session, inputId = "sqgen_tabbox", selected = "panel4"
  )})
  observeEvent(input$sqgen_cards, {updateTabsetPanel(
    session, inputId = "sqgen_tabbox", selected = "panel5"
  )})
  
  # generate plots
  output$sqgen_plot1 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Av Rat`,
      xlabel = "Games played",
      ylabel = "Average rating",
      targetratio = NA
    )
  })
  output$sqgen_plot2 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Pts/Gm`,
      xlabel = "Games played",
      ylabel = "Points won per game",
      targetratio = NA
    )
  })
  output$sqgen_plot3 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tgls/90`, y = data$`Gls plus Asts`,
      xlabel = "Team goals per 90min",
      ylabel = "Total goals + assists per 90min",
      targetratio = NA
    )
  })
  output$sqgen_plot4 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Gls/90`, y = data$`Asts/90`,
      xlabel = "Goals per 90min",
      ylabel = "Assists per 90min",
      targetratio = NA
    )
  })
  output$sqgen_plot5 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Yel`, y = data$`Red`,
      xlabel = "Yellow cards per 90min",
      ylabel = "Red cards per 90min",
      targetratio = NA
    )
  })
  
  #### Squad: Passing ####
  
  # link buttons with tabs
  observeEvent(input$sqpass_attcomp, {updateTabsetPanel(
    session, inputId = "sqpass_tabbox", selected = "panel1"
  )})
  observeEvent(input$sqpass_compkey, {updateTabsetPanel(
    session, inputId = "sqpass_tabbox", selected = "panel2"
  )})
  observeEvent(input$sqpass_compchan, {updateTabsetPanel(
    session, inputId = "sqpass_tabbox", selected = "panel3"
  )})
  observeEvent(input$sqpass_compass, {updateTabsetPanel(
    session, inputId = "sqpass_tabbox", selected = "panel4"
  )})
  observeEvent(input$sqpass_teamgass, {updateTabsetPanel(
    session, inputId = "sqpass_tabbox", selected = "panel5"
  )})
  observeEvent(input$sqpass_crossattcomp, {updateTabsetPanel(
    session, inputId = "sqpass_tabbox", selected = "panel6"
  )})
  
  # generate plots
  output$sqpass_plot1 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps A/90`, y = data$`Ps C/90`,
      xlabel = "Attempted passes per 90min",
      ylabel = "Completed passes per 90min",
      targetratio = 0.85
    )
  })
  output$sqpass_plot2 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps C/90`, y = data$`K Ps/90`,
      xlabel = "Completed passes per 90min",
      ylabel = "Key passes per 90min",
      targetratio = NA
    )
  })
  output$sqpass_plot3 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps C/90`, y = data$`Ch C/90`,
      xlabel = "Completed passes per 90min",
      ylabel = "Chances created per 90min",
      targetratio = NA
    )
  })
  output$sqpass_plot4 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps C/90`, y = data$`Asts/90`,
      xlabel = "Completed passes per 90min",
      ylabel = "Assists per 90min",
      targetratio = NA
    )
  })
  output$sqpass_plot5 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tgls/90`, y = data$`Asts/90`,
      xlabel = "Team goals per 90min",
      ylabel = "Assists per 90min",
      targetratio = NA
    )
  })
  output$sqpass_plot6 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Cr A`, y = data$`Cr C`,
      xlabel = "Attempted crosses per 90min",
      ylabel = "Completed crosses per 90min",
      targetratio = 0.2
    )
  })
  
  #### Squad: Shooting ####
  
  # link buttons with tabs
  observeEvent(input$sqshoot_atttarg, {updateTabsetPanel(
    session, inputId = "sqshoot_tabbox", selected = "panel1"
  )})
  observeEvent(input$sqshoot_targgoal, {updateTabsetPanel(
    session, inputId = "sqshoot_tabbox", selected = "panel2"
  )})
  observeEvent(input$sqshoot_teamggoal, {updateTabsetPanel(
    session, inputId = "sqshoot_tabbox", selected = "panel3"
  )})
  observeEvent(input$sqshoot_attxg, {updateTabsetPanel(
    session, inputId = "sqshoot_tabbox", selected = "panel4"
  )})
  observeEvent(input$sqshoot_xggoal, {updateTabsetPanel(
    session, inputId = "sqshoot_tabbox", selected = "panel5"
  )})
  
  # generate plots
  output$sqshoot_plot1 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Shot/90`, y = data$`ShT/90`,
      xlabel = "Attempted shots per 90min",
      ylabel = "On target shots per 90min",
      targetratio = 0.5
    )
  })
  output$sqshoot_plot2 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`ShT/90`, y = data$`Gls/90`,
      xlabel = "On target shots per 90min",
      ylabel = "Goals per 90min",
      targetratio = 0.3
    )
  })
  output$sqshoot_plot3 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tgls/90`, y = data$`Gls/90`,
      xlabel = "Team goals per 90min",
      ylabel = "Goals per 90min",
      targetratio = NA
    )
  })
  output$sqshoot_plot4 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Shot/90`, y = data$`xG`,
      xlabel = "Attempted shots per 90min",
      ylabel = "Average xG per 90min",
      targetratio = NA
    )
  })
  output$sqshoot_plot5 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`xG`, y = data$`Gls/90`,
      xlabel = "Average xG per 90min",
      ylabel = "Goals per 90min",
      targetratio = NA
    )
  })
  
  #### Squad: Movement ####
  
  # link buttons with tabs
  observeEvent(input$sqmove_drib, {updateTabsetPanel(
    session, inputId = "sqmove_tabbox", selected = "panel1"
  )})
  observeEvent(input$sqmove_offs, {updateTabsetPanel(
    session, inputId = "sqmove_tabbox", selected = "panel2"
  )})
  observeEvent(input$sqmove_dist, {updateTabsetPanel(
    session, inputId = "sqmove_tabbox", selected = "panel3"
  )})
  
  # generate plots
  output$sqmove_plot1 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`DrbPG`,
      xlabel = "Games played",
      ylabel = "Dribbles per 90min",
      targetratio = NA
    )
  })
  output$sqmove_plot2 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Off`,
      xlabel = "Games played",
      ylabel = "Offsides per 90min",
      targetratio = NA
    )
  })
  output$sqmove_plot3 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Dist/90`,
      xlabel = "Games played",
      ylabel = "Distance covered per 90min",
      targetratio = NA
    )
  })
  
  #### Squad: Aerial ####
  
  # link buttons with tabs
  observeEvent(input$sqaerial_attwon, {updateTabsetPanel(
    session, inputId = "sqaerial_tabbox", selected = "panel1"
  )})
  observeEvent(input$sqaerial_wonkey, {updateTabsetPanel(
    session, inputId = "sqaerial_tabbox", selected = "panel2"
  )})
  
  # generate plots
  output$sqaerial_plot1 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Aer A/90`, y = data$`Hdrs W/90`,
      xlabel = "Attempted aerial challenges per 90min",
      ylabel = "Won aerial challenges per 90min",
      targetratio = NA
    )
  })
  output$sqaerial_plot2 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Hdrs W/90`, y = data$`K Hdrs`,
      xlabel = "Won aerial challenges per 90min",
      ylabel = "Key aerial challenges per 90min",
      targetratio = NA
    )
  })
  
  #### Squad: Tackling ####
  
  # link buttons with tabs
  observeEvent(input$sqtackle_attcomp, {updateTabsetPanel(
    session, inputId = "sqtackle_tabbox", selected = "panel1"
  )})
  observeEvent(input$sqtackle_compkey, {updateTabsetPanel(
    session, inputId = "sqtackle_tabbox", selected = "panel2"
  )})
  observeEvent(input$sqtackle_attfoul, {updateTabsetPanel(
    session, inputId = "sqtackle_tabbox", selected = "panel3"
  )})
  observeEvent(input$sqtackle_attcard, {updateTabsetPanel(
    session, inputId = "sqtackle_tabbox", selected = "panel4"
  )})
  observeEvent(input$sqtackle_int, {updateTabsetPanel(
    session, inputId = "sqtackle_tabbox", selected = "panel5"
  )})
  observeEvent(input$sqtackle_mist, {updateTabsetPanel(
    session, inputId = "sqtackle_tabbox", selected = "panel6"
  )})
  
  # generate plots
  output$sqtackle_plot1 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck A`, y = data$`Tck W`,
      xlabel = "Attempted tackles per 90min",
      ylabel = "Won tackles per 90min",
      targetratio = 0.6
    )
  })
  output$sqtackle_plot2 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck W`, y = data$`K Tck`,
      xlabel = "Won tackles per 90min",
      ylabel = "Key tackles per 90min",
      targetratio = NA
    )
  })
  output$sqtackle_plot3 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck A`, y = data$`Fls`,
      xlabel = "Attempted tackles per 90min",
      ylabel = "Fouls made per 90min",
      targetratio = NA
    )
  })
  output$sqtackle_plot4 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck A`, y = data$`Cards`,
      xlabel = "Attempted tackles per 90min",
      ylabel = "Cards per 90min",
      targetratio = NA
    )
  })
  output$sqtackle_plot5 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Int/90`,
      xlabel = "Games played",
      ylabel = "Interceptions per 90min",
      targetratio = NA
    )
  })
  output$sqtackle_plot6 <- renderPlotly({
    data <- squad_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tcon/90`, y = data$`Gl Mst`,
      xlabel = "Team goals conceded per 90min",
      ylabel = "Mistakes leading to goal per 90min",
      targetratio = NA
    )
  })
  
  #### Scouting: Filters ####
  
  # filtered subset data for reactable presentation
  scout_filtsubset <- reactive({
    shortlist %>%
      select(Name, Rec, Position, PositionParsed, Division, Club, Age, Value, Wage, Mins) %>%
      # filter using the filters inputs
      dplyr::filter(Division %in% input$scout_div) %>%
      dplyr::filter(
        grepl_op(pattern_array = input$scout_pos, x = PositionParsed)
      ) %>%
      dplyr::filter(between(Age, input$scout_age[1], input$scout_age[2])) %>%
      dplyr::filter(Value <= input$scout_maxvalue) %>%
      dplyr::filter(Wage <= input$scout_maxwage) %>%
      dplyr::filter(Mins >= input$scout_minapps*90) %>%
      dplyr::select(-c(Division, PositionParsed)) %>%
      mutate(
        GamesPlayed = round(Mins/90, digits = 1), .keep = "unused", .after = Mins
      ) %>%
      drop_na()
  })
  
  # filtered subset reactable
  output$scout_filtered <- renderReactable({
    tabulate_it(data = scout_filtsubset(), dataset = "scouting")
  })
  
  # filtered subset names: used as an index
  scout_index <- reactive({
    scout_filtsubset() %>% as.data.frame() %>% dplyr::select(Name)# %>% as.vector()
  })
  
  # filtered subset reactable for plots
  scout_data <- reactive({
    subsetdata(
      data = shortlist, reactindex = as.vector(as.data.frame(scout_index())$Name),
      `Av Rat`, `Pts/Gm`, `Clean sheets`, # general
      `Ps A/90`, `Ps C/90`, `K Ps/90`, `Ch C/90`, `Asts/90`, `Cr A`, `Cr C`, # passing
      `Shot/90`, `ShT/90`, `Gls/90`, `Tgls/90`, `xG`, # shooting
      `DrbPG`, `Dist/90`, `Off`, # movement
      `Aer A/90`, `Hdrs W/90`, `K Hdrs`, # aerial
      `Tck R`, `Tck W`, `K Tck`, `Int/90`, `Fls`, `Red`, `Yel`, # tackling 1
      `Tcon/90`, `Gl Mst`#, # tackling 2
    ) %>%
      mutate(`Gls plus Asts` = `Gls/90` + `Asts/90`) %>%
      mutate(`Cr A` = `Cr A`/Mins) %>%
      mutate(`Cr C` = `Cr C`/Mins) %>%
      mutate(`xG` = `xG`/Mins) %>%
      mutate(`Off` = `Off`/Mins) %>%
      mutate(`K Hdrs` = `K Hdrs`/Mins) %>%
      # for some reason no attempted tackles valid data so using ratio and won to calculate it
      mutate(`Tck A` = (`Tck W`/`Tck R`)/`Mins`) %>%
      mutate(`Tck W` = `Tck W`/Mins) %>%
      mutate(`K Tck` = `K Tck`/Mins) %>%
      mutate(`Fls` = `Fls`/Mins) %>%
      mutate(`Red` = `Red`/Mins) %>%
      mutate(`Yel` = `Yel`/Mins) %>%
      mutate(`Cards` = `Red` + `Yel`) %>%
      mutate(`Gl Mst` = `Gl Mst`/Mins) #%>%
      # mutate(`` = ``/Mins)
  })
  
  #### Scouting: General ####
  
  # link buttons with tabs
  observeEvent(input$scgen_rating, {updateTabsetPanel(
    session, inputId = "scgen_tabbox", selected = "panel1"
  )})
  observeEvent(input$scgen_pointsw, {updateTabsetPanel(
    session, inputId = "scgen_tabbox", selected = "panel2"
  )})
  observeEvent(input$scgen_teamggoalass, {updateTabsetPanel(
    session, inputId = "scgen_tabbox", selected = "panel3"
  )})
  observeEvent(input$scgen_goalass, {updateTabsetPanel(
    session, inputId = "scgen_tabbox", selected = "panel4"
  )})
  observeEvent(input$scgen_cards, {updateTabsetPanel(
    session, inputId = "scgen_tabbox", selected = "panel5"
  )})
  
  # generate plots
  output$scgen_plot1 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Av Rat`,
      xlabel = "Games played",
      ylabel = "Average rating",
      targetratio = NA
    )
  })
  output$scgen_plot2 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Pts/Gm`,
      xlabel = "Games played",
      ylabel = "Points won per game",
      targetratio = NA
    )
  })
  output$scgen_plot3 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tgls/90`, y = data$`Gls plus Asts`,
      xlabel = "Team goals per 90min",
      ylabel = "Total goals + assists per 90min",
      targetratio = NA
    )
  })
  output$scgen_plot4 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Gls/90`, y = data$`Asts/90`,
      xlabel = "Goals per 90min",
      ylabel = "Assists per 90min",
      targetratio = NA
    )
  })
  output$scgen_plot5 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Yel`, y = data$`Red`,
      xlabel = "Yellow cards per 90min",
      ylabel = "Red cards per 90min",
      targetratio = NA
    )
  })
  
  #### Scouting: Passing ####
  
  # link buttons with tabs
  observeEvent(input$scpass_attcomp, {updateTabsetPanel(
    session, inputId = "scpass_tabbox", selected = "panel1"
  )})
  observeEvent(input$scpass_compkey, {updateTabsetPanel(
    session, inputId = "scpass_tabbox", selected = "panel2"
  )})
  observeEvent(input$scpass_compchan, {updateTabsetPanel(
    session, inputId = "scpass_tabbox", selected = "panel3"
  )})
  observeEvent(input$scpass_compass, {updateTabsetPanel(
    session, inputId = "scpass_tabbox", selected = "panel4"
  )})
  observeEvent(input$scpass_teamgass, {updateTabsetPanel(
    session, inputId = "scpass_tabbox", selected = "panel5"
  )})
  observeEvent(input$scpass_crossattcomp, {updateTabsetPanel(
    session, inputId = "scpass_tabbox", selected = "panel6"
  )})
  
  # generate plots
  output$scpass_plot1 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps A/90`, y = data$`Ps C/90`,
      xlabel = "Attempted passes per 90min",
      ylabel = "Completed passes per 90min",
      targetratio = 0.85
    )
  })
  output$scpass_plot2 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps C/90`, y = data$`K Ps/90`,
      xlabel = "Completed passes per 90min",
      ylabel = "Key passes per 90min",
      targetratio = NA
    )
  })
  output$scpass_plot3 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps C/90`, y = data$`Ch C/90`,
      xlabel = "Completed passes per 90min",
      ylabel = "Chances created per 90min",
      targetratio = NA
    )
  })
  output$scpass_plot4 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Ps C/90`, y = data$`Asts/90`,
      xlabel = "Completed passes per 90min",
      ylabel = "Assists per 90min",
      targetratio = NA
    )
  })
  output$scpass_plot5 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tgls/90`, y = data$`Asts/90`,
      xlabel = "Team goals per 90min",
      ylabel = "Assists per 90min",
      targetratio = NA
    )
  })
  output$scpass_plot6 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Cr A`, y = data$`Cr C`,
      xlabel = "Attempted crosses per 90min",
      ylabel = "Completed crosses per 90min",
      targetratio = 0.2
    )
  })
  
  #### Scouting: Shooting ####
  
  # link buttons with tabs
  observeEvent(input$scshoot_atttarg, {updateTabsetPanel(
    session, inputId = "scshoot_tabbox", selected = "panel1"
  )})
  observeEvent(input$scshoot_targgoal, {updateTabsetPanel(
    session, inputId = "scshoot_tabbox", selected = "panel2"
  )})
  observeEvent(input$scshoot_teamggoal, {updateTabsetPanel(
    session, inputId = "scshoot_tabbox", selected = "panel3"
  )})
  observeEvent(input$scshoot_attxg, {updateTabsetPanel(
    session, inputId = "scshoot_tabbox", selected = "panel4"
  )})
  observeEvent(input$scshoot_xggoal, {updateTabsetPanel(
    session, inputId = "scshoot_tabbox", selected = "panel5"
  )})
  
  # generate plots
  output$scshoot_plot1 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Shot/90`, y = data$`ShT/90`,
      xlabel = "Attempted shots per 90min",
      ylabel = "On target shots per 90min",
      targetratio = 0.5
    )
  })
  output$scshoot_plot2 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`ShT/90`, y = data$`Gls/90`,
      xlabel = "On target shots per 90min",
      ylabel = "Goals per 90min",
      targetratio = 0.3
    )
  })
  output$scshoot_plot3 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tgls/90`, y = data$`Gls/90`,
      xlabel = "Team goals per 90min",
      ylabel = "Goals per 90min",
      targetratio = NA
    )
  })
  output$scshoot_plot4 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Shot/90`, y = data$`xG`,
      xlabel = "Attempted shots per 90min",
      ylabel = "Average xG per 90min",
      targetratio = NA
    )
  })
  output$scshoot_plot5 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`xG`, y = data$`Gls/90`,
      xlabel = "Average xG per 90min",
      ylabel = "Goals per 90min",
      targetratio = NA
    )
  })
  
  #### Scouting: Movement ####
  
  # link buttons with tabs
  observeEvent(input$scmove_drib, {updateTabsetPanel(
    session, inputId = "scmove_tabbox", selected = "panel1"
  )})
  observeEvent(input$scmove_offs, {updateTabsetPanel(
    session, inputId = "scmove_tabbox", selected = "panel2"
  )})
  observeEvent(input$scmove_dist, {updateTabsetPanel(
    session, inputId = "scmove_tabbox", selected = "panel3"
  )})
  
  # generate plots
  output$scmove_plot1 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`DrbPG`,
      xlabel = "Games played",
      ylabel = "Dribbles per 90min",
      targetratio = NA
    )
  })
  output$scmove_plot2 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Off`,
      xlabel = "Games played",
      ylabel = "Offsides per 90min",
      targetratio = NA
    )
  })
  output$scmove_plot3 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Dist/90`,
      xlabel = "Games played",
      ylabel = "Distance covered per 90min",
      targetratio = NA
    )
  })
  
  #### Scouting: Aerial ####
  
  # link buttons with tabs
  observeEvent(input$scaerial_attwon, {updateTabsetPanel(
    session, inputId = "scaerial_tabbox", selected = "panel1"
  )})
  observeEvent(input$scaerial_wonkey, {updateTabsetPanel(
    session, inputId = "scaerial_tabbox", selected = "panel2"
  )})
  
  # generate plots
  output$scaerial_plot1 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Aer A/90`, y = data$`Hdrs W/90`,
      xlabel = "Attempted aerial challenges per 90min",
      ylabel = "Won aerial challenges per 90min",
      targetratio = NA
    )
  })
  output$scaerial_plot2 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Hdrs W/90`, y = data$`K Hdrs`,
      xlabel = "Won aerial challenges per 90min",
      ylabel = "Key aerial challenges per 90min",
      targetratio = NA
    )
  })
  
  #### Scouting: Tackling ####
  
  # link buttons with tabs
  observeEvent(input$sctackle_attcomp, {updateTabsetPanel(
    session, inputId = "sctackle_tabbox", selected = "panel1"
  )})
  observeEvent(input$sctackle_compkey, {updateTabsetPanel(
    session, inputId = "sctackle_tabbox", selected = "panel2"
  )})
  observeEvent(input$sctackle_attfoul, {updateTabsetPanel(
    session, inputId = "sctackle_tabbox", selected = "panel3"
  )})
  observeEvent(input$sctackle_attcard, {updateTabsetPanel(
    session, inputId = "sctackle_tabbox", selected = "panel4"
  )})
  observeEvent(input$sctackle_int, {updateTabsetPanel(
    session, inputId = "sctackle_tabbox", selected = "panel5"
  )})
  observeEvent(input$sctackle_mist, {updateTabsetPanel(
    session, inputId = "sctackle_tabbox", selected = "panel6"
  )})
  
  # generate plots
  output$sctackle_plot1 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck A`, y = data$`Tck W`,
      xlabel = "Attempted tackles per 90min",
      ylabel = "Won tackles per 90min",
      targetratio = 0.6
    )
  })
  output$sctackle_plot2 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck W`, y = data$`K Tck`,
      xlabel = "Won tackles per 90min",
      ylabel = "Key tackles per 90min",
      targetratio = NA
    )
  })
  output$sctackle_plot3 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck A`, y = data$`Fls`,
      xlabel = "Attempted tackles per 90min",
      ylabel = "Fouls made per 90min",
      targetratio = NA
    )
  })
  output$sctackle_plot4 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tck A`, y = data$`Cards`,
      xlabel = "Attempted tackles per 90min",
      ylabel = "Cards per 90min",
      targetratio = NA
    )
  })
  output$sctackle_plot5 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Mins`, y = data$`Int/90`,
      xlabel = "Games played",
      ylabel = "Interceptions per 90min",
      targetratio = NA
    )
  })
  output$sctackle_plot6 <- renderPlotly({
    data <- scout_data() %>% as.data.frame()
    plot_it(
      data = data,
      x = data$`Tcon/90`, y = data$`Gl Mst`,
      xlabel = "Team goals conceded per 90min",
      ylabel = "Mistakes leading to goal per 90min",
      targetratio = NA
    )
  })
  
  #### Scouting: Shortlist ####
  
  # render final table
  output$scout_listfinal <- renderReactable({
    # intersection of all players chosen in the different scouting submenus
    listfinal <- Reduce(intersect, list(
      input$scgen_shortl,
      input$scpass_shortl,
      input$scshoot_shortl,
      input$scmove_shortl,
      input$scaerial_shortl,
      input$sctackle_shortl
    ))
    # filter filtered subset scouting data with only the final names
    listfinal_data <-  shortlist %>%
      select(Name, Rec, Position, Club, Age, Value, Wage, Mins) %>%
      mutate(
        GamesPlayed = round(Mins/90, digits = 1), .keep = "unused", .after = Mins
      ) %>%
      dplyr::filter(Name %in% listfinal)
    # reactable
    tabulate_it(data = listfinal_data, dataset = "scouting")
  })
  
}

# App data loaded...check line 458
toc()

# Run app -----------------------------------------------------------------

# status message
cat("\nApp will launch in 3...")
Sys.sleep(1)
cat("2...")
Sys.sleep(1)
cat("1...\n")
Sys.sleep(1)
# run without warnings
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
