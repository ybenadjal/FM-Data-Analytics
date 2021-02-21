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
  "XML",
  "tictoc",
  "DT"
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
library(magrittr) # tidyverse package
library(plotly) # plotly package
library(DT) # DT package
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


# App inputs 1 ------------------------------------------------------------
# independant of the data

# unique FM positions
positions <- c("GK", # goalkeepers
               "D(L)", "D(C)", "D(R)", # defenders
               "WB(L)", "WB(R)", # wingbacks
               "DM", # defensive midfielders
               "M(L)", "M(C)", "M(R)", # midfielders
               "AM(L)", "AM(C)", "AM(R)", # attacking midfielders
               "ST(C)" # strikers
)

# FM data points: attributes
fm_attributes <- c("Aer","Cmd","Com","Ecc","Han","Kic","1v1","Pun","Ref","TRO",
                "Thr","Agg","Ant","Bra","Cmp","Cnt","Dec","Det","Fla","Ldr",
                "OtB","Pos","Tea","Vis","Wor","Acc","Agi","Bal","Jum","Nat",
                "Pac","Sta","Str","Cor","Cro","Dri","Fin","Fir","Fre","Hea",
                "Lon","L Th","Mar","Pas","Pen","Tck","Tec")

# FM data points: statistics (chalkboard)
fm_statschalkboard <- c("Hdrs A","Aer A/90","Hdrs","Hdrs W/90","Hdr %",
                     "K Hdrs","Cr A","Cr C/A","Cr C","Gl Mst","Distance",
                     "Dist/90","Drb","DrbPG","Off","Asts/90","CCC","Ch C/90",
                     "K Pas","K Ps/90","Pas A","Ps A/90","Pas %","Ps C",
                     "Ps C/90","Svh","Svp","Svt","Shots","ShT","ShT/90",
                     "Shot %","Shot/90","Itc","Int/90","K Tck","Tck A",
                     "Tck R","Tck W","Tck1")

# FM data points: statistics (general)
fm_statsgeneral <- c("AT Apps","AT Gls","AT Lge Apps","AT Lge Gls","Apps",
                  "Ast","Mins/Gl","Av Rat","Clean sheets","Con/90","FA",
                  "Fls","Gwin","D","Lost","G. Mis","Won","Gls","Conc",
                  "Gls/90","Last 5 FT Games","Last 5 Games","Mins",
                  "Mins/Gm","Last C","Last Gl","Pens","Pens Faced",
                  "Pens Saved","Pens Saved Ratio","Pens S","Pen/R","PoM",
                  "Pts/Gm","Red","Starts","Tcon/90","Tgls","Tcon","Tgls/90",
                  "xG", "Yel","Int Apps","Int Ast","Int Av Rat","Int Conc")

# FM data points: custom category - financials
fm_financials <- c("Appearance Fee","Assist Bonus","Cln Sheet Bonus","Goal Bonus",
                "Int Cap Bonus","SLAB","SLGAB","SLGB","Team Year Bonus",
                "Top Score Bonus","Unused Sub Fee","WaCLG","Injury Rls",
                "Min Fee Rls","Min Fee Rls Clubs In Cont Comp",
                "Min Fee Rls Clubs Mjr Cont Comp","Min Fee Rls to Higher Div",
                "Min Fee Rls to Domestic Clubs","Min Fee Rls to Foreign Clubs",
                "Non Prom Rls Cls","Non-Playing Rel","Relegation Release",
                "Wage","Wage After Tax","Wage Contrib.","New Wage","Max AP",
                "Max WD","Min AP","Min WD","Asking Price","Fee",
                "Last Trans. Fee","Ovr","Transfer Fees Received","Value")

# FM data points: custom category - stats for progress tables
fm_stats <- c("Aer A/90", "Hdrs W/90", "Hdr %", "K Hdrs", "Gl Mst", "Dist/90",
            "DrbPG", "Off", "Cr A", "Cr C", "Cr C/A", "Ps A/90", "Ps C/90",
            "Pas %", "K Ps/90", "Ch C/90", "Asts/90", "Shot/90", "ShT/90",
            "Shot %", "Gls/90", "Int/90", "K Tck", "Tck A", "Tck W", "Tck R",
            "Svh", "Svp", "Svt", "Con/90", "Apps", "Starts", "Mins", "Av Rat",
            "Clean sheets", "Gwin", "PoM", "Pts/Gm", "Tcon/90", "Tgls/90",
            "Pens Faced", "Pens Saved", "Pens Saved Ratio", "Pens", "Pens S",
            "Pen/R", "FA", "Fls", "Yel", "Red")

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
    mutate_at(fm_attributes, function(x) {
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
    mutate_at(c(fm_statschalkboard,fm_statsgeneral),
              function(x) {x %>% str_remove_all(",") %>% as.numeric}) %>%
    # Format financial values as numeric
    mutate_at(fm_financials, format_money) %>%
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
# and the less saturated color in the tables, depending on the variable 'plot'
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

# DT tables for the progress submenus
# tabulates attributes/performance data difference
tabulate_progress <- function(dataList, prevdate, currdate, type) {
  
  # transform prevdate and currdate from the inputs format into the original format
  prevdate %<>% as.Date("%d %b %Y") %>% format("%Y%m%d")
  currdate %<>% as.Date("%d %b %Y") %>% format("%Y%m%d")
  
  # determine if working with attributes or performance stats
  if (type == "attr") {
    datapoints <- fm_attributes
  } else {
    datapoints <- fm_stats
  }
  
  # previous date data
  previous <- dataList[[prevdate]]() %>% as.data.frame() %>%
    # select required columns: base 
    select(Name, Age, Position, Rec, all_of(datapoints))
  
  # current date data
  current <- dataList[[currdate]]() %>% as.data.frame() %>%
    select(Name, Age, Position, Rec, all_of(datapoints))
  
  # difference dataframe
  # name column: union of all names in the prev & curr data sets
  difference <- data.frame(Name = base::union(previous$Name, current$Name))
  # add a comment column: check if player is new or out
  # stack() to transform named vector to dataframe
  comment <- stack(sapply(difference$Name, function(x) {
    case_when(
      # if name in prev but not in curr: player is out
      (x %in% previous$Name) & !(x %in% current$Name) ~ "Player no longer at club",
      # if name not in prev but in curr: player is new
      !(x %in% previous$Name) & (x %in% current$Name) ~ "New player",
      # if name in prev and curr: player has stayed; no comment
      (x %in% previous$Name) & (x %in% current$Name) ~ ""
    )
  })) %>%
    # rename values column to facilitate the inner_join later
    mutate(Comment = values, .keep = "unused")
  
  # add corresponding comment to the player
  difference %<>% inner_join(comment, by = c("Name" = "ind"))
  
  # difference: prev version contains all names from difference
  difference_prev <- difference %>% left_join(previous, by = c("Name" = "Name"))
  # difference: curr version contains all names from difference
  difference_curr <- difference %>% left_join(current, by = c("Name" = "Name"))
  
  # set difference as current version
  difference <- difference_curr
  # compute per 90 statistics for appropriate data if tabulating performance progress
  if (type != "attr") {
    for (i in list(difference_curr, difference_prev)) {
      i %<>%
        mutate(`K Hdrs/90` = `K Hdrs`/Mins, .after = `K Hdrs`) %>%
        mutate(`Gl Mst/90` = `Gl Mst`/Mins, .after = `Gl Mst`) %>%
        mutate(`Off/90` = `Off`/Mins, .after = `Off`) %>%
        mutate(`Cr A/90` = `Cr A`/Mins, .after = `Cr A`) %>%
        mutate(`Cr C/90` = `Cr C`/Mins, .after = `Cr C`) %>%
        mutate(`K Tck/90` = `K Tck`/Mins, .after = `K Tck`) %>%
        mutate(`Tck A/90` = `Tck A`/Mins, .after = `Tck A`) %>%
        mutate(`Tck W/90` = `Tck W`/Mins, .after = `Tck W`) %>%
        mutate(`Svh/90` = `Svh`/Mins, .after = `Svh`) %>%
        mutate(`Svp/90` = `Svp`/Mins, .after = `Svp`) %>%
        mutate(`Svt/90` = `Svt`/Mins, .after = `Svt`) %>%
        mutate(`FA/90` = `FA`/Mins, .after = `FA`) %>%
        mutate(`Fls/90` = `Fls`/Mins, .after = `Fls`) %>%
        mutate(`Yel/90` = `Yel`/Mins, .after = `Yel`) %>%
        mutate(`Red/90` = `Red`/Mins, .after = `Red`) %>%
        select(-c(
          `K Hdrs`, `Gl Mst`, `Off`, `Cr A`, `Cr C`, `K Tck`, `Tck A`,
          `Tck W`, `Svh`, `Svp`, `Svt`, `FA`, `Fls`, `Yel`, `Red`
        ))
    }
  }
  
  # compute difference between curr and prev
  for (i in c("Rec", datapoints)) {
    difference[i] <- difference_curr[[i]] - difference_prev[[i]]
  }
  
  # add overall column if tabulating the attributes progress
  if (type == "attr") {
    difference %<>% mutate(Overall = rowSums(select(., all_of(datapoints))), .after = "Rec")
    # round values to 2 digits
    for (i in c("Rec", datapoints)) {
      difference[i] <- round(difference[[i]], digits = 2)
    }
  }
  
  # initialise DT column options
  DTcols_options <- list()
  # options depending on dataset type: attr or stats
  if (type == "attr") {
    # grouped columns
    DTcols_options$cols <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          # group columns and hover info
          th(rowspan = 2, "Name", title = "Player Name"),
          th(rowspan = 2, "Comment", title = "Comment"),
          th(rowspan = 2, "Age", title = "Age"),
          th(rowspan = 2, "Position", title = "Position"),
          th(rowspan = 2, "DNA Rating", title = "DNA Rating"),
          th(rowspan = 2, "Overall", title = "Total variation of attributes"),
          th(colspan = 11, "Goalkeeping", aligne = "center"),
          th(colspan = 14, "Mental", aligne = "center"),
          th(colspan = 8, "Physical", aligne = "center"),
          th(colspan = 14, "Technical", aligne = "center")
        ),
        tr(
          # individual columns and hover info
          th("Aer", title = "Aerial Reach"),
          th("Cmd", title = "Command Of Area"),
          th("Com", title = "Communication"),
          th("Ecc", title = "Eccentricity"),
          th("Han", title = "Handling"),
          th("Kic", title = "Kicking"),
          th("1v1", title = "One On Ones"),
          th("Pun", title = "Punching (Tendency)"),
          th("Ref", title = "Reflexes"),
          th("TRO", title = "Rushing Out (Tendency)"),
          th("Thr", title = "Throwing"),
          th("Agg", title = "Aggression"),
          th("Ant", title = "Anticipation"),
          th("Bra", title = "Bravery"),
          th("Cmp", title = "Composure"),
          th("Cnt", title = "Concentration"),
          th("Dec", title = "Decisions"),
          th("Det", title = "Determination"),
          th("Fla", title = "Flair"),
          th("Ldr", title = "Leadership"),
          th("OtB", title = "Off The Ball"),
          th("Pos", title = "Positioning"),
          th("Tea", title = "Teamwork"),
          th("Vis", title = "Vision"),
          th("Wor", title = "Work Rate"),
          th("Acc", title = "Acceleration"),
          th("Agi", title = "Agility"),
          th("Bal", title = "Balance"),
          th("Jum", title = "Jumping Reach"),
          th("Nat", title = "Natural Fitnes"),
          th("Pac", title = "Pace"),
          th("Sta", title = "Stamina"),
          th("Str", title = "Strength"),
          th("Cor", title = "Corners"),
          th("Cro", title = "Crossing"),
          th("Dri", title = "Dribbling"),
          th("Fin", title = "Finishing"),
          th("Fir", title = "First Touch"),
          th("Fre", title = "Free Kicks"),
          th("Hea", title = "Heading"),
          th("Lon", title = "Long Shots"),
          th("L Th", title = "Long Throws"),
          th("Mar", title = "Marking"),
          th("Pas", title = "Passing"),
          th("Pen", title = "Penalty Taking"),
          th("Tck", title = "Tackling"),
          th("Tec", title = "Technique")
        )
      )
    ))
    # background color for grouped columns names
    DTcols_options$group_JSrender <- "
    function(thead) {
    $(thead).closest('thead').find('th').eq(6).css('background-color', '#D9E1F2');
    $(thead).closest('thead').find('th').eq(7).css('background-color', '#8EA9DB');
    $(thead).closest('thead').find('th').eq(8).css('background-color', '#D9E1F2');
    $(thead).closest('thead').find('th').eq(9).css('background-color', '#8EA9DB')
    }
    "
    # sort order
    DTcols_options$order <- list(
      list(1, "asc"), # comment
      list(5, "desc"), # overall
      list(4, "desc"), # DNA rating
      list(2, "desc") # Age
    )
  } else {
    # grouped columns
    DTcols_options$cols <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          # group columns and hover info
          th(rowspan = 2, "Name", title = "Player Name"),
          th(rowspan = 2, "Comment", title = "Comment"),
          th(rowspan = 2, "Age", title = "Age"),
          th(rowspan = 2, "Position", title = "Position"),
          th(rowspan = 2, "DNA Rating", title = "DNA Rating"),
          th(colspan = 4, "Aerial Challenges"),
          th(colspan = 1, "Mistakes", aligne = "center"),
          th(colspan = 3, "Movement", aligne = "center"),
          th(colspan = 3, "Crosses", aligne = "center"),
          th(colspan = 6, "Passes", aligne = "center"),
          th(colspan = 4, "Shots", aligne = "center"),
          th(colspan = 5, "Tackles & Interceptions", aligne = "center"),
          th(colspan = 4, "Saves", aligne = "center"),
          th(colspan = 20, "General", aligne = "center")
        ),
        tr(
          # individual columns and hover info
          th("Aer A/90", title = "Aerial Challenges attempts per 90 minutes"),
          th("Hdrs W/90", title = "Headers won per 90 minutes"),
          th("Hdr %", title = "Headers Won Ratio"),
          th("K Hdrs/90", title = "Key Headers per 90 minutes"),
          th("Gl Mst/90", title = "Mistakes Leading To Goal per 90 minutes"),
          th("Dist/90", title = "Distance Covered Per 90 Minutes"),
          th("DrbPG", title = "Dribbles Made Per Game"),
          th("Off/90", title = "Offsides per 90 minutes"),
          th("Cr A/90", title = "Cross Attempts per 90 minutes"),
          th("Cr C/90", title = "Crosses Completed per 90 minutes"),
          th("Cr C/A", title = "Cross Completion Ratio"),
          th("Ps A/90", title = "Pass attempts per 90 minutes"),
          th("Ps C/90", title = "Passes completed per 90 minutes"),
          th("Pas %", title = "Pass Completion Ratio"),
          th("K Ps/90", title = "Key Passes per 90 minutes"),
          th("Ch C/90", title = "Chances Created per 90 minutes"),
          th("Asts/90", title = "Assists per 90 minutes"),
          th("Shot/90", title = "Shots per 90 minutes"),
          th("ShT/90", title = "Shorts on target per 90 minutes"),
          th("Shot %", title = "Shots On Target Ratio"),
          th("Gls/90", title = "Goals per 90 minutes"),
          th("Int/90", title = "Interceptions per 90 minutes"),
          th("K Tck/90", title = "Key Tackles per 90 minutes"),
          th("Tck A/90", title = "Tackle attempts per 90 minutes"),
          th("Tck W/90", title = "Tackles Completed per 90 minutes"),
          th("Tck R", title = "Tackle completion ratio"),
          th("Svh/90", title = "Saves Held per 90 minutes"),
          th("Svp/90", title = "Saves Parried per 90 minutes"),
          th("Svt/90", title = "Saves Tipped per 90 minutes"),
          th("Con/90", title = "Conceded per 90 minutes"),
          th("Apps", title = "Appearances"),
          th("Starts", title = "Starting Appearances"),
          th("Mins", title = "Minutes"),
          th("Av Rat", title = "Average rating"),
          th("Clean sheets", title = "Clean sheets"),
          th("Gwin", title = "Game win ratio"),
          th("PoM", title = "Player Of The Match"),
          th("Pts/Gm", title = "Points won per game"),
          th("Tcon/90", title = "Team conceded per 90 minutes"),
          th("Tgls/90", title = "Team goals per 90 minutes"),
          th("Pens Faced", title = "Penalties Faced"),
          th("Pens Saved", title = "Penalties Saved"),
          th("Pens Saved Ratio", title = "Penalties Saved Ratio"),
          th("Pens", title = "Penalties"),
          th("Pens S", title = "Penalties scored"),
          th("Pen/R", title = "Penalties scored ratio"),
          th("FA/90", title = "Fouls Against per 90 minutes"),
          th("Fls/90", title = "Fouls Made per 90 minutes"),
          th("Yel/90", title = "Yellow cards per 90 minutes"),
          th("Red/90", title = "Red cards per 90 minutes")
        )
      )
    ))
    # background color for grouped columns names
    DTcols_options$group_JSrender <- "
    function(thead) {
    $(thead).closest('thead').find('th').eq(5).css('background-color', '#D9E1F2');
    $(thead).closest('thead').find('th').eq(6).css('background-color', '#8EA9DB');
    $(thead).closest('thead').find('th').eq(7).css('background-color', '#D9E1F2');
    $(thead).closest('thead').find('th').eq(8).css('background-color', '#8EA9DB');
    $(thead).closest('thead').find('th').eq(9).css('background-color', '#D9E1F2');
    $(thead).closest('thead').find('th').eq(10).css('background-color', '#8EA9DB');
    $(thead).closest('thead').find('th').eq(11).css('background-color', '#D9E1F2');
    $(thead).closest('thead').find('th').eq(12).css('background-color', '#8EA9DB');
    $(thead).closest('thead').find('th').eq(13).css('background-color', '#D9E1F2')
    }
    "
    # sort order
    DTcols_options$order <- list(
      list(1, "asc"), # comment
      list(4, "desc"), # DNA rating
      list(2, "desc") # Age
    )
  }
  
  # number of freezed columns
  nb_freezed_cols <- if_else(type == "attr", 6, 5)
  
  # DT table
  DT::datatable(
    data = difference,
    # no row names displayed
    rownames = FALSE,
    # replace Rec column name: DNA rating
    colnames = c("DNA Rating" = "Rec"),
    # required extensions
    extensions = "FixedColumns",
    # options
    options = list(
      # freeze first 6 columns for attributes and 5 for stats
      fixedColumns = list(leftColumns = nb_freezed_cols),
      # no paging, scroll instead
      paging = FALSE,
      # grouped columns styling
      headerCallback = JS(DTcols_options$group_JSrender),
      # order
      order = DTcols_options$order,
      # horizontal scrolling enabled
      scrollX = TRUE,
      # vertical scrolling enabled
      scrollY = "450px"
    ),
    # format grouped columns
    container = DTcols_options$cols
  ) %>%
    # format font color and cell background depending on data
    formatStyle(
      5:dim(difference)[1],
      backgroundColor = styleInterval(c(-.0001,0), c("#FFC7CE","#FFEB9C", "#C6EFCE")),
      color = styleInterval(c(-.0001,0), c("#9C0006","#9C6500", "#006100"))
    )
}

# DT tables for the filtered data
tabulate_filtdata <- function(data, dataset) {
  # name of the Rec column in the table
  Rec_name <- if_else(dataset == "squad", "DNA Rating", "Scouting Rec")
  # replace name in data
  names(data)[2] <- Rec_name
  
  # DT table
  DT::datatable(
    data = data,
    # no row names displayed
    rownames = FALSE,
    # replace Rec column name by appropriate name: dataset
    # add "p/w" to wage column name
    colnames = c("Wage (p/w)" = "Wage"),
    # options
    options = list(
      # no paging, scroll instead
      paging = FALSE,
      # order
      order = list(
        list(7, "desc"), # games played
        list(5, "desc"), # value
        list(6, "desc"), # wage
        list(1, "desc"), # rec
        list(4, "desc") # age
      ),
      # horizontal scrolling enabled
      scrollX = TRUE,
      # vertical scrolling enabled
      scrollY = "500px"
    )
  ) %>%
    # format font color and cell background depending on data
    formatStyle(
      # format all row based on Rec's value
      2, target = "row",
      # original colors
      # c("#bf3eff", "#ff0000", "#ffa500", "#ffff00", "#00ee00", "#008b00")
      backgroundColor = styleInterval(
        c(-1, 49, 59, 69, 84),
        c("#e5b1ff", "#ff9999", "#ffdb99", "#ffff99", "#92ff92", "#20ff20")
      ),
      color = styleInterval(
        c(-1, 49, 59, 69, 84),
        c("#6c00a2", "#990000", "#996300", "#666600", "#008100", "#004000")
      )
    ) %>%
    # format Value and Wage as currency
    formatCurrency(
      columns = c("Value", "Wage (p/w)"),
      currency = "\u20ac ",
      interval = 3, mark = ",",
      digits = 2, dec.mark = ".",
      before = TRUE
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
# extract all squad files in the data folder
squads <- list.files("data/") %>% .[grepl("^squad", list.files("data"))]
# extract the in-game dates of each squad snapshot
squads_dates <- squads %>% str_remove_all(".html|squad_")
# extract the most recent in-game date from the squad snapshots
squads_latest_date <- squads_dates %>% as.numeric() %>% max() %>% as.character()
# empty list to contain all the squad snapshots
squads <- list()
# import all squad snapshots in a list
for (i in squads_dates) {
  # import squad snapshot
  squads[[i]] <- parse_FMoutput(paste0("data/squad_", i, ".html"))
  # # import squad snapshot
  # squads$tempname <- parse_FMoutput(paste0("data/squad_", i, ".html"))
  # # replace the name by the date
  # names(squads) %<>% replace(. == "tempname", i)
}
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
# clean & format all squad snapshot data
for (i in squads_dates) {
  squads[[i]] %<>% mutate(Rec = NA, .after = Name) %>% cleandata()
}
toc()

# clean & format shortlist data and measure execution time
cat("\nCleaning & formatting shortlist data...")
tic("\nCleaned & formatted shortlist data")
shortlist %<>% cleandata()
toc()

# App inputs 2 ------------------------------------------------------------
# dependant on the data

# status message
cat("\nLoading app data...")
tic("\nLoaded app data")

# unique clubs appearing in the squad
# i.e. the managed clubs + clubs with players on loan
# managed club first on the list
sqclubs <- squads[[squads_latest_date]]$Club %>%
  # get frequencies of Club in the list
  cbind(freq = ave(squads[[squads_latest_date]]$Club, squads[[squads_latest_date]]$Club, FUN = length)) %>%
  unique() %>%
  as.data.frame() %>%
  # sort by desc order using the frequencies: managed club in first position
  dplyr::arrange(desc(freq)) %>%
  # pull list of clubs as vector
  dplyr::pull(1)

# unique divisions appearing in the shortlist (put free player at the end)
divisions <- shortlist$Division %>% unique() %>% sort() %>%
  .[. != "Free player/unknown"] %>% c("Free player/unknown")


# App ui: header ----------------------------------------------------------

Header <- dashboardHeader(
  title = "FM Data Analytics"
)

# App ui: sidebar ---------------------------------------------------------

Sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Home", tabName = "home", icon = icon("home"),
      selected = TRUE
    ),
    
    menuItem(
      "Squad Analysis", tabName = "squad", icon = icon("users"),
      startExpanded = FALSE,
      menuSubItem(
        "DNA Model", tabName = "sqmodel", icon = icon("sliders-h")
      ),
      menuSubItem(
        "Filters", tabName = "sqfilter", icon = icon("filter")
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
      ),
      menuSubItem(
        "Progress", tabName = "sqprog", icon = icon("chart-line")
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
    #### Home ####
    tabItem(
      tabName = "home",
      fluidRow(
        # valuebox for the number of players at the club
        valueBox(
          value = squads[[squads_latest_date]] %>% nrow(),
          subtitle = "players at the club",
          icon = icon("users"), color = "aqua",
          width = 4
        ),
        # valuebox for the number of players in the shortlist
        valueBox(
          value = shortlist %>% nrow(),
          subtitle = "players in the shortlist",
          icon = icon("search"), color = "aqua",
          width = 4
        )
      ),
      tabBox(
        title = "Summary",
        width = 12, height = "70vh",
        tabPanel(
          # goal contributions distribution bar plot
          title = "Goal Contributions",
          plotlyOutput(outputId = "home_plot_gcd")
        ),
        tabPanel(
          # playing time boxplot
          title = "Playing Time",
          plotlyOutput(outputId = "home_plot_pt"),
          HTML(
            "
            <p><br></p>
            <p><em>* Use the boxplot diagrams to understand the data distribution. Actual points are available on the left of each diagram and information is available when hovered on.</em></p>
            "
          )
        ),
        tabPanel(
          # wage boxplot
          title = "Wage",
          plotlyOutput(outputId = "home_plot_wage"),
          HTML(
            "
            <p><br></p>
            <p><em>* Use the boxplot diagrams to understand the data distribution. Actual points are available on the left of each diagram and information is available when hovered on.</em></p>
            <p><em>** <strong>Loaned players</strong> and <strong>loaned out players</strong> are <u>not included</u>.</em></p>
            "
          )
        )
      )
    ),
    
    #### Squad: DNA Model submenu ####
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
          DT::DTOutput(
            outputId = "squad_filtered"
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
    
    #### Squad: Progress submenu ####
    tabItem(
      tabName = "sqprog",
      # progress analysis tabbox
      tabBox(
        width = 12, height = "85vh",
        tabPanel(
          title = "Dates selection",
          HTML(
            "
            <p>Select a <em>&quot;start date&quot;</em> and an <em>&quot;end date&quot;</em> to compare your squad&apos;s data between the selected dates. The available dates to select from are inferred from the name of your squad data files extracted from Football Manager and stored in your <strong>data</strong> folder. To avoid errors, name all your squad data as follows: <strong><em>&quot;</em></strong><strong><em>squad_yyyymmdd&quot;</em></strong>, where <em>&quot;yyyymmdd&quot;</em> represents the in-game date.</p>
            <p><br></p>
            "
          ),
          # start date
          pickerInput(
            inputId = "squad_startdate", label = "Start date: ",
            choices = squads_dates %>% as.Date(format = "%Y%m%d") %>% sort() %>% format("%d %b %Y"),
            selected = squads_dates %>% min() %>% as.Date(format = "%Y%m%d") %>% format("%d %b %Y"),
            multiple = FALSE,
            options = list(
              `size` = 5
            )
          ),
          # end date
          pickerInput(
            inputId = "squad_enddate", label = "End date: ",
            choices = squads_dates %>% as.Date(format = "%Y%m%d") %>% sort() %>% format("%d %b %Y"),
            selected = squads_latest_date %>% as.Date(format = "%Y%m%d") %>% format("%d %b %Y"),
            multiple = FALSE,
            options = list(
              `size` = 5
            )
          )
        ),
        tabPanel(
          title = "Performance",
          DT::DTOutput(
            outputId = "squad_progperf"
          )
        ),
        tabPanel(
          title = "Attributes",
          DT::DTOutput(
            outputId = "squad_progattr"
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
          DT::DTOutput(
            outputId = "scout_filtered"
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
        DT::DTOutput(
          outputId = "scout_listfinal"
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
  
  #### Home ####
  
  # goal contributions distribution plot
  output$home_plot_gcd <- renderPlotly({
    plot_ly(
      # goal contribution distribution data
      # use latest snapshot of the squad
      data = squads[[squads_latest_date]] %>%
        # required data points: name, club, goals, assists
        select(Name, Club, Gls, Ast, Position, Wage, Age, Mins) %>%
        # remove NA values
        drop_na() %>%
        # keep only players that are currently at the club, i.e remove loaned players
        dplyr::filter(Club == sort(table(Club), decreasing = TRUE) %>% names() %>% .[1]) %>%
        # remove players that have no goals 
        dplyr::filter(Gls + Ast != 0),
      # order goals and assists by decreasing order: left to right
      # add goals data
      x = ~reorder(reorder(Name, -Ast), -Gls), y = ~Gls, name = "Goals",
      type = "bar",
      # adds text when marker is hovered on
      hoverinfo = "text",
      # define text logic: name, age, position, mins, ratio and marker value
      hovertext = ~paste0(
        "<b>Name: ", Name,
        "<br>Age: </b>", Age,
        "<br><b>Position: </b>", Position,
        "<br><b>Wage: </b> \u20AC ", formatC(Wage, format="f", big.mark = ",", digits=0), " p/w",
        "<br><b>Games played: </b>", round(Mins/90, digits = 1)
      ),
      # text on the bar
      text = ~Gls, textposition = "auto",
      marker = list(color = "purple")
    ) %>%
      add_trace(
        # add assists data
        y = ~Ast, name = "Assists",
        # adds text when marker is hovered on
        hoverinfo = "text",
        # define text logic
        hovertext = ~paste0(
          "<b>Name: ", Name,
          "<br>Age: </b>", Age,
          "<br><b>Position: </b>", Position,
          "<br><b>Wage: </b> \u20AC ", formatC(Wage, format="f", big.mark = ",", digits=0), " p/w",
          "<br><b>Games played: </b>", round(Mins/90, digits = 1)
        ),
        # text on the bar
        text = ~Ast, textposition = "auto",
        marker = list(color = "cyan")
      ) %>%
      # no axis labels, add title
      layout(
        # title: X goals and Y assists in Z competitive matches
        title = ~paste0(
          sum(replace_na(Gls, 0)),
          " goals and ",
          sum(replace_na(Ast, 0)),
          " assists in ",
          # total number of minutes divided by 11 players divided by 90 minutes
          # gives total of competitive matches played
          sum(replace_na(Mins, 0)) %>% prod(1/11) %>% prod(1/90),
          " competitive matches"
        ),
        xaxis = list(title = ""), yaxis = list(title = ""),
        # data used for the dynamic plot title
        data = squads[[squads_latest_date]] %>%
          select(Club, Gls, Ast, Mins) %>%
          dplyr::filter(Club == sort(table(Club), decreasing = TRUE) %>% names() %>% .[1])
      )
  })
  
  # playing time distribution plot
  output$home_plot_pt <- renderPlotly({
    # dataframe for player status (agreed playing time) and status' category
    apt_category <- data.frame(apt = c(
      "Star Player", "Important Player", "Regular Starter",
      "Squad Player", "Impact Sub", "Fringe Player",
      "Emergency Backup", "B Team Regular", "Surplus to Requirements",
      "Breakthrough Prospect", "Future Prospect", "Youngster",
      "First-Choice Goalkeeper", "Cup Goalkeeper", "Backup",
      "Loaned out, Age\u226523", "Loaned out, 19\u2264Age\u226422", "Loaned out, Age\u226418"
    ), cat = c(rep("Senior Squad", 9), rep("Youth Players", 3), rep("Goalkeepers", 3), rep("Loaned Players", 3))) %>%
      # add number of occurrences for each player status
      mutate(
        apt = paste0(
          apt, " (", table(squads[[squads_latest_date]]$`Agreed Playing Time`)[apt], ")"
        ) %>%
          # replace NA by 0 in the occurrences
          str_replace_all("NA", "0")
      )
    
    plot_ly(
      # playing time data
      data = squads[[squads_latest_date]] %>%
        # required data points: playing time, age, minutes
        select(Name, Age, Position, Wage, `Agreed Playing Time`, Mins, Gls, Ast) %>%
        # games played = minutes/90
        mutate(`Games Played` = round(`Mins`/90, digits = 1) %>% replace_na(0), .keep = "unused") %>%
        # subset loaned out categories by age
        mutate(`Agreed Playing Time` = if_else(
          condition = is.na(`Agreed Playing Time`),
          true = case_when(
            `Age` <= 18 ~ "Loaned out, Age\u226418",
            between(`Age`,19,22) ~ "Loaned out, 19\u2264Age\u226422",
            `Age`>= 23 ~ "Loaned out, Age\u226523"
          ),
          false = `Agreed Playing Time`
        )) %>%
        # number of occurrences of each status
        mutate(freq = table(`Agreed Playing Time`)[`Agreed Playing Time`]) %>%
        # add frequency to status
        mutate(`Agreed Playing Time` = paste0(`Agreed Playing Time`, " (", freq, ")")) %>%
        # left join to add the categories
        left_join(y = apt_category, by = c("Agreed Playing Time" = "apt")) %>%
        # add loaned out category
        mutate(cat = replace_na(cat, "Loaned Players")),
      x = ~`Agreed Playing Time`, y = ~`Games Played`,
      type = "box", boxpoints = "all",
      color = ~cat,
      colors = c(
        "Senior Squad" = "purple", # squad
        "Youth Players" = "deepskyblue2", # youth
        "Goalkeepers" = "green3", # gk
        "Loaned Players" = "orange" # loaned
      ),
      # adds text when marker is hovered on
      hoverinfo = "text",
      # define text logic
      hovertext = ~paste0(
        "<b>Name: ", Name,
        "<br>Age: </b>", Age,
        "<br><b>Position: </b>", Position,
        "<br><b>Wage: </b> \u20AC ", formatC(Wage, format="f", big.mark = ",", digits=0), " p/w",
        "<br><b>Games played: </b>", replace_na(`Games Played`, 0),
        "<br><b>Goals: </b>", replace_na(Gls, 0),
        "<br><b>Assists: </b>", replace_na(Ast, 0)
      )
    ) %>%
      layout(
        xaxis = list(
          # no title axis
          title = "",
          # order x axis
          categoryorder = "array", categoryarray = apt_category$apt
        ),
        # y axis title
        yaxis = list(title = "Games Played")
      )
  })
  
  # playing time distribution plot
  output$home_plot_wage <- renderPlotly({
    # dataframe for player status (agreed playing time) and status' category
    apt_category <- data.frame(apt = c(
      "Star Player", "Important Player", "Regular Starter",
      "Squad Player", "Impact Sub", "Fringe Player",
      "Emergency Backup", "B Team Regular", "Surplus to Requirements",
      "Breakthrough Prospect", "Future Prospect", "Youngster",
      "First-Choice Goalkeeper", "Cup Goalkeeper", "Backup"
    ), cat = c(rep("Senior Squad", 9), rep("Youth Players", 3), rep("Goalkeepers", 3))) %>%
      # add number of occurrences for each player status
      mutate(
        apt = paste0(
          apt, " (", table((squads[[squads_latest_date]] %>% dplyr::filter(is.na(`Loan Expires`)))$`Agreed Playing Time`)[apt], ")"
        ) %>%
          # replace NA by 0 in the occurrences
          str_replace_all("NA", "0")
      )
    
    plot_ly(
      # wage data
      data = squads[[squads_latest_date]] %>%
        # required data points
        select(Name, Age, Position, `Agreed Playing Time`, Wage, `Loan Expires`, Mins, Gls, Ast) %>%
        # remove loaned in and loaned out players
        dplyr::filter(is.na(`Loan Expires`)) %>%
        # remove loan expiration column
        select(-`Loan Expires`) %>%
        # number of occurrences of each status
        mutate(freq = table(`Agreed Playing Time`)[`Agreed Playing Time`]) %>%
        # add frequency to status
        mutate(`Agreed Playing Time` = paste0(`Agreed Playing Time`, " (", freq, ")")) %>%
        # left join to add the categories
        left_join(y = apt_category, by = c("Agreed Playing Time" = "apt")),
      x = ~`Agreed Playing Time`, y = ~Wage,
      type = "box", boxpoints = "all",
      color = ~cat,
      colors = c(
        "Senior Squad" = "purple", # squad
        "Youth Players" = "deepskyblue2", # youth
        "Goalkeepers" = "green3" # gk
      ),
      # adds text when marker is hovered on
      hoverinfo = "text",
      # define text logic
      hovertext = ~paste0(
        "<b>Name: ", Name,
        "<br>Age: </b>", Age,
        "<br><b>Position: </b>", Position,
        "<br><b>Wage: </b> \u20AC ", formatC(Wage, format="f", big.mark = ",", digits=0), " p/w",
        "<br><b>Games played: </b>", round(replace_na(Mins, 0)/90, digits = 1),
        "<br><b>Goals: </b>", replace_na(Gls, 0),
        "<br><b>Assists: </b>", replace_na(Ast, 0)
      )
    ) %>%
      layout(
        xaxis = list(
          # no title axis
          title = "",
          # order x axis
          categoryorder = "array", categoryarray = apt_category$apt
        ),
        # y axis title
        yaxis = list(title = "Wage (p/w)")
      )
  })
  
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
  
  # calculate the DNA ratings
  # initial empty reactive list
  squads_DNArated <- reactiveValues()
  # calculate each date's DNA ratings
  for (i in squads_dates) {
    squads_DNArated[[i]] <- reactive({
      if (length(c(input$sqmodel_technical, input$sqmodel_mental, input$sqmodel_physical, input$sqmodel_gk)) == 0) {
        squads[[i]] %>% mutate(`Rec` = -1)
      } else {
        # key outfield attributes
        outf_keyattr <- c(input$sqmodel_technical, input$sqmodel_mental, input$sqmodel_physical)
        outf_n <- length(outf_keyattr)
        # key gk attributes
        gk_keyattr <- c(input$sqmodel_mental, input$sqmodel_physical, input$sqmodel_gk)
        gk_n <- length(gk_keyattr)
        # subset data: outfield players
        outf_players <- squads[[i]] %>%
          dplyr::select(Name, Position, all_of(outf_keyattr)) %>%
          dplyr::filter(Position != "GK")
        # subset data: goalkeepers
        gk_players <- squads[[i]] %>%
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
        squads[[i]] %>% dplyr::select(-Rec) %>%
          inner_join(y = ratings, by = c("Name" = "Name", "Position" = "Position"))
      }
    })
  }
  
  #### Squad: Filters ####
  
  # filtered subset data for DT table presentation
  squad_filtsubset <- reactive({
    squads_DNArated[[squads_latest_date]]() %>% as.data.frame() %>%
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
        `Games Played` = round(Mins/90, digits = 1), .keep = "unused", .after = Mins
      ) %>%
      drop_na()
  })
  
  # filtered subset DT table
  output$squad_filtered <- DT::renderDT({
    tabulate_filtdata(data = squad_filtsubset(), dataset = "squad")
  })
  
  # filtered subset names: used as an index
  squad_index <- reactive({
    squad_filtsubset() %>% as.data.frame() %>% dplyr::select(Name)# %>% as.vector()
  })
  
  # filtered subset DT table for plots
  squad_data <- reactive({
    subsetdata(
      data = squads_DNArated[[squads_latest_date]]() %>% as.data.frame(),
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
  
  #### Squad: Progress ####
  
  # stats progress
  output$squad_progperf <- DT::renderDT({
    tabulate_progress(squads_DNArated, input$squad_startdate, input$squad_enddate, "stats")
  })
  # attributes progress
  output$squad_progattr <- DT::renderDT({
    tabulate_progress(squads_DNArated, input$squad_startdate, input$squad_enddate, "attr")
  })
  
  #### Scouting: Filters ####
  
  # filtered subset data for DT table presentation
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
        `Games Played` = round(Mins/90, digits = 1), .keep = "unused", .after = Mins
      ) %>%
      drop_na()
  })
  
  # filtered subset DT table
  output$scout_filtered <- DT::renderDT({
    tabulate_filtdata(data = scout_filtsubset(), dataset = "scouting")
  })
  
  # filtered subset names: used as an index
  scout_index <- reactive({
    scout_filtsubset() %>% as.data.frame() %>% dplyr::select(Name)# %>% as.vector()
  })
  
  # filtered subset DT table for plots
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
  output$scout_listfinal <- DT::renderDataTable({
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
      select(
        Name, Rec, Position, Age,
        `Min AP`, `Max AP`, `Min WD`, `Max WD`,
        `Min Fee Rls`,
        `Injury Rls`, `Min Fee Rls Clubs In Cont Comp`, `Min Fee Rls Clubs Mjr Cont Comp`,
        `Min Fee Rls to Higher Div`, `Min Fee Rls to Domestic Clubs`, `Min Fee Rls to Foreign Clubs`,
        `Non Prom Rls Cls`, `Non-Playing Rel`, `Relegation Release`
      ) %>%
      mutate(Rls = if_else(is.na(`Min Fee Rls`), "No", "Yes"), .keep = "unused") %>%
      mutate(CondRls = if_else(
        is.na(`Injury Rls`) &&
          is.na(`Min Fee Rls Clubs In Cont Comp`) &&
          is.na(`Min Fee Rls Clubs Mjr Cont Comp`) &&
          is.na(`Min Fee Rls to Higher Div`) &&
          is.na(`Min Fee Rls to Domestic Clubs`) &&
          is.na(`Min Fee Rls to Foreign Clubs`) &&
          is.na(`Non Prom Rls Cls`) && 
          is.na(`Non-Playing Rel`) &&
          is.na(`Relegation Release`),
        "No", "Yes"), .keep = "unused"
      ) %>%
      dplyr::filter(Name %in% listfinal)
    
    # DT table
    DT::datatable(
      data = listfinal_data,
      # no row names displayed
      rownames = FALSE,
      # replace Rec column name by appropriate name: dataset
      # add "p/w" to wage column name
      colnames = c(
        "Scouting Rec" = "Rec",
        "Min WD (p/w)" = "Min WD", "Max WD (p/w)" = "Max WD",
        "Rls Cls?" = "Rls", "Conditional Rls Cls?" = "CondRls"
      ),
      # options
      options = list(
        # no paging, scroll instead
        paging = FALSE,
        # order
        order = list(
          list(1, "desc"), # rec
          list(5, "desc"), # max value
          list(4, "desc"), # min value
          list(7, "desc"), # max wage
          list(6, "desc"), # min wage
          list(3, "desc") # age
        ),
        # horizontal scrolling enabled
        scrollX = TRUE,
        # vertical scrolling enabled
        scrollY = "500px"
      )
    ) %>%
    # format font color and cell background depending on data
    formatStyle(
      # format all row based on Rec's value
      2, target = "row",
      # original colors
      # c("#bf3eff", "#ff0000", "#ffa500", "#ffff00", "#00ee00", "#008b00")
      backgroundColor = styleInterval(
        c(-1, 49, 59, 69, 84),
        c("#e5b1ff", "#ff9999", "#ffdb99", "#ffff99", "#92ff92", "#20ff20")
      ),
      color = styleInterval(
        c(-1, 49, 59, 69, 84),
        c("#6c00a2", "#990000", "#996300", "#666600", "#008100", "#004000")
      )
    ) %>%
    # format Value and Wage as currency
    formatCurrency(
      columns = 5:8,
      currency = "\u20ac ",
      interval = 3, mark = ",",
      digits = 2, dec.mark = ".",
      before = TRUE
    )
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
