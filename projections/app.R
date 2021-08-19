### Ballblast Projections Apps

library(shiny)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
#library(nflfastR)
library(RPostgres)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
options(stringsAsFactors = FALSE)
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "footballdb",
                 host = 'footballdb.cq0hesolzrqv.us-west-2.rds.amazonaws.com',
                 port = 5432,
                 user = "footballdb", 
                 password = 'f00tb4ll!'
                 #engine = 'django.contrib.gis.db.backends.postgis'
)

final_df <- dbGetQuery(con, 'select * from ballblast')
all_teams <- c("Arizona Cardinals","Atlanta Falcons","Baltimore Ravens","Buffalo Bills","Carolina Panthers","Chicago Bears",
              "Cincinnati Bengals","Cleveland Browns","Dallas Cowboys","Denver Broncos","Detroit Lions","Green Bay Packers",
              "Houston Texans","Indianapolis Colts","Jacksonville Jaguars","Kansas City Chiefs","LA Chargers","LA Rams",
              "Las Vegas Raiders","Miami Dolphins","Minnesota Vikings","New England Patriots","New Orleans Saints",
              "New York Giants","New York Jets","Philadelphia Eagles","Pittsburgh Steelers","Seattle Seahawks",
              "San Francisco 49ers","Tampa Bay Buccaneers","Tennessee Titans","Washington Football Team")

qb_print <- function(df, ranker_select){
    df %>% filter(ranker == ranker_select, Position == "QB", !grepl("QB2", Player), !grepl("QB3", Player), !grepl("QB4", Player),
                  `Fantasy Points` > 0) %>%
        select("headshot_url","Player","Team","Pass Attempts","Completions","Pass Yards","Pass TD","Int","Completion%",
               "Pass TD%","YPA","Rush Attempts","Rushing Yards","Projected Rushing TDs","Fantasy Points","Positional Rank") %>%
        arrange(-`Fantasy Points`) %>% 
        gt(rowname_col = "Positional Rank") %>%
        tab_header(title = paste0('BallBlast Quarterback Rankings - ', ranker_select)) %>% 
        tab_stubhead(label = "Rank") %>%
        text_transform(
            locations = cells_body(vars(Team)),
            fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
        ) %>%
        text_transform(
            locations = cells_body(columns = vars(headshot_url)),
            fn = function(x){
                gt::web_image(x)
            }
        ) %>%
        cols_label(
            headshot_url = 'Player',
            Player = '',
            Team = '',
            `Pass Attempts` = 'Attempts',
            Completions = 'Comp',
            `Pass Yards` = 'Yards',
            `Pass TD` = 'TD',
            Int = 'INT',
            `Completion%` = 'Comp%',
            `Pass TD%` = 'TD%',
            `Rush Attempts` = 'Attempts',
            `Rushing Yards` = 'Yards',
            `Projected Rushing TDs` = 'TD',
            `Fantasy Points` = 'FanPts',
            `Positional Rank` = 'Rank'
        ) %>%
        tab_style(style = cell_text(font = google_font(name = "Questrial"),
                                    weight = "bold"),
                  locations = cells_title()) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Team,`Pass Attempts`,`Completions`,`Pass Yards`,`Pass TD`,`Int`,`Rush Attempts`,
                                              `Rushing Yards`,`Projected Rushing TDs`,`YPA`,`Fantasy Points`,`Completion%`,`Pass TD%`))) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_stub()) %>%
        tab_style(style = cell_borders(sides = c("bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Player,headshot_url))) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(vars(Player))) %>%
        tab_style(style = cell_text(size = "14px"), locations = cells_stub()) %>%
        fmt_number(columns = vars(`Pass Attempts`, `Completions`, `Pass Yards`, `Pass TD`, `Int`, `Rush Attempts`, `Rushing Yards`, `Projected Rushing TDs`), 
                   decimals = 0) %>%
        fmt_number(columns = vars(`YPA`,`Fantasy Points`), decimals = 1) %>%
        fmt_percent(columns = vars(`Completion%`,`Pass TD%`), decimals = 1) %>%
        tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
        tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(Player))) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_column_labels(vars(Player))) %>% 
        tab_spanner(label = 'Passing Stats', 
                    columns = vars(`Pass Attempts`, `Completions`, `Pass Yards`, `Pass TD`, `Int`, `Completion%`,`Pass TD%`, YPA)) %>% 
        tab_spanner(label = 'Rushing Stats', columns = vars(`Rush Attempts`, `Rushing Yards`, `Projected Rushing TDs`)) %>% 
        tab_source_note(source_note = '') %>%
        data_color(
            columns = 4,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass Attempts`),max(df$`Pass Attempts`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 5,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Completions`),max(df$`Completions`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 6,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass Yards`),max(df$`Pass Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 7,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass TD`),max(df$`Pass TD`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 8,
            colors = scales::col_numeric(palette = rev(RColorBrewer::brewer.pal(n = 5, name = "RdYlGn")),
                                         domain = rev(c(min(df$`Int`),max(df$`Int`)))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 9,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Completion%`),max(df$`Completion%`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 10,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass TD%`),max(df$`Pass TD%`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 11,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`YPA`),max(df$`YPA`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 12,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rush Attempts`),max(df$`Rush Attempts`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 13,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rushing Yards`),max(df$`Rushing Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 14,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Rushing TDs`),max(df$`Projected Rushing TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 15,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Fantasy Points`),max(df$`Fantasy Points`))),
            autocolor_text = FALSE
        ) %>%
        cols_width(vars(Player) ~ px(175), vars(`Positional Rank`) ~ px(64)) %>% 
        tab_options(
            table.width = px(1200),
            column_labels.background.color = "black",
            heading.background.color = "black",
            table.font.color = "black",
            data_row.padding = '5px',
            row_group.padding = '3px',
            heading.border.bottom.color = 'white',
            column_labels.border.bottom.color = 'black',
            column_labels.border.top.color = "white",
            column_labels.border.bottom.width = 1.4,
            column_labels.font.weight = "bold",
            column_labels.border.lr.color = "white",
            table_body.border.top.color = 'black',
            table_body.border.bottom.width = 0.7,
            table_body.border.bottom.color = 'black',
            row_group.border.bottom.width = 1,
            row_group.border.bottom.color = 'black',
            row_group.border.top.width = 1.5,
            row_group.border.top.color = 'black',
            table.border.top.color = 'black',
            table.background.color = '#F2F2F2',
            table.border.bottom.color = 'black',
            table.border.left.color = "black",
            table.border.right.color = "black",
            row.striping.background_color = '#FFFFFF',
            row.striping.include_table_body = TRUE,
            row.striping.include_stub = TRUE,
            footnotes.border.bottom.color = "black",
            stub.border.color = "black",
            stub.font.weight = "bold",
            table.font.size = "12px",
            footnotes.border.lr.color = "black",
            source_notes.border.bottom.color = "black"
        )
}
rb_print <- function(df, ranker_select){
    df %>% filter(ranker == ranker_select, Position == "RB", !grepl("RB2", Player), !grepl("RB3", Player), !grepl("RB4", Player),
                  !grepl("RB5", Player), !grepl("RB6", Player), !grepl("RB7", Player), !grepl("RB8", Player), `Fantasy Points` > 0) %>%
        select("headshot_url","Player","Team","Rush Attempts","Rushing Yards","YPC","Projected Rushing TDs",
               "Targets","Receptions","Receiving Yards","Projected Receiving TDs","Target Share","Fantasy Points","Positional Rank") %>%
        arrange(-`Fantasy Points`) %>% 
        gt(rowname_col = "Positional Rank") %>%
        tab_header(title = paste0('BallBlast Running Back Rankings - ', ranker_select)) %>% 
        tab_stubhead(label = "Rank") %>%
        text_transform(
            locations = cells_body(vars(Team)),
            fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
        ) %>%
        text_transform(
            locations = cells_body(columns = vars(headshot_url)),
            fn = function(x){
                gt::web_image(x)
            }
        ) %>%
        cols_label(
            headshot_url = 'Player',
            Player = '',
            Team = '',
            `Rush Attempts` = 'Attempts',
            `Rushing Yards` = 'Yards',
            `Projected Rushing TDs` = 'TD',
            `Receiving Yards` = 'Yards',
            `Projected Receiving TDs` = 'TD',
            `Fantasy Points` = 'FanPts',
            `Positional Rank` = 'Rank'
        ) %>%
        tab_style(style = cell_text(font = google_font(name = "Questrial"),
                                    weight = "bold"),
                  locations = cells_title()) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Team,`Rush Attempts`,`Rushing Yards`,`Projected Rushing TDs`,YPC,Targets,Receptions,
                                              `Receiving Yards`,`Projected Receiving TDs`,`Target Share`,`Fantasy Points`))) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_stub()) %>%
        tab_style(style = cell_borders(sides = c("bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Player,headshot_url))) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(vars(Player))) %>%
        tab_style(style = cell_text(size = "14px"), locations = cells_stub()) %>%
        fmt_number(columns = vars(`Rush Attempts`,`Rushing Yards`,`Projected Rushing TDs`,Targets, Receptions,`Receiving Yards`,`Projected Receiving TDs`), 
                   decimals = 0) %>%
        fmt_number(columns = vars(YPC,`Fantasy Points`), decimals = 1) %>%
        fmt_percent(columns = vars(`Target Share`), decimals = 1) %>%
        tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
        tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(Player))) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_column_labels(vars(Player))) %>% 
        tab_spanner(label = 'Rushing Stats', columns = vars(`Rush Attempts`, `Rushing Yards`, YPC, `Projected Rushing TDs`)) %>% 
        tab_spanner(label = 'Receiving Stats', 
                    columns = vars(Targets, Receptions,`Receiving Yards`,`Projected Receiving TDs`,`Target Share`)) %>% 
        tab_source_note(source_note = '') %>%
        data_color(
            columns = 4,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rush Attempts`),max(df$`Rush Attempts`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 5,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rushing Yards`),max(df$`Rushing Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 6,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$YPC),max(df$YPC))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 7,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Rushing TDs`),max(df$`Projected Rushing TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 8,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Targets),max(df$Targets))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 9,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Receptions),max(df$Receptions))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 10,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Receiving Yards`),max(df$`Receiving Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 11,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Receiving TDs`),max(df$`Projected Receiving TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 12,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Target Share`),max(df$`Target Share`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 13,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Fantasy Points`),max(df$`Fantasy Points`))),
            autocolor_text = FALSE
        ) %>%
        cols_width(vars(Player) ~ px(175), vars(`Positional Rank`) ~ px(64)) %>% 
        tab_options(
            table.width = px(1200),
            table.font.color = "black",
            data_row.padding = '5px',
            row_group.padding = '3px',
            heading.border.bottom.color = 'black',
            column_labels.border.bottom.color = 'black',
            column_labels.border.bottom.width = 1.4,
            column_labels.font.weight = "bold",
            table_body.border.top.color = 'black',
            table_body.border.bottom.width = 0.7,
            table_body.border.bottom.color = 'black',
            row_group.border.bottom.width = 1,
            row_group.border.bottom.color = 'black',
            row_group.border.top.width = 1.5,
            row_group.border.top.color = 'black',
            table.border.top.color = 'black',
            table.background.color = '#F2F2F2',
            table.border.bottom.color = 'black',
            table.border.left.color = "black",
            table.border.right.color = "black",
            row.striping.background_color = '#FFFFFF',
            row.striping.include_table_body = TRUE,
            row.striping.include_stub = TRUE,
            footnotes.border.bottom.color = "black",
            stub.border.color = "black",
            stub.font.weight = "bold",
            table.font.size = "12px",
            footnotes.border.lr.color = "black",
            source_notes.border.bottom.color = "black"
        )
}
wr_print <- function(df, ranker_select){
    df %>% filter(ranker == ranker_select, Position == "WR", !grepl("WR2", Player), !grepl("WR3", Player), !grepl("WR4", Player),
                  !grepl("WR5", Player), !grepl("WR6", Player), !grepl("WR7", Player), !grepl("WR8", Player), `Fantasy Points` > 0) %>%
        select("headshot_url","Player","Team","Targets","Receptions","Receiving Yards","Projected Receiving TDs","Target Share",
               "Rush Attempts","Rushing Yards","YPC","Projected Rushing TDs","Fantasy Points","Positional Rank") %>%
        arrange(-`Fantasy Points`) %>% 
        gt(rowname_col = "Positional Rank") %>%
        tab_header(title = paste0('BallBlast Wide Receiver Rankings - ', ranker_select)) %>% 
        tab_stubhead(label = "Rank") %>%
        text_transform(
            locations = cells_body(vars(Team)),
            fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
        ) %>%
        text_transform(
            locations = cells_body(columns = vars(headshot_url)),
            fn = function(x){
                gt::web_image(x)
            }
        ) %>%
        cols_label(
            headshot_url = 'Player',
            Player = '',
            Team = '',
            `Rush Attempts` = 'Attempts',
            `Rushing Yards` = 'Yards',
            `Projected Rushing TDs` = 'TD',
            `Receiving Yards` = 'Yards',
            `Projected Receiving TDs` = 'TD',
            `Fantasy Points` = 'FanPts',
            `Positional Rank` = 'Rank'
        ) %>%
        tab_style(style = cell_text(font = google_font(name = "Questrial"),
                                    weight = "bold"),
                  locations = cells_title()) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Team,`Rush Attempts`,`Rushing Yards`,`Projected Rushing TDs`,YPC,Targets,Receptions,
                                              `Receiving Yards`,`Projected Receiving TDs`,`Target Share`,`Fantasy Points`))) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_stub()) %>%
        tab_style(style = cell_borders(sides = c("bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Player,headshot_url))) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(vars(Player))) %>%
        tab_style(style = cell_text(size = "14px"), locations = cells_stub()) %>%
        fmt_number(columns = vars(`Rush Attempts`,`Rushing Yards`,`Projected Rushing TDs`,Targets, Receptions,`Receiving Yards`,`Projected Receiving TDs`), 
                   decimals = 0) %>%
        fmt_number(columns = vars(YPC,`Fantasy Points`), decimals = 1) %>%
        fmt_percent(columns = vars(`Target Share`), decimals = 1) %>%
        tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
        tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(Player))) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_column_labels(vars(Player))) %>% 
        tab_spanner(label = 'Rushing Stats', columns = vars(`Rush Attempts`, `Rushing Yards`, YPC, `Projected Rushing TDs`)) %>% 
        tab_spanner(label = 'Receiving Stats', 
                    columns = vars(Targets, Receptions,`Receiving Yards`,`Projected Receiving TDs`,`Target Share`)) %>% 
        tab_source_note(source_note = '') %>%
        data_color(
            columns = 4,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Targets),max(df$Targets))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 5,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Receptions),max(df$Receptions))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 6,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Receiving Yards`),max(df$`Receiving Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 7,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Receiving TDs`),max(df$`Projected Receiving TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 8,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Target Share`),max(df$`Target Share`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 9,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rush Attempts`),max(df$`Rush Attempts`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 10,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rushing Yards`),max(df$`Rushing Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 11,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$YPC),max(df$YPC))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 12,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Rushing TDs`),max(df$`Projected Rushing TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 13,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Fantasy Points`),max(df$`Fantasy Points`))),
            autocolor_text = FALSE
        ) %>%
        cols_width(vars(Player) ~ px(175), vars(`Positional Rank`) ~ px(64)) %>% 
        tab_options(
            table.width = px(1200),
            table.font.color = "black",
            data_row.padding = '5px',
            row_group.padding = '3px',
            heading.border.bottom.color = 'black',
            column_labels.border.bottom.color = 'black',
            column_labels.border.bottom.width = 1.4,
            column_labels.font.weight = "bold",
            table_body.border.top.color = 'black',
            table_body.border.bottom.width = 0.7,
            table_body.border.bottom.color = 'black',
            row_group.border.bottom.width = 1,
            row_group.border.bottom.color = 'black',
            row_group.border.top.width = 1.5,
            row_group.border.top.color = 'black',
            table.border.top.color = 'black',
            table.background.color = '#F2F2F2',
            table.border.bottom.color = 'black',
            table.border.left.color = "black",
            table.border.right.color = "black",
            row.striping.background_color = '#FFFFFF',
            row.striping.include_table_body = TRUE,
            row.striping.include_stub = TRUE,
            footnotes.border.bottom.color = "black",
            stub.border.color = "black",
            stub.font.weight = "bold",
            table.font.size = "12px",
            footnotes.border.lr.color = "black",
            source_notes.border.bottom.color = "black"
        )
}
te_print <- function(df, ranker_select){
    df %>% filter(ranker == ranker_select, Position == "TE", !grepl("TE2", Player), !grepl("TE3", Player), !grepl("TE4", Player),
                  !grepl("TE5", Player), !grepl("TE6", Player), !grepl("TE7", Player), !grepl("TE8", Player), `Fantasy Points` > 0) %>%
        select("headshot_url","Player","Team","Targets","Receptions","Receiving Yards","Projected Receiving TDs","Target Share",
               "Fantasy Points","Positional Rank") %>%
        arrange(-`Fantasy Points`) %>% 
        gt(rowname_col = "Positional Rank") %>%
        tab_header(title = paste0('BallBlast Tight End Rankings - ', ranker_select)) %>% 
        tab_stubhead(label = "Rank") %>%
        text_transform(
            locations = cells_body(vars(Team)),
            fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
        ) %>%
        text_transform(
            locations = cells_body(columns = vars(headshot_url)),
            fn = function(x){
                gt::web_image(x)
            }
        ) %>%
        cols_label(
            headshot_url = 'Player',
            Player = '',
            Team = '',
            `Receiving Yards` = 'Yards',
            `Projected Receiving TDs` = 'TD',
            `Fantasy Points` = 'FanPts',
            `Positional Rank` = 'Rank'
        ) %>%
        tab_style(style = cell_text(font = google_font(name = "Questrial"),
                                    weight = "bold"),
                  locations = cells_title()) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Team,Targets,Receptions,`Receiving Yards`,`Projected Receiving TDs`,
                                              `Target Share`,`Fantasy Points`))) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_stub()) %>%
        tab_style(style = cell_borders(sides = c("bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Player,headshot_url))) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(vars(Player))) %>%
        tab_style(style = cell_text(size = "14px"), locations = cells_stub()) %>%
        fmt_number(columns = vars(Targets, Receptions,`Receiving Yards`,`Projected Receiving TDs`), 
                   decimals = 0) %>%
        fmt_number(columns = vars(`Fantasy Points`), decimals = 1) %>%
        fmt_percent(columns = vars(`Target Share`), decimals = 1) %>%
        tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
        tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(Player))) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_column_labels(vars(Player))) %>% 
        tab_spanner(label = 'Receiving Stats', 
                    columns = vars(Targets, Receptions,`Receiving Yards`,`Projected Receiving TDs`,`Target Share`)) %>% 
        tab_source_note(source_note = '') %>%
        data_color(
            columns = 4,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Targets),max(df$Targets))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 5,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Receptions),max(df$Receptions))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 6,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Receiving Yards`),max(df$`Receiving Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 7,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Receiving TDs`),max(df$`Projected Receiving TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 8,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Target Share`),max(df$`Target Share`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 9,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Fantasy Points`),max(df$`Fantasy Points`))),
            autocolor_text = FALSE
        ) %>%
        cols_width(vars(Player) ~ px(175), vars(`Positional Rank`) ~ px(64)) %>% 
        tab_options(
            table.width = px(1200),
            table.font.color = "black",
            data_row.padding = '5px',
            row_group.padding = '3px',
            heading.border.bottom.color = 'black',
            column_labels.border.bottom.color = 'black',
            column_labels.border.bottom.width = 1.4,
            column_labels.font.weight = "bold",
            table_body.border.top.color = 'black',
            table_body.border.bottom.width = 0.7,
            table_body.border.bottom.color = 'black',
            row_group.border.bottom.width = 1,
            row_group.border.bottom.color = 'black',
            row_group.border.top.width = 1.5,
            row_group.border.top.color = 'black',
            table.border.top.color = 'black',
            table.background.color = '#F2F2F2',
            table.border.bottom.color = 'black',
            table.border.left.color = "black",
            table.border.right.color = "black",
            row.striping.background_color = '#FFFFFF',
            row.striping.include_table_body = TRUE,
            row.striping.include_stub = TRUE,
            footnotes.border.bottom.color = "black",
            stub.border.color = "black",
            stub.font.weight = "bold",
            table.font.size = "12px",
            footnotes.border.lr.color = "black",
            source_notes.border.bottom.color = "black"
        )
}
team_print <- function(df, team, ranker_select){
    teaminitials <- sort(unique(final_df$Team))
    teamlong <- c("Arizona Cardinals","Atlanta Falcons","Baltimore Ravens","Buffalo Bills","Carolina Panthers","Chicago Bears",
                  "Cincinnati Bengals","Cleveland Browns","Dallas Cowboys","Denver Broncos","Detroit Lions","Green Bay Packers",
                  "Houston Texans","Indianapolis Colts","Jacksonville Jaguars","Kansas City Chiefs","LA Chargers","LA Rams",
                  "Las Vegas Raiders","Miami Dolphins","Minnesota Vikings","New England Patriots","New Orleans Saints",
                  "New York Giants","New York Jets","Philadelphia Eagles","Pittsburgh Steelers","Seattle Seahawks",
                  "San Francisco 49ers","Tampa Bay Buccaneers","Tennessee Titans","Washington Football Team")
    teamselect <- team
    teamselectinitials <- teaminitials[grep(teamselect,teamlong)]
    
    df <- final_df %>% filter(Team == teamselectinitials, ranker == ranker_select, `Fantasy Points` > 0,
                              !grepl("TE2", Player), !grepl("TE3", Player), !grepl("TE4", Player),
                              !grepl("TE5", Player), !grepl("TE6", Player), !grepl("TE7", Player), !grepl("TE8", Player), 
                              !grepl("WR2", Player), !grepl("WR3", Player), !grepl("WR4", Player),
                              !grepl("WR5", Player), !grepl("WR6", Player), !grepl("WR7", Player), !grepl("WR8", Player), 
                              !grepl("RB2", Player), !grepl("RB3", Player), !grepl("RB4", Player),
                              !grepl("RB5", Player), !grepl("RB6", Player), !grepl("RB7", Player), !grepl("RB8", Player), 
                              !grepl("QB2", Player), !grepl("QB3", Player), !grepl("QB4", Player))
    
    df %>% arrange(match(Position, c('QB','RB','WR','TE')),-`Pass Attempts`,-`Rush Attempts`,-`Targets`) %>%
        select("headshot_url","Player","Team","Pass Attempts","Completions","Pass Yards","Pass TD","Int","Completion%",
               "Pass TD%","Rush Attempts","Rushing Yards","Projected Rushing TDs",
               "Targets","Receptions","Receiving Yards","Projected Receiving TDs","Target Share","Fantasy Points","Positional Rank") %>%
        #arrange(-`Fantasy Points`) %>% 
        gt(rowname_col = "Positional Rank") %>%
        tab_header(title = paste0('BallBlast ',teamselect,' Projections - ', ranker_select)) %>% 
        tab_stubhead(label = "Rank") %>%
        text_transform(
            locations = cells_body(vars(Team)),
            fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
        ) %>%
        text_transform(
            locations = cells_body(columns = vars(headshot_url)),
            fn = function(x){
                gt::web_image(x)
            }
        ) %>%
        cols_label(
            headshot_url = 'Player',
            Player = '',
            Team = '',
            `Pass Attempts` = 'Att',
            Completions = 'Comp',
            `Pass Yards` = 'Yards',
            `Pass TD` = 'TD',
            Int = 'INT',
            `Completion%` = 'Comp%',
            `Pass TD%` = 'TD%',
            `Rush Attempts` = 'Att',
            `Rushing Yards` = 'Yards',
            `Projected Rushing TDs` = 'TD',
            Receptions = "Rec",
            `Receiving Yards` = 'Yards',
            `Projected Receiving TDs` = 'TD',
            `Fantasy Points` = 'FanPts',
            `Positional Rank` = 'Rank'
        ) %>%
        tab_style(style = cell_text(font = google_font(name = "Questrial"),
                                    weight = "bold"),
                  locations = cells_title()) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Team,`Pass Attempts`,`Completions`,`Pass Yards`,`Pass TD`,`Int`,`Rush Attempts`,
                                              `Rushing Yards`,`Projected Rushing TDs`,
                                              Targets,Receptions,`Receiving Yards`,`Projected Receiving TDs`,`Target Share`,
                                              `Fantasy Points`,`Completion%`,`Pass TD%`))) %>%
        tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_stub()) %>%
        tab_style(style = cell_borders(sides = c("bottom"), color = "black", weight = px(2), style = "solid"),
                  locations = cells_body(vars(Player,headshot_url))) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(vars(Player))) %>%
        tab_style(style = cell_text(size = "14px"), locations = cells_stub()) %>%
        fmt_number(columns = vars(`Pass Attempts`, `Completions`, `Pass Yards`, `Pass TD`, `Int`, 
                                  Targets,Receptions,`Receiving Yards`,`Projected Receiving TDs`,
                                  `Rush Attempts`, `Rushing Yards`, `Projected Rushing TDs`), 
                   decimals = 0) %>%
        fmt_number(columns = vars(`Fantasy Points`), decimals = 1) %>%
        fmt_percent(columns = vars(`Completion%`,`Pass TD%`,`Target Share`), decimals = 1) %>%
        fmt_missing(columns = 4:19, missing_text = "") %>%
        tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
        tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(Player))) %>% 
        tab_style(style = cell_text(align = 'left'), locations = cells_column_labels(vars(Player))) %>% 
        tab_spanner(label = 'Passing Stats', 
                    columns = vars(`Pass Attempts`, `Completions`, `Pass Yards`, `Pass TD`, `Int`, `Completion%`,`Pass TD%`)) %>% 
        tab_spanner(label = 'Rushing Stats', columns = vars(`Rush Attempts`, `Rushing Yards`, `Projected Rushing TDs`)) %>% 
        tab_spanner(label = 'Receiving Stats', 
                    columns = vars(Targets, Receptions,`Receiving Yards`,`Projected Receiving TDs`,`Target Share`)) %>% 
        tab_source_note(source_note = '') %>%
        tab_row_group(
            group = "Tight End",
            rows = which(grepl("TE", df$Position))
        ) %>%
        tab_row_group(
            group = "Wide Receiver",
            rows = which(grepl("WR", df$Position))
        ) %>%
        tab_row_group(
            group = "Running Back",
            rows = which(grepl("RB", df$Position))
        ) %>%
        tab_row_group(
            group = "Quarterback",
            rows = which(grepl("QB", df$Position))
        ) %>%
        tab_style(style = cell_fill(color = "lightblue"), locations = cells_row_groups()) %>%
        data_color(
            columns = 4,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass Attempts`),max(df$`Pass Attempts`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 5,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Completions`),max(df$`Completions`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 6,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass Yards`),max(df$`Pass Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 7,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass TD`),max(df$`Pass TD`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 8,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Int`),max(df$`Int`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 9,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Completion%`),max(df$`Completion%`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 10,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Pass TD%`),max(df$`Pass TD%`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 11,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rush Attempts`),max(df$`Rush Attempts`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 12,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Rushing Yards`),max(df$`Rushing Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 13,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Rushing TDs`),max(df$`Projected Rushing TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 14,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Targets),max(df$Targets))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 15,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$Receptions),max(df$Receptions))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 16,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Receiving Yards`),max(df$`Receiving Yards`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 17,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Projected Receiving TDs`),max(df$`Projected Receiving TDs`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 18,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Target Share`),max(df$`Target Share`))),
            autocolor_text = FALSE
        ) %>%
        data_color(
            columns = 19,
            colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                         domain = c(min(df$`Fantasy Points`),max(df$`Fantasy Points`))),
            autocolor_text = FALSE
        ) %>%
        cols_width(vars(Player) ~ px(175),
                   vars(`Positional Rank`) ~ px(64)) %>% 
        tab_options(
            table.width = px(1200),
            table.font.color = "black",
            data_row.padding = '5px',
            row_group.padding = '3px',
            heading.border.bottom.color = 'black',
            column_labels.border.bottom.color = 'black',
            column_labels.border.bottom.width = 1.4,
            column_labels.font.weight = "bold",
            table_body.border.top.color = 'black',
            table_body.border.bottom.width = 0.7,
            table_body.border.bottom.color = 'black',
            row_group.border.bottom.width = 1,
            row_group.border.bottom.color = 'black',
            row_group.border.top.width = 1.5,
            row_group.border.top.color = 'black',
            row_group.font.size = "14px",
            row_group.font.weight = "bold",
            table.border.top.color = 'black',
            table.background.color = '#F2F2F2',
            table.border.bottom.color = 'black',
            table.border.left.color = "black",
            table.border.right.color = "black",
            row.striping.background_color = '#FFFFFF',
            row.striping.include_table_body = TRUE,
            row.striping.include_stub = TRUE,
            footnotes.border.bottom.color = "black",
            stub.border.color = "black",
            stub.font.weight = "bold",
            table.font.size = "12px",
            footnotes.border.lr.color = "black",
            source_notes.border.bottom.color = "black"
        )
}

defaultdf <- qb_print(final_df, "Kate")

ui <- fluidPage(theme = shinytheme("darkly"),
    
    br(),
    fluidRow(
        column(2, img(src = 'logo.png', align = "right", height = 300)),
        #column(1),
        column(2, pickerInput("tog", "Select Projections Type:", choices = c("Positional","Team","Average"), selected = "Positional")),
        conditionalPanel("input.tog == 'Team'", 
                         column(2, pickerInput("team", "Select Team:", choices = all_teams, 
                                               options = list(`live-search` = TRUE, size = 5)))),
        conditionalPanel("input.tog == 'Positional'", 
                         column(2, pickerInput("position", "Select Position:", choices = c("QB","RB","WR","TE")))),
        column(2, pickerInput("ranker", "Select Ranker:", choices = c("Kate","Michelle","Matt"))),
        # conditionalPanel("input.tog == 'Positional' || input.tog = 'Average'",
        #                  column(2, pickerInput("sort", "Select Column to Sort:", choices = NULL))),
        column(2, actionBttn("go", "Get Projections!", style = "gradient", color = "danger", icon = icon("football-ball"))),
    ),
    tags$style(type='text/css', "#go { width:100%; margin-top: 25px;}"),
    fluidRow(
        withSpinner(gt_output("projtable"))
    ),
    tags$style(type='text/css', "#projtable { width:100%; margin-top: -200px;}"),
)


server <- function(input, output, session) {
    
    # observeEvent(input$tog, {
    #     if(input$position == "QB"){
    #         df <- qb_print(final_df, input$ranker)
    #         s <- "Fantasy Points"
    #     }
    #     if(input$position == "RB"){
    #         df <- rb_print(final_df, input$ranker)
    #         s <- "Fantasy Points"
    #     }
    #     if(input$position == "WR"){
    #         df <- wr_print(final_df, input$ranker)
    #         s <- "Fantasy Points"
    #     }
    #     if(input$position == "TE"){
    #         df <- te_print(final_df, input$ranker)
    #         s <- "Fantasy Points"
    #     }
    #     if(input$tog == "Average"){
    #         df <- unique(final_df %>%
    #                          filter(`Fantasy Points` > 0,
    #                                 !grepl("TE2", Player), !grepl("TE3", Player), !grepl("TE4", Player),
    #                                 !grepl("TE5", Player), !grepl("TE6", Player), !grepl("TE7", Player), !grepl("TE8", Player),
    #                                 !grepl("WR2", Player), !grepl("WR3", Player), !grepl("WR4", Player),
    #                                 !grepl("WR5", Player), !grepl("WR6", Player), !grepl("WR7", Player), !grepl("WR8", Player),
    #                                 !grepl("RB2", Player), !grepl("RB3", Player), !grepl("RB4", Player),
    #                                 !grepl("RB5", Player), !grepl("RB6", Player), !grepl("RB7", Player), !grepl("RB8", Player),
    #                                 !grepl("QB2", Player), !grepl("QB3", Player), !grepl("QB4", Player)) %>%
    #                          select(ranker,headshot_url,Player,Team,Position,`Fantasy Points`)) %>% spread(ranker,`Fantasy Points`) %>%
    #             mutate_at(c("Kate","Michelle","Matt"), funs(as.numeric)) %>% group_by(Position) %>%
    #             dplyr::mutate(Average = round(rowMeans(across(where(is.numeric)), na.rm = TRUE),1),
    #                           `Consensus PosRank` = paste0(Position, rank(-Average, ties.method = "first")),
    #                           Kate = round(Kate, 1),
    #                           Michelle = round(Michelle, 1),
    #                           Matt = round(Matt, 1)) %>% arrange(-Average) %>% ungroup() %>%
    #             select(headshot_url,Player,Team,Kate,Matt,Michelle,Average,`Consensus PosRank`)
    #         s <- "Consensus PosRank"
    #     }
    #     
    #     updatePickerInput(session, "sort", "Select Columns to Sort:", choices = colnames(df), selected = s)
    # })
    
    
    projData <- reactiveValues(data = defaultdf)
    observeEvent(input$go, {
        
        if(input$tog == "Positional" & input$position == "QB"){
            df <- qb_print(final_df, input$ranker)
        }
        if(input$tog == "Positional" & input$position == "RB"){
            df <- rb_print(final_df, input$ranker)
        }
        if(input$tog == "Positional" & input$position == "WR"){
            df <- wr_print(final_df, input$ranker)
        }
        if(input$tog == "Positional" & input$position == "TE"){
            df <- te_print(final_df, input$ranker)
        }
        if(input$tog == "Team"){
            df <- team_print(final_df, input$team, input$ranker)
        }
        if(input$tog == "Average"){
            df <- unique(final_df %>%
                             filter(`Fantasy Points` > 0,
                                    !grepl("TE2", Player), !grepl("TE3", Player), !grepl("TE4", Player),
                                    !grepl("TE5", Player), !grepl("TE6", Player), !grepl("TE7", Player), !grepl("TE8", Player),
                                    !grepl("WR2", Player), !grepl("WR3", Player), !grepl("WR4", Player),
                                    !grepl("WR5", Player), !grepl("WR6", Player), !grepl("WR7", Player), !grepl("WR8", Player),
                                    !grepl("RB2", Player), !grepl("RB3", Player), !grepl("RB4", Player),
                                    !grepl("RB5", Player), !grepl("RB6", Player), !grepl("RB7", Player), !grepl("RB8", Player),
                                    !grepl("QB2", Player), !grepl("QB3", Player), !grepl("QB4", Player)) %>%
                             select(ranker,headshot_url,Player,Team,Position,`Fantasy Points`)) %>% spread(ranker,`Fantasy Points`) %>%
                mutate_at(c("Kate","Michelle","Matt"), funs(as.numeric)) %>% group_by(Position) %>%
                dplyr::mutate(Average = round(rowMeans(across(where(is.numeric)), na.rm = TRUE),1),
                              `Consensus PosRank` = paste0(Position, rank(-Average, ties.method = "first")),
                              Kate = round(Kate, 1),
                              Michelle = round(Michelle, 1),
                              Matt = round(Matt, 1)) %>% arrange(-Average) %>% ungroup() %>%
                select(headshot_url,Player,Team,Kate,Matt,Michelle,Average,`Consensus PosRank`)
            
           df <- df %>% gt(rowname_col = "Consensus PosRank") %>%
                tab_header(title = 'BallBlast Consensus Projections') %>%
                tab_stubhead(label = "Rank") %>%
                text_transform(
                    locations = cells_body(vars(Team)),
                    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
                ) %>%
                text_transform(
                    locations = cells_body(columns = vars(headshot_url)),
                    fn = function(x){
                        gt::web_image(x)
                    }
                ) %>%
                cols_label(
                    headshot_url = 'Player',
                    Player = '',
                    Team = ''
                ) %>%
                tab_style(style = cell_text(font = google_font(name = "Questrial"),
                                            weight = "bold"),
                          locations = cells_title()) %>%
                tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                          locations = cells_body(vars(Team,Kate,Michelle,Matt,Average))) %>%
                tab_style(style = cell_borders(sides = c("right","bottom"), color = "black", weight = px(2), style = "solid"),
                          locations = cells_stub()) %>%
                tab_style(style = cell_borders(sides = c("bottom"), color = "black", weight = px(2), style = "solid"),
                          locations = cells_body(vars(Player,headshot_url))) %>%
                tab_style(style = cell_text(weight = "bold"), locations = cells_body(vars(Player))) %>%
                tab_style(style = cell_text(size = "14px"), locations = cells_stub()) %>%
                #fmt_number(columns = c(), decimals = 1) %>%
                fmt_missing(columns = 5:7, missing_text = "") %>%
                tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
                tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>%
                tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(Player))) %>%
                tab_style(style = cell_text(align = 'left'), locations = cells_column_labels(vars(Player))) %>%
                tab_source_note(source_note = '') %>%
                data_color(
                    columns = 5,
                    colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                                 domain = c(min(df$Kate, na.rm = TRUE),max(df$Kate, na.rm = TRUE))),
                    autocolor_text = FALSE
                ) %>%
                data_color(
                    columns = 6,
                    colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                                 domain = c(min(df$Matt, na.rm = TRUE),max(df$Matt, na.rm = TRUE))),
                    autocolor_text = FALSE
                ) %>%
                data_color(
                    columns = 7,
                    colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                                 domain = c(min(df$Michelle, na.rm = TRUE),max(df$Michelle, na.rm = TRUE))),
                    autocolor_text = FALSE
                ) %>%
                data_color(
                    columns = 8,
                    colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(n = 5, name = "RdYlGn"),
                                                 domain = c(min(df$Average, na.rm = TRUE),max(df$Average, na.rm = TRUE))),
                    autocolor_text = FALSE
                ) %>%
                cols_width(c(Player) ~ px(175),
                           c(`Consensus PosRank`) ~ px(64)) %>%
                tab_options(
                    table.width = px(1200),
                    table.font.color = "black",
                    data_row.padding = '5px',
                    row_group.padding = '3px',
                    heading.border.bottom.color = 'black',
                    column_labels.border.bottom.color = 'black',
                    column_labels.border.bottom.width = 1.4,
                    column_labels.font.weight = "bold",
                    table_body.border.top.color = 'black',
                    table_body.border.bottom.width = 0.7,
                    table_body.border.bottom.color = 'black',
                    row_group.border.bottom.width = 1,
                    row_group.border.bottom.color = 'black',
                    row_group.border.top.width = 1.5,
                    row_group.border.top.color = 'black',
                    row_group.font.size = "14px",
                    row_group.font.weight = "bold",
                    table.border.top.color = 'black',
                    table.background.color = '#F2F2F2',
                    table.border.bottom.color = 'black',
                    table.border.left.color = "black",
                    table.border.right.color = "black",
                    row.striping.background_color = '#FFFFFF',
                    row.striping.include_table_body = TRUE,
                    row.striping.include_stub = TRUE,
                    footnotes.border.bottom.color = "black",
                    stub.border.color = "black",
                    stub.font.weight = "bold",
                    table.font.size = "12px",
                    footnotes.border.lr.color = "black",
                    source_notes.border.bottom.color = "black"
                )
            
        }
        
        projData$data <- df
        
    })
    
    output$projtable <- render_gt({
        
        projData$data
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
