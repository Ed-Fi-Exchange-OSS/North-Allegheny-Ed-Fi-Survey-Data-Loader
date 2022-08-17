

# This is a Shiny web application...

# First-time package installation, if needed:

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(httr)
library(tidyverse)
library(jsonlite)
library(shinyWidgets)
library(DT)
library(readxl)
library(stringdist)
library(lubridate)
library(dashboardthemes)
library(shinyjs)
library(DBI)
library(RSQLite)
library(parallel)
library(uuid)
library(shinyBS)
library(openssl)
library(pbapply)
library(rintrojs)
library(fontawesome)

###############
### Options ###
###############

pboptions(
    type = "shiny",
    nout = 4,
    initial = 1,
    title = "Data Upload Progress",
    label = "Almost there ...")

#################
### FUNCTIONS ###
#################

getODSResource<-function(resource, baseURL, token, limit=100, moreParameters=""){
    firstReq<-GET(paste0(baseURL, resource,"?limit=",limit, moreParameters %>% URLencode()),
                  add_headers("Authorization" = 
                                  paste0("Bearer ", token$credentials$access_token)))
    firstRes<-fromJSON(rawToChar(firstReq$content), flatten = T)
    resourceOut<-firstRes
    
    resLength<-ifelse(is.null(nrow(firstRes)), 0, nrow(firstRes))
    if(resLength==limit){
        iter<-1
        continue<-1
        while(continue>0){
            tempReq<-GET(paste0(baseURL, resource,"?limit=",limit,moreParameters %>% URLencode(),
                                "&offset=",nrow(resourceOut)),
                         add_headers("Authorization" = 
                                         paste0("Bearer ", token$credentials$access_token)))
            tempRes<-fromJSON(rawToChar(tempReq$content), flatten = T)
            resourceOut<-rbind(resourceOut,tempRes)
            iter<-iter+1
            continue<-ifelse(length(tempRes)==0,0,1)
        }
    }
    resourceOut
}

getODSDescriptors<-function(descriptor, baseURL, token){
    descriptors<-fromJSON(rawToChar(
        GET(paste0(baseURL, descriptor,"?limit=100"),
            add_headers("Authorization" =
                            paste0("Bearer ", token$credentials$access_token)))$content
    ))
    # output<-paste0(descriptors$namespace[descriptors$description==choice],"#", 
    #                descriptors$codeValue[descriptors$description==choice])
    
    descriptors
}

formatDescriptor<-function(descriptor, baseURL, choice, token){
    descriptors<-fromJSON(rawToChar(
        GET(paste0(baseURL, descriptor,"?limit=100"),
            add_headers("Authorization" =
                            paste0("Bearer ", token$credentials$access_token)))$content
    ))
    output<-paste0(descriptors$namespace[descriptors$codeValue==choice],"#", 
                   descriptors$codeValue[descriptors$codeValue==choice])
    output
}

## Create or Connect to App Database
userSessionID<-UUIDgenerate()

connectAppDB<-function(){
    con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
    if(!dbExistsTable(con, "postLog")){
        tempDF<-data.frame(
            postID = NA,
            sessionID = NA,
            timestamp = NA,
            postBody = NA,
            headers = NA,
            apiResponse = NA
        )
        dbCreateTable(con, "postLog", tempDF)
        tempLoad<-data.frame(
            convoyID = NA,
            payload = NA
        )
        dbCreateTable(con, "tempLoad", tempLoad)
        
    }
    if(!dbExistsTable(con, "admin")){
        tempDF<-data.frame(
            type = "adminPass",
            key = "adminPass",
            value = sha256("admin") %>% as.character()
        )
        dbWriteTable(con, "admin", tempDF)
        }
    
    if(!dbExistsTable(con, "odsConnections")){
        dbCreateTable(con, "odsConnections",
                      data.frame(
                          connectionID = NA,
                          connectionName = NA,
                          url = NA,
                          key = NA,
                          secret = NA,
                          defaultNamespace = NA,
                          userBasedAccess = NA,
                          permittedUsers = NA
                      ))
    }
    dbDisconnect(con)
}

connectAppDB()

### Temp Token (Dev Use Only) ##
{
# tempToken<-        oauth2.0_token(
#     endpoint = oauth_endpoint(
#         authorize = "authorize",
#         access = "token",
#         base_url = paste0(substr("", 1,
#                                  regexpr("data", "")[[1]]-1),"oauth")
#     ),
#     app = oauth_app(
#         appname = "ODS Sandbox",
#         key = "",
#         secret = ""
#     ),
#     client_credentials = T,
#     cache = F)
}

####################
### CUSTOM THEME ###
####################

{
    customTheme <- shinyDashboardThemeDIY(
        ### general
        appFontFamily = "Montserrat"
        ,appFontColor = "#000000"
        ,primaryFontColor = "#000000"
        ,infoFontColor = "#7A0EED"
        ,successFontColor = "#0F0F0F"
        ,warningFontColor = "#0F0F0F"
        ,dangerFontColor = "#C42B2B"
        ,bodyBackColor = "#CCB562"
        
        ### header
        ,logoBackColor = "#000000"
        
        ,headerButtonBackColor = "#1F1F1F"
        ,headerButtonIconColor = "#DCDCDC"
        ,headerButtonBackColorHover = "#646464"
        ,headerButtonIconColorHover = "#3C3C3C"
        
        ,headerBackColor = "#000000"
        ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
        ,sidebarBackColor = "#424242"
        ,sidebarPadding = "0"
        
        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = "0"
        ,sidebarMenuBorderRadius = 0
        
        ,sidebarShadowRadius = ""
        ,sidebarShadowColor = "0px 0px 0px"
        
        ,sidebarUserTextColor = "#737373"
        
        ,sidebarSearchBackColor = "#F0F0F0"
        ,sidebarSearchIconColor = "#646464"
        ,sidebarSearchBorderColor = "#DCDCDC"
        
        ,sidebarTabTextColor = "#F0EDED"
        ,sidebarTabTextSize = "14"
        ,sidebarTabBorderStyle = "none"
        ,sidebarTabBorderColor = "none"
        ,sidebarTabBorderWidth = "0"
        
        ,sidebarTabBackColorSelected = "#E6E6E6"
        ,sidebarTabTextColorSelected = "#000000"
        ,sidebarTabRadiusSelected = "0px"
        
        ,sidebarTabBackColorHover = "#F5F5F5"
        ,sidebarTabTextColorHover = "#000000"
        ,sidebarTabBorderStyleHover = "none solid none none"
        ,sidebarTabBorderColorHover = "#C8C8C8"
        ,sidebarTabBorderWidthHover = "4"
        ,sidebarTabRadiusHover = "0px"
        
        ### boxes
        ,boxBackColor = "#FFFFFF"
        ,boxBorderRadius = "5"
        ,boxShadowSize = "none"
        ,boxShadowColor = ""
        ,boxTitleSize = "18"
        ,boxDefaultColor = "#E1E1E1"
        ,boxPrimaryColor = "#5F9BD5"
        ,boxInfoColor = "#B4B4B4"
        ,boxSuccessColor = "#70AD47"
        ,boxWarningColor = "#ED7D31"
        ,boxDangerColor = "#E84C22"
        
        ,tabBoxTabColor = "#F8F8F8"
        ,tabBoxTabTextSize = "14"
        ,tabBoxTabTextColor = "#646464"
        ,tabBoxTabTextColorSelected = "#2D2D2D"
        ,tabBoxBackColor = "#F8F8F8"
        ,tabBoxHighlightColor = "#C8C8C8"
        ,tabBoxBorderRadius = "5"
        
        ### inputs
        ,buttonBackColor = "#D9D2A1"
        ,buttonTextColor = "#000000"
        ,buttonBorderColor = "#969696"
        ,buttonBorderRadius = "30"
        
        ,buttonBackColorHover = "#BEBEBE"
        ,buttonTextColorHover = "#000000"
        ,buttonBorderColorHover = "#969696"
        
        ,textboxBackColor = "#FFFFFF"
        ,textboxBorderColor = "#767676"
        ,textboxBorderRadius = "5"
        ,textboxBackColorSelect = "#F5F5F5"
        ,textboxBorderColorSelect = "#6C6C6C"
        
        ### tables
        ,tableBackColor = "#EBE3B7"
        ,tableBorderColor = "#C2C2C2"
        ,tableBorderTopSize = "1"
        ,tableBorderRowSize = "1"
    )
}

##########
### UI ###
##########

ui <- dashboardPage(
    
    # Header
    dashboardHeader(title = "Ed-Fi Form Loader Application/Utility (FLAT)",
                    
                    leftUi = tagList(actionButton("startIntro", "How To Use This Page", 
                                                  style="color: #182b38; background-color: #f7c705; border-color: white; box-shadow: 0px 0px 6px white; font-weight: bold"))),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        introjsUI(),
        
        # Style specification to enable wider intro boxes for user intro/walkthrough
        tags$style(HTML(
            ".introjs-tooltip {
      max-width: 100%;
      min-width: 600px;
    }"
        )),
        sidebarMenu(id = "sideBarTabs",
            menuItem("1. Connect", tabName = "appConnections", icon = icon("plug")),
            menuItem("2. Survey Metadata", tabName = "surveyMetadata", icon = icon("clipboard-check")),
            menuItem("3. Item Metadata", tabName = "itemMetadata", icon = icon("question-circle")),
            menuItem("4. Load Results", tabName = "loadResults", icon = icon("upload")),
            menuItem("Admin", tabName = "admin", icon = icon("gear")),
            headerPanel(" "),
            headerPanel(" "),
            actionButton("refreshConnection", "Refresh Connection", 
                         icon = icon("retweet"),
                         #size = "xs", 
                         #style = "material-flat"
            ),
            headerPanel(" ")

        ))
    ,

        # MAIN PANEL
        dashboardBody(
            
            # Invoke Shiny JS
            useShinyjs(),
            
            #customTheme,
            
             shinyDashboardThemes(
                 theme = "poor_mans_flatly"
             ),
            
            tabItems(
                
                # ODS/DATA CONNECTION
                tabItem(tabName = "appConnections",
                        
                        # ODS Connection
                         #div(style = "border-style: groove groove groove groove; border-radius: 20px;
                         #padding: 15px;",
                        box(id = "connectOdsBox", title = "Ed-Fi ODS Connection", 
                            status = "black",
                            #h3("Ed-Fi ODS Connection", align = "center"),
                            uiOutput("odsConnectionOptions"),
                            textInput(inputId = "ODS_URL",
                                      label = "API URL (ending with'ed-fi/')",
                                      #placeholder = "https://YOUR_URL_HERE/edfi.ods.webapi/data/v3/ed-fi/"
                                      
                            ),
                            bsTooltip("ODS_URL", 
                                      title = "Full URL For Connecting to your ODS API. This should end with \\'/ed-fi/\\'", 
                                      placement = "top"),
                            textInput(inputId = "apiKey", # KEY
                                      label = "Client Key"
                                      
                            ),
                            passwordInput(inputId = "apiSecret", # SECRET
                                          label = "Client Secret"),
                            actionButton("connectODS", "Connect!")), # CONNECT BUTTON
                        
                        # headerPanel(" "),
                        # div(style = "height: 20px;"),
                        
                        # Flat File Upload
                        # div(style = "border-style: groove groove groove groove; border-radius: 20px;
                        # padding: 15px;",
                        box(id = "uploadFileBox", title = "Flat File Upload", status = "black",
                            #h3("Flat File Upload", align = "center"),
                            fileInput("fileIn", "Upload Survey Results File", accept = c(".xls", ".xlsx", ".csv")),
                            actionButton("fileUploadTooltip", HTML(as.character(icon("info-circle", lib = "font-awesome")))),
                            bsPopover("fileUploadTooltip", 
                                      title = "<b>File Guidelines</b>",
                                      content = "Accepts .xlsx or .csv files. File should include only a plain table (no missing rows or special formatting) in the general layout of a Google Forms response file, with one row per response and one column per response property. <p><p><i>If a .xlsx file is uploaded, only the content from the first tab will be used</i>",
                                      placement = "bottom", 
                                      trigger = "hover"),
                            headerPanel(" "),
                            headerPanel(" "),
                            h1(" "),
                            br(),
                            #bsCollapse("sampleTableCollapse", 
                                       bsCollapsePanel(HTML("<b><u>Example Table</u></b>"), 
                                                       h4("The data structure in your file should resemble the example table below (number of rows and columns are arbitrary, as are column names, but no rows are empty, each row is a separate response, each column is a question or response property, and a date/datetime column is included to indicate survey completion date/time):", 
                                                          style = "text-align:center;padding-left:50px;padding-right:50px;"),
                                                       DTOutput("sampleTable")
                                                       )
                                       #)

                            )#, # FILE INPUT
                        
                ),
                
                # SURVEY METADATA
                tabItem(tabName = "surveyMetadata",
                

                         div(style = "border-style: groove groove groove groove; 
                                        border-radius: 20px; padding: 15px;",
                             #div(style = "border-style: groove groove groove groove;
                             #           padding: 15px;",
                                # h3("Typeform Surveys", align = "center"),
                                # DTOutput("forms")
                             #   ),
                         
                             h3(HTML("<b>Load New Survey</b>"), align = "center"),
                             HTML("<p><b>Use this page to tell the Data Store about a new survey you want to upload.</b></p>"),
                                withSpinner(uiOutput("surveyInputs"), 
                                            type = 4, 
                                            color = "grey"),
                             DTOutput("questionInfo")
                             )
                         
                         ), 
                
                # SURVEY QUESTIONS
                    tabItem(tabName = "itemMetadata",
                        
                         div(style = "border-style: groove groove groove groove; 
                                        border-radius: 20px; padding: 15px;",
                             h3(HTML("<b>Load Survey Items</b>"), align = "center"),
                             HTML("<p><b>Use this page to tell your Data Store about the questions/items you will be uploading for this survey.</b></p>"),
                             bsCollapsePanel(HTML("<b><u>Instructions</u></b>"), 
                                                 div(HTML("<p>The <i>All Items</i> table on the left shows one line for each column header in the file you uploaded.

                                <ul><li><b>To begin</b>, choose the survey you are uploading from the list right below these instructions.

                                <li><b>To proceed</b>, click through each row in the <i>All Items</i> table and update the question properties, as necessary, in the editor that appears to the right (be sure to click 'Update Item' to save your changes).

                                <li><b>When you're finished</b>, click 'Preview' to check your work before clicking 'Post' to upload your survey item metadata.</ul></p>")),
                                style = "info"
                             ),
                             headerPanel(" "),
                             uiOutput("Questions_ODSSurveyUI"),
                             # pickerInput("Questions_formType", 
                             #             "Source",
                             #             choices = c("Microsoft Forms",
                             #                         "Google Forms", 
                             #                         "Typeform")%>%
                             #                 sort()),
                             fluidRow(
                                 column(width = 4,
                                        uiOutput("questionColumnOptionsUI")),
                                 column(width = 8,
                                        withSpinner(uiOutput("questionPropertiesUI"), 
                                                    type = 4,
                                                    color = "grey"))
                                 )
                         )
                         ),
                
                # RESPONSES
                    tabItem(tabName = "loadResults",
                        
                         div(style = "border-style: groove groove groove groove; 
                                        border-radius: 20px; padding: 15px;",
                             h3(HTML("<b>Load Survey Responses</b>"), align = "center"),
                             HTML("<b>Use this page to upload the actual survey response data to your data store. NOTE: Survey metadata and item metadata (steps 2 & 3, at left) must exist in your Data Store before you can successfully upload response data.</b>
                                  However, if you have already uploaded the metadata and a first batch of responses, there is no need to re-upload metadata before using this page to upload another batch of responses to the same survey.<br><br>"),
                             bsCollapsePanel(style = "info", HTML("<b><u>Instructions</u></b>"), 
                                             HTML("<h3>Steps</h3>
                                  <br><ol>
                                            <li><b>Map & Load Survey Response Metadata - </b> Use the prompts on the left side of the screen to configure the metadata for each survey response entry (e.g. respondent info and completion date/time and location, if applicable).
                                                <ol>
                                                    <li><b>Click 'Preview'</b> (on the left) to transform the data from your file upload to the proper format.
                                                    <li><b>Review Transformed Data</b> in the viewer that appears. Ensure that, as you page through, preview data matches the data for the proper row in your upload file.
                                                    <li><b>Submit!</b> Click the 'POST' button to upload your data to the Data Store. A prompt will inform you if there were any errors.
                                                    <li><b>Download Log File</b> Click the 'Download API POST Log' button to download a log of the upload results. Share this with your data administrator if you encountered errors during the upload.</ol>
                                            <li><b>Map & Load Survey Question Response Data </b> - Use the prompts on the right side of the screen to map survey items with your file upload. Each prompt comes from a piece of item metadata you uploaded in Step 3 (at left), and the dropdown options are column headers from your file upload (Step 1). 
                                                    <i>Note: The application will try its best to map columns from your upload to survey item text from the metadata you uploaded, based on string similarity, but <u> you should always verify the default mappings</u>. Default mappings with a poor matching score are preceded with an exclamation point!</i>
                                                <ol>
                                                    <li><b>Click 'Preview'</b> (on the right) to start the transform-and-load process. A prompt will inform you if there were any errors.
                                                    <li><b>Download Log File</b> Click the 'Download API POST Log' button to download a log of the upload results. Share this with your data administrator if you encountered errors during the upload.</ol>
                                    </ol>
                                  ")
                             ),
                             div(id = "surveyResponseMetadataBlock", style = "border-style: groove groove groove groove;padding: 15px;",
                                 h4("Survey Response Metadata"),
                                 withSpinner(uiOutput("Responses_ODSSurveyUI"),
                                             type = 4, 
                                             color = "grey") %>% fluidRow(),
                                 withSpinner(uiOutput("Responses_surveyResponseIdentifierUI"),
                                             type = 4,
                                             color = "grey") %>% fluidRow()
                             ),
                             fluidRow(
                                 tabsetPanel(id = "SR_ResponseMapping",
                                     tabPanel("Part A: Survey Responses",
                                              column(width = 4,
                                                     withSpinner(uiOutput("Responses_SurveyResponseUI"),
                                                                 type = 4,
                                                                 color = "grey")),
                                              column(width = 8,
                                                     h4("Survey Responses (General)"),
                                                     actionBttn("SR_previous", "<<", style = "material-flat",
                                                                size = "xs"),
                                                     actionBttn("SR_next", ">>", style = "material-flat",
                                                                size = "xs"),
                                                     downloadButton("SR_downloadPostLog", "Download API POST Log"),
                                                     # verbatimTextOutput("responses_surveyResponseJSON"),
                                                     DTOutput("responses_surveyResponseJSON"),
                                                     h4("Survey Question Responses")#,
                                                     #uiOutput("SQR_UI")
                                              )
                                              ),
                                     tabPanel("Part B: Survey Question Responses",
                                              withSpinner(uiOutput("SQR_UI"),
                                                          type = 4,
                                                          color = "grey")
                                              )
                                 )

                             )
                         ) 
                         ),
                
                # ADMIN
                tabItem("admin",
                        passwordInput("adminPass", "Admin Configuration Key"),
                        actionBttn("submitAdminPass", "Submit!", style = "material-flat", color = "primary", size = "sm"),
                        headerPanel(" "),
                        headerPanel(" "),
                        uiOutput("adminPage")
                        )
            
            #)
        )
    )
)

##############
### SERVER ###
##############

server <- function(input, output, session) {

    baseURL<-"https://api.typeform.com/"
    
    ## Local vs. Published Switch (Dev use)
    appMode<-reactive({
        if(Sys.getenv("R_CONFIG_ACTIVE")=="rsconnect"){
            "published"
        }else{
            "local"
        }
    })

    ## Get User
    currentUser<-reactive({        
        
        if(appMode()=="local"){
            out<-Sys.getenv("USERNAME") # Dev Use...
        }else{
            out<-session$user%>%tolower() # RStudio Connect Use...
        }
        print(out)
        return(out)
        
        # NOTE: For non-RStudio Connect implementations, data/IT admin will need to establish methodology for constructing
        # this variable and wider authentication scheme.
        
    })
    
    # Typeform PAT
    PAT<-reactiveValues(token = NA)
    
    # PAT Check
    checkPAT<-function(){
        req(input$connectPAT)
        if(is.na(PAT$token)){
            sendSweetAlert(session, "Hang on!", "Did you connect to Typeform in the left-side panel?")
            return(1)
        }else{
            return(0)
            }
    }
    
    # Test Typeform API Connection
    # observeEvent(input$connectPAT, {
    #     if(GET(paste0(baseURL, "/forms"),
    #                       add_headers("Authorization" = 
    #                                       paste0("Bearer ", input$typeformPAT)))$status_code==200
    #     ){
    #         PAT$token<-input$typeformPAT
    #         sendSweetAlert(session, "Awesome!", "Connection Successful", "success")
    #     }else{
    #         sendSweetAlert(session, "Uh oh...", "Invalid Token", "error")
    #     }
    # })
    
    # ODS Connection User Feedback
    
    # observeEvent(input$connectODS, {
    #     
    #     # Create ODS App and Endpoint Objects
    #     ODS_Endpoint<-oauth_endpoint(
    #         authorize = "authorize",
    #         access = "token",
    #         base_url = paste0(substr(input$ODS_URL, 1,regexpr("data", input$ODS_URL)[[1]]-1),"oauth")
    #     )
    #     
    #     ODS_App<-oauth_app(
    #         appname = "ODS Sandbox",
    #         key = input$apiKey,
    #         secret = input$apiSecret
    #     )
    #     
    #     
    #     # Test Connection
    #     tryConnect<-class(try(
    #         oauth2.0_token(endpoint = ODS_Endpoint, 
    #                        app = ODS_App, 
    #                        client_credentials = T, 
    #                        cache = F)))
    #     testConnect<-ifelse(length(tryConnect)==1 & tryConnect=="try-error",
    #                         "fail", "pass")
    #     testConnect<-ifelse("pass"%in%testConnect, "pass", "fail")
    #     
    #     if(testConnect=="pass"){sendSweetAlert(session, "ODS Connection Successful!", type = "success")}
    #     else{sendSweetAlert(session, "ODS Connection Unsuccessful :(", type = "danger")}
    # })
    
    # Token Refresh setup
    observeEvent(input$refreshConnection, {
        click("connectODS")
    })
    
    # Token Expiration Stamp
    tokenExpiration<-reactiveValues(timestamp=NA)
    
    # ODS Token
    myToken<-eventReactive(input$connectODS, {
        req(input$connectODS)
        input$refreshConnection
        
        # Create ODS App and Endpoint Objects
        ODS_Endpoint<-oauth_endpoint(
            authorize = "authorize",
            access = "token",
            base_url = paste0(substr(input$ODS_URL, 1,regexpr("data", input$ODS_URL)[[1]]-1),"oauth")
        )
        
        ODS_App<-oauth_app(
            appname = "ODS Sandbox",
            key = input$apiKey,
            secret = input$apiSecret
        )
        
        # Test Connection
        tryConnect<-class(try(
            oauth2.0_token(endpoint = ODS_Endpoint, 
                           app = ODS_App, 
                           client_credentials = T, 
                           cache = F)))
        testConnect<-ifelse(length(tryConnect)==1 & tryConnect=="try-error",
                            "fail", "pass")
        testConnect<-ifelse("pass"%in%testConnect, "pass", "fail")
        
        if(testConnect=="pass"){
            sendSweetAlert(session, "ODS Connection Successful!", type = "success")
            
            out<-oauth2.0_token(
                endpoint = ODS_Endpoint, 
                app = ODS_App, 
                client_credentials = T, 
                cache = F)
            
            tokenExpiration$timestamp<-Sys.time()+out$credentials$expires_in
            print("successfully retrieved token")
            #print(names(out))
            #out$expireTime<-Sys.time()+out$credentials$expires_in
            tokenExpiration$timestamp<-Sys.time()+out$credentials$expires_in
            out
            }
        else{sendSweetAlert(session, "ODS Connection Unsuccessful :(", type = "danger")}
        

    })
    
    
    # Ingest File Upload (if applicable)
    fileUpload<-eventReactive(input$fileIn,
                                {file1<-input$fileIn
                                path<-file1$datapath
                                
                                if(str_detect(tolower(path), ".xls")){
                                    out<-read_excel(path)}else{
                                        if(str_detect(tolower(path), ".csv")){
                                            out<-read.csv(path, stringsAsFactors = F)
                                            colnames(out) <- colnames(out) %>% str_replace_all("\\.", " ")
                                        }
                                    }
                                
                                # Clip values at 255 characters, per frustrating API restrictions
                                out<-map_df(out, function(x){
                                    if(class(x) %>% paste0(collapse = "")=="character"){substr(x,1,255)}else{x}
                                })
                                
                                # colnames(file1)[1] <- ifelse(str_detect(colnames(file1)[1]%>%
                                #                                             substr(1,1),"[a-zA-Z0-9_]")==F,
                                #                              gsub('^...','',colnames(file1)[1]), colnames(file1)[1])
                                
                                out
                                })
    
    ## Set Survey Pickers
    observeEvent(input$sideBarTabs, {
        print(input$sideBarTabs)
        currentTab<-input$sideBarTabs
        print(input$connectODS==0)
        
        if(input$connectODS==0 | (!(isTruthy(input$fileIn)))){
            if(currentTab!="appConnections" & currentTab!="admin"){
                sendSweetAlert(session, "Hang on!", 
                               "Please connect to an ODS and upload a survey file in the 'Connect' tab in order to use the other parts of this app!",
                               type = "warning")
            }
        }else{
            if(currentTab!="appConnections" & currentTab!="admin" & (!length(myToken())>1)){
                sendSweetAlert(session, "Hang on!", 
                               "Please connect to an ODS and upload a survey file in the 'Connect' tab in order to use the other parts of this app!",
                               type = "warning")
            }else{

        # Set Survey Pickers to best match
        # Questions Tab
        updateSelectInput(session, "Questions_ODSSurvey", 
                          selected = unlist(ODSSurveys())[
                              stringsim(input$fileIn$name,
                                        ODSSurveys() %>% unlist(), method = "jaccard")%>%which.max()])
        # Responses Tab
        updateSelectInput(session, "Responses_ODSSurvey", 
                          selected = unlist(ODSSurveys())[
                              stringsim(input$fileIn$name,
                                        ODSSurveys() %>% unlist(), method = "jaccard")%>%which.max()])
            }
            }
    })
    
    #-------------------------------#
    # Existing ODS Survey Reactives #
    #-------------------------------#
    
    ## SURVEYS ##
    # Reactive
    ODSSurveys<-reactive({
        req(myToken())
        req(input$fileIn)
        out<-tryCatch({getODSResource("surveys", input$ODS_URL, myToken())%>%
            as.data.frame()},
            error=function(cond){data.frame("Survey" = NA)}
        )
        if(nrow(out)==0){
            data.frame("Survey" = NA)
        }else{
            out %>% 
                select("Title" = contains("surveyTitle"),
                       "Identifier" = contains("surveyIdentifier"))%>%
                mutate("Survey" = paste0(Title, " (ID: ", Identifier, ")"))%>%
                select(Survey)%>%
                as.vector()
        }
        

    })
    
    # Survey Drop-down
    output$Questions_ODSSurveyUI<-renderUI({
        pickerInput("Questions_ODSSurvey", "Choose Survey", 
                    choices = ODSSurveys(), multiple = F,
                    options = pickerOptions(liveSearch = T)) %>% tipify(
                        title = "This is a list of all known surveys in the Data Store you connected to in Step 1. If the survey you\\'re looking for does not appear here, be sure you have uploaded the survey metadata for it using the \\'Survey Metadata\\' tab at left (step 2), and try clicking \\'Refresh Connection\\'. Items are formatted as: <i>Survey Name (Survey Unique Identifier)</i>",
                        placement = "top"
                    )
    })
    
    # Selected Survey
    selectedODSSurvey<-reactive({
        req(ODSSurveys())
        req(input$Questions_ODSSurvey)
        substr(input$Questions_ODSSurvey, 
               str_locate(input$Questions_ODSSurvey, " \\(ID: ")[2]+1,
               nchar(input$Questions_ODSSurvey)-1)
    })
    
    ## SURVEY QUESTIONS ##
    # Reactive
    ODSSurveyQuestions<-reactive({
        req(myToken())
        getODSResource("surveyQuestions", input$ODS_URL, myToken(),
                       moreParameters = 
                           paste0("surveyIdentifier=",
                                  selectedODSSurvey()))%>%
            as.data.frame()
    })
    
    ## SURVEY RESPONSES (High-Level) ##
    
    
    ## SURVEY RESPONSE DETAILS ##
    
    
    #----------------#
    # Load Questions #
    #----------------#
    
    ## UI ##
    # Filter out metadata based on form type
    # columnExclusions<-reactive({switch(input$Questions_formType, 
    #                          "Google Forms" = c("Timestamp"),
    #                          "Microsoft Forms" = c("ID", "Start time", "Completion time"))
    # })
    
    # Create column headers (/question text) object once a file is uploaded (note reactivity to 
    # columnExclusions object above)
    columnHeaders<-reactiveValues(values = NA)
    observeEvent(fileUpload(), {
        req(fileUpload())
        out<-data.frame("Questions" = colnames(fileUpload()))
        # if(isTruthy(input$Questions_formType)){
        #     columnHeaders$values<-out#%>%filter(!Questions %in% columnExclusions())
        # }


        
        columnHeaders$values<-out
        })
    
    # Adjust column headers and question properties objects based on changes to the form type selection
    # observeEvent(columnExclusions(), {
    #     if("Questions" %in% names(columnHeaders$values)){
    #     columnHeaders$values<-columnHeaders$values%>%filter(!Questions %in% columnExclusions())
    #         if(length(questionProperties$values)>1){
    #             questionProperties$values<-questionProperties$values[-which(
    #                 map_lgl(questionProperties$values, 
    #                         function(x){x[["questionText"]] %in% columnExclusions()}))]
    #          print(which(
    #              map_lgl(questionProperties$values, 
    #                      function(x){x[["questionText"]] %in% columnExclusions()})))   
    #         }
    #     }
    # })
    
    # Set possible questions based on column headers
    output$questionColumns<-renderDT({
        input$deleteQuestion
        #columnHeaders$values
        data.frame("Item Text" = map_chr(questionProperties$values, function(x){x[["questionText"]]}))
        }, 
        selection = "single",
        class = 'table-condensed',
        options = list(scrollY = "250px", 
                       pageLength=100,
                       dom = "t")
    )
    

    ## QUESTIONS UI
    
    # Question Property Object
    questionProperties<-reactiveValues(values = NA)
    observeEvent(selectedODSSurvey(), {
        #req(input$Questions_ODSSurvey)
        #input$Questions_formType
        
        questionProperties$values<-imap(columnHeaders$values$Questions, function(x,y){
            list("surveyReference" = list(
                "namespace" = getODSResource("surveys", input$ODS_URL, myToken(), 
                                             moreParameters = paste0("surveyIdentifier=",
                                                                     selectedODSSurvey()))%>%
                    select(contains("namespace"))%>%
                    head(1)%>%
                    as.vector()%>%
                    unlist(),
                "surveyIdentifier" = selectedODSSurvey()
                ),
                "questionCode" = paste0(selectedODSSurvey(), "_Q", y),
                "questionText" = x,
                "questionFormDescriptor" = formatDescriptor(descriptor = "questionFormDescriptors", 
                                                            baseURL = input$ODS_URL,
                                                            token = myToken(),
                                                            choice = ifelse(unique(fileUpload()[[x]])%>%
                                                                                length()>8, "Textbox", 
                                                                            ifelse(class(
                                                                                fileUpload()[[x]])=="numeric",
                                                                                "Ranking",
                                                                                "Dropdown"))
                                                            ),
                "responseChoices" = switch(length(unique(fileUpload()[[x]]))<8,
                                                data.frame("sortOrder" = seq_along(unique(fileUpload()[[x]])),
                                                     "textValue" = unique(fileUpload()[[x]])),
                                                   NA),
                "originalColumnName" = x
            )
        })
    })
    
    # Column Options
    output$questionColumnOptionsUI<-renderUI({
        req(input$Questions_ODSSurvey)
        
        if(isTruthy(fileUpload())){
                   list(div(style = "border-style: groove groove groove groove;padding: 15px;",
                            h4("All Items", align = "center"),
                       DTOutput("questionColumns"), 
                       headerPanel(" "),
                       actionBttn("deleteQuestion", "Delete Selected", 
                                  style = "material-flat", 
                                  size = "xs", 
                                  color = "danger") %>% tipify(
                                      title = "Deleting an item means you are omitting it from the list of questions/prompts your Data Store will associate with this survey. (I.e. deleting one of these will mean you can\\'t upload survey response data for the deleted item.)",
                                      placement = "top"
                                  ))
                   )
            
        }else{h4("Oops! Please upload a file (at left) to get started!")}
    })
    
    # Delete Question
    
    observeEvent(input$deleteQuestion, {
        selected<-input$questionColumns_rows_selected
        questionProperties$values<-questionProperties$values[-selected]
        
        #columnHeaders$values<-columnHeaders$values[-selected,]
    })
    
    # Response Options
    questionResponseOptions<-reactive({
        req(input$Questions_ODSSurvey)
        selected<-input$questionColumns_rows_selected
        switch(length(unique(fileUpload()[[#columnHeaders$values$Questions[selected]
            questionProperties$values[[selected]][["originalColumnName"]]
            ]]))<8,
                                    unique(fileUpload()[[#columnHeaders$values$Questions[selected]
                                        questionProperties$values[[selected]][["originalColumnName"]]
                                        ]]),
                                    NA)})
    
    # Question Properties UI
    output$questionPropertiesUI<-renderUI({
        req(input$Questions_ODSSurvey)
        selected<-input$questionColumns_rows_selected
        input$Q_updateQuestionProperties
        #input$deleteQuestion
        #req(input$questionColumns)
        if(length(selected)){
            list(fluidRow(
                div(id = "Q_itemDetails", style = "border-style: groove groove groove groove; border-radius: 5px;padding: 15px;",
                fluidRow(h4("Item Details", align = "center"),
                    column(width = 4, 
                       textAreaInput("Q_questionCode", "Question Code", height = "75px",
                                 questionProperties$values[[selected]][["questionCode"]])
                       ) %>% tipify(
                           title = "Unique identifier for this survey item. This app creates a default identifier using the format: <i> surveyUniqueIdentifier_QuestionNumber</i>. Generally, you will not need to edit this value (even if you choose to remove some items using the red button at left)."
                       ),
                column(width = 4, 
                       textAreaInput("Q_questionText", "Question Text", height = "75px",
                                     questionProperties$values[[selected]][["questionText"]]
                       ) %>% tipify(
                           title = "What is the exact text respondents were prompted with? The default value shown here assumes the column header from your file upload contains the question/prompt text, but you can edit this as needed."
                       )),
                column(width = 4,
                       selectInput("Q_questionFormDescriptor", "Question Type", 
                                   choices = c("Checkbox",
                                               "Dropdown",
                                               "Matrix of dropdowns",
                                               "Matrix of numeric ratings",
                                               "Matrix of textboxes",
                                               "Radio box",
                                               "Ranking",
                                               "Textbox"),
                                   selected = questionProperties$values[[selected]][["questionFormDescriptor"]]%>%
                                       substr(
                                           str_locate(
                                               questionProperties$values[[selected]][["questionFormDescriptor"]], 
                                               "#")[1]+1, 
                                           nchar(questionProperties$values[[selected]][["questionFormDescriptor"]])
                                       )) %>% tipify(
                                           title = "What was the format of this item? \\'Textbox\\' or \\'Dropdown\\' will be appropriate in most cases. <b> NOTE: if your upload contains <8 unique values for a given column, this will default to \\'Dropdown\\' type (closed response). If >=8 unique values, \\'Textbox\\' is the assumed type.</b>"
                                       )
                       )),
                fluidRow(
                  column(width = 6,
                         selectInput("Q_responseChoices", "Response Choices",
                                     multiple = T,
                                     choices = questionProperties$values[[selected]][["responseChoices"]][["textValue"]],
                                     # questionResponseOptions(),
                                     selected = questionProperties$values[[selected]][["responseChoices"]][["textValue"]]
                         ) %>% tipify(
                             title = "What response options were respondents given for this prompt? <b>NOTE: If you changed the Question Type from \\'Dropdown\\' to \\'Textbox,\\' be sure to remove any lingering response choices before clicking \\'Update Item\\'</b> (I.e. this should be blank if the item type is \\'Textbox\\')."
                         )
                         ),
                  column(width = 4,
                         actionBttn("Q_enterNewResponseOption", "Add Response Choice",
                                    style = "material-flat", size = "xs", color = "royal")
                         )
                )
                ,
            actionBttn("Q_updateQuestionProperties", "Update Item", 
                       style = "material-flat", size = "xs", color = "success")),
            headerPanel(" ")),
            fluidRow(
            # actionBttn("Q_previewQuestionProperties", "Preview", 
            #            style = "material-flat", size = "sm", color = "royal"),
            actionBttn("Q_ODSPOST", "UPLOAD!",
                       style = "material-flat", size = "sm", color = "success"),
            downloadButton("SQ_downloadPostLog", "Download API POST Log"),
            uiOutput("testQPropsUI"))
            )
        }else{list(
            h4("Please choose a question (left) to edit."),
            # actionBttn("Q_previewQuestionProperties", "Preview", 
            #            style = "material-flat", size = "sm", color = "royal"),
            actionBttn("Q_ODSPOST", "UPLOAD!",
                       style = "material-flat", size = "sm", color = "success"),
            #downloadButton("SQ_downloadPostLog", "Download API POST Log"),
            uiOutput("testQPropsUI")
        )
            }
    })
    
    ## Add New Response Option (Modal) ##
    Q_newResponseOptionModal <- function(failed = FALSE) {
        modalDialog(
            textInput("Q_newResponseOption", "Response Option"
            ),
            if (failed)
                div(tags$b("Please enter text for this response option (or hit 'Cancel')",
                           style = "color: red;")),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("Q_submitNewResponseOption", "Submit")
            )
        )
    }
    
    # Make New Response Modal Appear
    observeEvent(input$Q_enterNewResponseOption, {showModal(Q_newResponseOptionModal())})
    
    ## Add New Response Option (Submission)
    observeEvent(input$Q_submitNewResponseOption, {
        req(input$Q_newResponseOption)
        updateSelectInput(session, "Q_responseChoices", 
                          choices = c(input$Q_responseChoices, 
                                      input$Q_newResponseOption),
                          selected = c(input$Q_responseChoices, #questionProperties$values[[input$questionColumns_rows_selected]][["responseChoices"]][["textValue"]],
                                       input$Q_newResponseOption))
        updateTextInput(session, "Q_newResponseOption", value = "")
        removeModal()
    })
    
    ## Update Survey Question Properties Button
    observeEvent(input$Q_updateQuestionProperties, {
        req(input$questionColumns_rows_selected)
        selected<-input$questionColumns_rows_selected
        
        questionProperties$values[[selected]][["questionFormDescriptor"]]<-formatDescriptor(
            descriptor = "questionFormDescriptors",
            baseURL = input$ODS_URL,
            choice = input[["Q_questionFormDescriptor"]],
            token = myToken()
        )
        
        map(c("questionCode",
              "questionText",
              #"questionFormDescriptor",
              "responseChoices"
              ),
            function(x){
                if(x=="responseChoices" &
                   "textValue" %in% names(questionProperties$values[[selected]][[x]])){
                    print(paste0("no error yet at ", x))
                    print("Old values:")
                    print(questionProperties$values[[selected]][[x]])
                    print("Supposed new values:")
                    print(data.frame(
                        "sortOrder" = seq_along(input[[paste0("Q_", x)]]),
                        "textValue" = input[[paste0("Q_", x)]]))
                    questionProperties$values[[selected]][[x]]<-data.frame(
                        "sortOrder" = seq_along(input[[paste0("Q_", x)]]),
                        "textValue" = input[[paste0("Q_", x)]])
                    #questionProperties$values[[selected]][[x]][["textValue"]]<-input[[paste0("Q_",x)]]
                    print(questionProperties$values[[selected]][[x]])
                }else{
                    questionProperties$values[[selected]][[x]]<-input[[paste0("Q_",x)]]
                }
            })
        columnHeaders$values$Questions[selected]<-input[["Q_questionText"]]
        print(questionProperties$values[[selected]][["questionCode"]])#<-input[[paste0("Q_","questionCode")]]
    })
    
    ## Preview Survey Question JSON ##
    surveyQuestionsJSON<-reactiveValues()
    
    # observeEvent(input$Q_previewQuestionProperties, {
    #     surveyQuestionsJSON$JSON<-
    #         map(questionProperties$values, function(x){ # List all properties except original colname
    #             x[-which(names(x) %in% "originalColumnName")]})%>%
    #         map(function(x){                        # If no responseChoices value...
    #             if((!length(x[["responseChoices"]])) &
    #                "responseChoices" %in% names(x)){ # Omit responseChoices property
    #                 x[-which(names(x) %in% "responseChoices")]
    #             }else{x}
    #         })
    # })
    
    # output$testQProps<-renderText({surveyQuestionsJSON$JSON%>%toJSON(auto_unbox = T, pretty = T)})
    # output$testQPropsUI<-renderUI({verbatimTextOutput("testQProps")})
    
    ## POST Survey Questions to ODS ##
    SQ_logPOST<-reactiveValues()
    observeEvent(input$Q_ODSPOST, {
        
        ## Transform Data
        surveyQuestionsJSON$JSON<-
            map(questionProperties$values, function(x){ # List all properties except original colname
                x[-which(names(x) %in% "originalColumnName")]})%>%
            map(function(x){                        # If no responseChoices value...
                if((!length(x[["responseChoices"]])) &
                   "responseChoices" %in% names(x)){ # Omit responseChoices property
                    x[-which(names(x) %in% "responseChoices")]
                }else{x}
            })
        
        ## Initialize Progress Bar
        progressSweetAlert(
            session = session, id = "SQ_POSTprogress",
            title = "Loading Survey Questions",
            display_pct = TRUE, value = 0, striped = T
        )
        
        PostSurveyQuestions<-list()
        
        # POST Data
        for(i in seq_along(surveyQuestionsJSON$JSON)){
            PostSurveyQuestions[[i]]<-POST(paste0(input$ODS_URL, "surveyQuestions"), 
                                             add_headers("Authorization" = paste0("Bearer ", 
                                                                                  myToken()$credentials$access_token),
                                                         "Content-Type" = "application/json"),
                                             body = toJSON(
                                                 surveyQuestionsJSON$JSON[[i]], 
                                                           auto_unbox = T, pretty = T)
            )
            
            # Update Progress Bar
            updateProgressBar(
                session = session,
                id = "SQ_POSTprogress",
                value = 100*(i/(length(seq_along(surveyQuestionsJSON$JSON))))
            )
        }
        
        # Reproduce POST for log file
        SQ_logPOST$results<-toJSON(
            imap(
                PostSurveyQuestions, 
                ~list("POST_Body" = toJSON(surveyQuestionsJSON$JSON[[.y]], 
                                           auto_unbox = T, pretty = T),
                      "Status" = .x$status,
                      "Headers" = .x$headers)
                ), 
            auto_unbox = T, pretty = T)
        
        # Close Progress Bar
        closeSweetAlert(session = session)
        
        # Confirmation Message
        sendSweetAlert(
            session = session,
            title = "Survey Questions Submitted!", 
            text = paste0("Remember to download the log file! (There were ", 
                          ifelse(length(SQ_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow()),
                                 SQ_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow(),
                                 0),
                          " errors.)"),
            type = "success"
        )
    })
    
    # Download POST Results
    output$SQ_downloadPostLog <- downloadHandler(
        filename = function() {
            paste0("ODS_Load_SurveyQuestions_", input$surveyIdentifier, "_", 
                   Sys.time()%>%str_replace_all(":", "_"), ".json")
        },
        content = function(file) {
            write(SQ_logPOST$results, file)
        }
    )
    
    ########################
    ### Survey Responses ###
    ########################
    
    # Survey & Respondent Type Drop-downs
    output$Responses_ODSSurveyUI<-renderUI({
        
        list(column(width = 3,pickerInput("Responses_ODSSurvey", "Choose Survey", 
                    choices = ODSSurveys(), multiple = F,
                    options = pickerOptions(liveSearch = T)) %>% tipify(
                        title = "This is a list of all known surveys in the Data Store you connected to in Step 1. If the survey you\\'re looking for does not appear here, be sure you have uploaded the survey metadata for it using the \\'Survey Metadata\\' tab at left (step 2) and try clicking \\'Refresh Connection\\'. Items are formatted as: <i>Survey Name (Survey Unique Identifier)</i>",
                        placement = "top"
                    )),
             column(width = 3,
             selectInput("Responses_RespondentTypes", "Respondent Type(s)*",
                         choices = c("Student", "Staff", "Parent", NA),
                         multiple = F) %>% tipify(
                             title = "If a person ID was collected in this survey, use this dropdown to specify which type (a new dropdown will appear below to choose which column from your file upload contains the ID). Choose \\'NA\\' if no Student/Parent/Staff ID was collected.",
                             placement = "top"
                         )
             )
        )
    })
    
    # Selected Survey ID Reactive
    Responses_selectedODSSurvey<-reactive({
        req(ODSSurveys())
        req(input$Responses_ODSSurvey)
        substr(input$Responses_ODSSurvey, 
               str_locate(input$Responses_ODSSurvey, " \\(ID: ")[2]+1,
               nchar(input$Responses_ODSSurvey)-1)
    })
    
    # Survey Response Identifier - Mandatory UI Fields for both surveyResponse and surveyQuestionResponse 
    output$Responses_surveyResponseIdentifierUI<-renderUI({
        req(input$Responses_ODSSurvey)
        req(input$Responses_RespondentTypes)
        
        list(
            column(width = 3, 
        map(seq_along(input$Responses_RespondentTypes), function(x){
            selectInput(paste0("Responses_RespondentType_", input$Responses_RespondentTypes[x]),
                        paste0("Column holding ", input$Responses_RespondentTypes[x], " ID"),
                        choices = c("Not Applicable", fileUpload()%>%colnames()),
                        selected = ifelse(stringsim(paste0(input$Responses_RespondentTypes[x], " ID"),
                                                    fileUpload()%>%colnames(), method = "jaccard")%>%max()>0.75,
                                          colnames(fileUpload())[
                                              stringsim(paste0(input$Responses_RespondentTypes[x], " ID"),
                                                        fileUpload()%>%colnames(), method = "jaccard")%>%which.max()],
                                          "Not Applicable")
            ) %>% tipify(
                title = paste0("Which column from your file uploads contains the respondent\\'s ", input$Responses_RespondentTypes[x], " ID? <i>If none, choose \\'Not Applicable\\'</i>."),
                placement = "bottom"
            )
        })),
        # Start Time
        column(width = 3,
        selectInput("Responses_StartTime", "Column Holding Survey Start Date/Timestamp",
                    choices = c("Not Applicable", fileUpload()%>%colnames()),
                    selected = colnames(fileUpload())[
                        stringsim("Start Time",
                                  fileUpload()%>%colnames(), method = "jaccard")%>%which.max()]) %>% tipify(
                                      title = "OPTIONAL! If one of your columns has a survey start timestamp, choose it here. If this dropdown and the one below are both given a column with timestamp data, the app will try to calculate and upload a Total Response Time variable (in seconds) for each respondent. Choose \\'Not Applicable\\' for this if there is no start timestamp (or if you have a start <u>date</u> but no timestamp)",
                                      placement = "top"
                                  )),
        # End Time
        column(width = 3,
               radioButtons("Responses_EndTimeType", "Survey End Date Data Source", 
                            choices = c("Column from my File Upload", "I'll pick a date manually"), 
                            selected = "Column from my File Upload"),
               uiOutput("Responses_SurveyEndDateUI")
        # selectInput("Responses_EndTime", "Column Holding Survey End Date/Timestamp*",
        #             choices = c(#"Not Applicable", ## Removed 'Not Applicable' option, as this is an Ed-Fi-required property
        #                 fileUpload()%>%colnames()),
        #             selected = colnames(fileUpload())[
        #                 stringsim("Completion Time",
        #                           fileUpload()%>%colnames(), method = "jaccard")%>%which.max()]) %>% tipify(
        #                               title = "REQUIRED! A response date must be recorded for each survey response. If your survey did not collect this information, you may need to add a column to your file with an assumed response date and re-upload (Step 1, at left). Preferred format is yyyy-mm-dd, though other standard unambiguous character formats may suffice (e.g. \\'m/dd/yyyy hh:mm:ss\\').",
        #                               placement = "top"
        #                           )
        )
        )
    })
    
    # Survey End Date Dynamic UI
    output$Responses_SurveyEndDateUI<-renderUI({
        if(input$Responses_EndTimeType=="Column from my File Upload"){
            selectInput("Responses_EndTime", "Column Holding Survey End Date/Timestamp*",
                        choices = c(#"Not Applicable", ## Removed 'Not Applicable' option, as this is an Ed-Fi-required property
                            fileUpload()%>%colnames()),
                        selected = colnames(fileUpload())[
                            stringsim("Completion Time",
                                      fileUpload()%>%colnames(), method = "jaccard")%>%which.max()]) %>% tipify(
                                          title = "REQUIRED! A response date must be recorded for each survey response. If your survey did not collect this information, you may need to add a column to your file with an assumed response date and re-upload (Step 1, at left). Preferred format is yyyy-mm-dd, though other standard unambiguous character formats may suffice (e.g. \\'m/dd/yyyy hh:mm:ss\\').",
                                          placement = "top"
                                      )
        }else{
            dateInput("Responses_EndDateManualInput", "End Date")
        }
    })
    
    # Survey Response Property Mapping (UI)
    output$Responses_SurveyResponseUI<-renderUI({
        req(input$Responses_ODSSurvey)
        req(input$Responses_RespondentTypes)
        list(
           # numericInput("SR_fileMinRow", "From Row", value = 1, min = 1, max = input$SR_fileMaxRow),
           # numericInput("SR_fileMaxRow", "To Row", value = nrow(fileUpload()), 
        #                 min = input$SR_fileMinRow, max = nrow(fileUpload())),
        #     map(seq_along(input$Responses_RespondentTypes), function(x){
        #     selectInput(paste0("Responses_RespondentType_", input$Responses_RespondentTypes[x]),
        #                 paste0("Column holding ", input$Responses_RespondentTypes[x], " ID"),
        #                 choices = c("Not Applicable", fileUpload()%>%colnames()),
        #                 selected = ifelse(stringsim(paste0(input$Responses_RespondentTypes[x], " ID"),
        #                                             fileUpload()%>%colnames(), method = "jaccard")%>%max()>0.75,
        #                                   colnames(fileUpload())[
        #                     stringsim(paste0(input$Responses_RespondentTypes[x], " ID"),
        #                               fileUpload()%>%colnames(), method = "jaccard")%>%which.max()],
        #                     "Not Applicable")
        #                 ) %>% tipify(
        #                     title = paste0("Which column from your file uploads contains the respondent\\'s ", input$Responses_RespondentTypes[x], " ID? <i>If none, choose \\'Not Applicable\\'</i>."),
        #                     placement = "right"
        #                 )
        # }),
        # # Start Time
        # selectInput("Responses_StartTime", "Column Holding Survey Start Date/Timestamp",
        #             choices = c("Not Applicable", fileUpload()%>%colnames()),
        #             selected = colnames(fileUpload())[
        #                 stringsim("Start Time",
        #                           fileUpload()%>%colnames(), method = "jaccard")%>%which.max()]) %>% tipify(
        #                               title = "OPTIONAL! If one of your columns has a survey start timestamp, choose it here. If this dropdown and the one below are both given a column with timestamp data, the app will try to calculate and upload a Total Response Time variable (in seconds) for each respondent. Choose \\'Not Applicable\\' for this if there is no start timestamp (or if you have a start <u>date</u> but no timestamp)",
        #                               placement = "right"
        #                           ),
        # # End Time
        # selectInput("Responses_EndTime", "Column Holding Survey End Date/Timestamp*",
        #             choices = c(#"Not Applicable", ## Removed 'Not Applicable' option, as this is an Ed-Fi-required property
        #                         fileUpload()%>%colnames()),
        #             selected = colnames(fileUpload())[
        #                 stringsim("Completion Time",
        #                           fileUpload()%>%colnames(), method = "jaccard")%>%which.max()]) %>% tipify(
        #                               title = "REQUIRED! A response date must be recorded for each survey response. If your survey did not collect this information, you may need to add a column to your file with an assumed response date and re-upload (Step 1, at left). Preferred format is yyyy-mm-dd, though other standard unambiguous character formats may suffice (e.g. \\'m/dd/yyyy hh:mm:ss\\').",
        #                               placement = "right"
        #                           ),
        # Respondent Full Name
        selectInput("Responses_RespondentFullName", "Column Holding Respondent Full Name",
                    choices = c("Not Applicable", fileUpload()%>%colnames()),
                    selected = colnames(fileUpload())[
                        stringsim("Full Name",
                                  fileUpload()%>%colnames(), method = "jaccard")%>%which.max()]) %>% tipify(
                                      title = "Choose \\'Not Applicable\\' if none. Remember, if, e.g., a student ID is collected (and specified above), this response will already be associated with the matching person record in the Data Store.",
                                      placement = "right"
                                  ),
        # Respondent Email
        selectInput("Responses_RespondentEmail", "Column Holding Respondent Email",
                    choices = c("Not Applicable", fileUpload()%>%colnames()),
                    selected = colnames(fileUpload())[
                        stringsim("Email",
                                  fileUpload()%>%colnames(), method = "lcs")%>%which.max()]) %>% tipify(
                                      title = "Choose \\'Not Applicable\\' if none.",
                                      placement = "right"
                                  ),
        # Respondent Location
        selectInput("Responses_RespondentLocation", "Column Holding Respondent Location",
                    choices = c("Not Applicable", fileUpload()%>%colnames()),
                    selected = "Not Applicable") %>% tipify(
                        title = "Choose \\'Not Applicable\\' if none.",
                        placement = "right"
                    ),
        actionBttn("Responses_PreviewSurveyResponseJSON", "Upload!", 
                   style = "material-flat", color = "success")#,
        # actionBttn("Responses_POSTsurveyResponses", "POST",
        #            style = "material-flat", color = "royal")
        )
    })
    
    ## Response Date Format Checker ##
    
    #### LEFT OFF 6/27: Still need to handle HH:MM:SS if colons in input format
    #                   AND handle complete misses (e.g. user or app selected wrong column entirely)
    #                   AND maybe activate this after hitting the upload button (currently triggers on load, which is annoying)
    #                   AND maybe an input for user-selected date, if no date column in file.
    observeEvent(input$Responses_EndTime, {
        if(input$Responses_EndTime!="Not Applicable"){
            #checkMe<-fileUpload()[[input$Responses_EndTime]][1]
            
            checkDate<-function(x){
                checkMe<-tryCatch(as_datetime(x))
                # If plain conversion didn't work, then...
                if(is.na(checkMe)){
                    # If we find 2 slashes in the value...
                    if(str_count(x, "\\/")==2){
                        #slashies<-(str_locate_all(x, "\\/") %>% pluck(1))[,1] %>% reduce(paste0)
                        tempMonth<-x %>% str_extract("^.?.?\\/") %>% str_remove("\\/")
                        tempDay<-x %>% str_extract("\\/.?.?\\/") %>% str_remove_all("\\/")
                        tempYear<-x %>% str_remove("^.*\\/.?.?\\/") %>% str_remove_all("\\/") %>% str_remove("\\s.*")
                        
                        tempDate<-paste0(
                            ifelse(nchar(tempYear)==2, paste0("20", tempYear), tempYear),
                            "-",
                            ifelse(nchar(tempMonth)==1, paste0("0", tempMonth), tempMonth),
                            "-",
                            ifelse(nchar(tempDay)==1, paste0("0", tempDay), tempDay)
                        )
                        
                        dateOut<-paste(tempDate, x %>% paste0(" ") %>% str_remove("^.*\\s")) %>% str_trim()
                        return(dateOut)
                    }
                }else{
                    return(x)
                }
            }
            
            #if(input$Responses_EndTimeType=="Column from my File Upload"){
            # testValue<-fileUpload()[[input$Responses_EndTime]][1]
            # out<-checkDate(testValue)
            # if(testValue!=checkDate(testValue)){
            #     sendSweetAlert(session, "Hey, Look!", HTML(paste0(
            #         "Response Date needs to be in a particular format for this to work.
            #         <br><br>We tried converting the dates for you and here's what we came up with:
            #         <br><b>Original Value</b>: ", testValue, "
            #         <br><b>Reformatted</b>: ", out, "
            #         <br><br>If this looks like the same date and time, carry on! If not, try reformatting this column to yyyy-mm-dd format in your source file and re-uploading (step 1) before you proceed.")),
            #         type = "warning"
            #     )
            # }
            #}

        }
        
    })
    
    
    ## Survey Response JSON ##
    surveyResponseJSON<-reactiveValues()
    observeEvent(input$Responses_PreviewSurveyResponseJSON, {
        # Configure property<--> input ID match-up
        inputMap<-list(
            parentReference = "Responses_RespondentType_Parent",
             studentReference = "Responses_RespondentType_Student",
             staffReference = "Responses_RespondentType_Staff",
             surveyReference = "Responses_ODSSurvey",
             electronicMailAddress = "Responses_RespondentEmail",
             fullName = "Responses_RespondentFullName",
             location = "Responses_RespondentLocation",
             responseDate = "Responses_EndTime"
        )
        
        # Sub in user-selected date if manual date type chosen
        if(input$Responses_EndTimeType!="Column from my File Upload"){
            inputMap[["responseDate"]]<-input$Responses_EndDateManualInput
        }
        
       
        
        # ## Create Progress Bar
        # progressSweetAlert(
        #     session = session, id = "SR_PreviewProgress",
        #     title = "Transforming Survey Response Data",
        #     display_pct = TRUE, value = 0, striped = T
        # )
        
        ##############
        ## ORIGINAL ##
        ##############
        
        ## Create list item in proper format for each response
        surveyResponseJSON$list<-list()
        
        # for(i in seq_len(nrow(fileUpload()))){
        #     surveyResponseJSON$list[[i]] =  
        #         list(
        #             # Construct Unique Survey Response Identifier
        #             "surveyResponseIdentifier" = paste0(
        #             Responses_selectedODSSurvey(), switch(input$Responses_RespondentTypes, 
        #         "Student" = ifelse(input[[inputMap$studentReference]]=="Not Applicable",
        #                           i,
        #                           fileUpload()[[input[[inputMap$studentReference]]]][i]),
        #         "Staff" = ifelse(input[[inputMap$staffReference]]=="Not Applicable",
        #                         i,
        #                         fileUpload()[[input[[inputMap$staffReference]]]][i]),
        #         "Parent" = ifelse(input[[inputMap$parentReference]]=="Not Applicable",
        #                          i,
        #                          fileUpload()[[input[[inputMap$parentReference]]]][i])
        #     ),
        #     ifelse(input[[inputMap$responseDate]]=="Not Applicable", 
        #            "", 
        #            fileUpload()[[input[[inputMap$responseDate]]]][i]%>%
        #                as_datetime()%>%as.integer())
        #     ),
        #     # Set survey reference
        #     "surveyReference" = list(
        #         "namespace" = getODSResource("surveys", input$ODS_URL, myToken(), 
        #                                      moreParameters = paste0("surveyIdentifier=",
        #                                                              Responses_selectedODSSurvey()))%>%
        #             select(contains("namespace"))%>%
        #             head(1)%>%
        #             as.vector()%>%
        #             unlist(),
        #         "surveyIdentifier" = Responses_selectedODSSurvey()
        #     ),
        #     
        #     # Set Response Date (Should this be conditional?)
        #     "responseDate" = fileUpload()[[input[[inputMap$responseDate]]]][i]%>%
        #         as.Date()
        #         )
        #     
        #     # Set person ID references (student, parent, staff) if applicable
        #     if(input$Responses_RespondentTypes!="NA"){
        #         map(tolower(input$Responses_RespondentTypes), function(x){
        #             if(input[[inputMap[[paste0(x, "Reference")]]]]!="Not Applicable"){
        #                 tempList<-list()
        #                 tempList[[paste0(x, "UniqueId")]] <- fileUpload()[[input[[inputMap[[paste0(x, "Reference")]]]]]][i]
        #                 surveyResponseJSON$list[[i]][[paste0(x, "Reference")]] = tempList
        #                 
        #             }
        #         })
        #     }
        #     
        #     # Add Email if included in results
        #     if(input$Responses_RespondentEmail!="Not Applicable"){
        #         surveyResponseJSON$list[[i]][["electronicMailAddress"]]<-fileUpload()[[
        #             input[[inputMap[["electronicMailAddress"]]]]]][i]
        #     }
        #     
        #     # Add Name if included in results
        #     if(input[[inputMap[["fullName"]]]]!="Not Applicable"){
        #         surveyResponseJSON$list[[i]][["fullName"]]<-fileUpload()[[
        #             input[[inputMap[["fullName"]]]]]][i]
        #     }
        #     
        #     # Add Location if included in results
        #     if(input[[inputMap[["location"]]]]!="Not Applicable"){
        #         surveyResponseJSON$list[[i]][["location"]]<-fileUpload()[[
        #             input[[inputMap[["location"]]]]]][i]
        #     }
        #     
        #     # Add Response Time (in seconds) if start/end time columns exist and are valid
        #     if(!("Not Applicable" %in% c(input$Responses_StartTime, input$Responses_EndTime))){
        #         if(as_datetime(fileUpload()[[
        #             input$Responses_StartTime]][i])%>%is.na()==F &
        #            as_datetime(fileUpload()[[
        #                input$Responses_EndTime]][i])%>%is.na()==F
        #            ){
        #             surveyResponseJSON$list[[i]][["responseTime"]]<-(as_datetime(
        #                 fileUpload()[[input$Responses_EndTime]][i])-as_datetime(
        #                     fileUpload()[[input$Responses_StartTime]][i]))%>%
        #                 as.period()%>%period_to_seconds()
        #         }
        #     }
        # 
        #     # Update Progress Bar
        #     updateProgressBar(
        #         session = session,
        #         id = "SR_PreviewProgress",
        #         value = 100*(i/nrow(fileUpload()))
        #     )
        # }
        # 
        # # Close Progress Bar
        # closeSweetAlert(session = session)
        # 
         surveyResponseJSON$cursor<-1
        
        ##########################
        ### PARALLEL WORKSPACE ###
        ##########################
        
        clust<-makeCluster(detectCores()-1)
        print("Successfully spun up clusters")
        currentUser<-currentUser() 
        print(input$Responses_EndTime)
        print(input$Responses_EndTimeType)
        #print(input[[inputMap$responseDate]])
        print(length(input$Responses_EndTimeType))
        
        
        clusterInputs<-list(
            input[[inputMap$studentReference]],
            input[[inputMap$staffReference]],
            input[[inputMap$parentReference]],
            #input[[inputMap$responseDate]],
            input[["ODS_URL"]],
            myToken(),
            Responses_selectedODSSurvey(),
            input$Responses_RespondentTypes,
            input$studentReference,
            input$staffReference,
            input$parentReference,
            input$Responses_RespondentEmail,
            input[[inputMap[["fullName"]]]],
            input[[inputMap[["location"]]]],
            input$Responses_StartTime,
            #input$Responses_EndTime,
            input$Responses_EndTimeType
        )
        
        print("Created clusterInput list")
        
        #print(clusterInputs[[inputMap$responseDate]])
        
        names(clusterInputs)<-c(
            paste0(inputMap$studentReference),
            paste0(inputMap$staffReference),
            paste0(inputMap$parentReference),
            #paste0(inputMap$responseDate),
            "ODS_URL",
            "myToken",
            "responses_selectedODSSurvey",
            "Responses_RespondentTypes",
            "StudentReference",
            "StaffReference",
            "ParentReference",
            "responses_RespondentEmail",
            paste0(inputMap[["fullName"]]),
            paste0(inputMap[["location"]]),
            "responses_StartTime",
            #"responses_EndTime",
            "responses_EndTimeType"
        )
        
        # Sub in column-based date if manual date type not chosen
        if(input$Responses_EndTimeType!="Column from my File Upload"){
            clusterInputs[[paste0(inputMap$responseDate)]]<-inputMap$responseDate
            clusterInputs[["responses_EndTime"]]<-input$Responses_EndTimeManualInput
        }else{
            clusterInputs[[paste0(inputMap$responseDate)]]<-input[[inputMap$responseDate]]
            clusterInputs[["responses_EndTime"]]<-input$Responses_EndTime
        }
        
        
        print("Created variable list to send to clusters")
        
        postID<-UUIDgenerate()
        
        clusterFileUpload<-fileUpload()
        
        # clusterExport(clust, varlist = c("inputMap", 
        #                        "clusterFileUpload"
        #                        ))
        # 
        
        
        clusterExport(clust, c("as_datetime", 
                               "write_file",
                               "%>%",
                               "map",
                               "sha1",
                               "select",
                               #"sendSweetAlert",
                               "UUIDgenerate",
                               #"getODSResource",
                               "dbConnect",
                               "dbAppendTable"
        ))
        
        print("Exported global objects to clusters")
        
        clusterExport(clust, c("clusterInputs", 
                               "getODSResource", 
                               "clusterFileUpload", 
                               "inputMap", 
                               "postID",
                               "currentUser"), envir = rlang::current_env())
        print("Explored local objects to clusters")

        #clusterEvalQ(clust, library(dplyr))
        clusterEvalQ(clust, library(httr))
        clusterEvalQ(clust, library(jsonlite))
        clusterEvalQ(clust, library(utils))
        clusterEvalQ(clust, library(RSQLite))
        
        print("Loaded libraries to clusters")
        
        prepSurveyResponse<-function(i){
            tempFunction<-function(j){
                tempOutput<-list(
                    # Construct Unique Survey Response Identifier
                    "surveyResponseIdentifier" = paste0(
                        ifelse(nchar(clusterInputs$responses_selectedODSSurvey)>40,
                               sha1(clusterInputs$responses_selectedODSSurvey),
                               clusterInputs$responses_selectedODSSurvey), switch(clusterInputs$Responses_RespondentTypes, 
                                                                          "Student" = ifelse(clusterInputs[[inputMap$studentReference]]=="Not Applicable",
                                                                                             j,
                                                                                             clusterFileUpload[[clusterInputs[[inputMap$studentReference]]]][j]),
                                                                          "Staff" = ifelse(clusterInputs[[inputMap$staffReference]]=="Not Applicable",
                                                                                           j,
                                                                                           clusterFileUpload[[clusterInputs[[inputMap$staffReference]]]][j]),
                                                                          "Parent" = ifelse(clusterInputs[[inputMap$parentReference]]=="Not Applicable",
                                                                                            j,
                                                                                            clusterFileUpload[[clusterInputs[[inputMap$parentReference]]]][j])
                        ),
                               ifelse(clusterInputs$responses_EndTimeType=="Column from my File Upload",
                                      clusterFileUpload[[clusterInputs[[inputMap$responseDate]]]][j]%>%
                                   as_datetime()%>%as.integer(),
                                   inputMap$responseDate)
                    ),
                    # Set survey reference
                    "surveyReference" = list(
                        "namespace" = getODSResource("surveys", clusterInputs$ODS_URL, clusterInputs$myToken, 
                                                     moreParameters = paste0("surveyIdentifier=",
                                                                             clusterInputs$responses_selectedODSSurvey))%>%
                            select(contains("namespace"))%>%
                            head(1)%>%
                            as.vector()%>%
                            unlist(),
                        "surveyIdentifier" = clusterInputs$responses_selectedODSSurvey
                    ),
                    
                    # Set Response Date (Should this be conditional?)
                    "responseDate" = ifelse(clusterInputs$responses_EndTimeType=="Column from my File Upload",
                                            clusterFileUpload[[clusterInputs[[inputMap$responseDate]]]][j]%>%
                                                as.Date() %>% as.character(),#as_datetime(),#%>%as.integer(),
                                            inputMap$responseDate %>% as.character())
                )
                
                
                # Set person ID references (student, parent, staff) if applicable
                if(clusterInputs$Responses_RespondentTypes!="NA"){
                    map(#tolower(clusterInputs$responses_RespondentTypes)
                        clusterInputs$Responses_RespondentTypes, function(x){
                            if(clusterInputs[[inputMap[[paste0(tolower(x), "Reference")]]]]!="Not Applicable"){
                                tempList<-list()
                                tempList[[paste0(tolower(x), "UniqueId")]] <- clusterFileUpload[[clusterInputs[[inputMap[[paste0(tolower(x), "Reference")]]]]]][j] %>% as.character()
                                tempOutput[[paste0(tolower(x), "Reference")]] <- tempList
                                
                                assign("tempOutput", tempOutput, envir = rlang::env_parent(rlang::current_env()))
                                
                            }
                        })
                }
                
                
                
                # Add Email if included in results
                if(clusterInputs$responses_RespondentEmail!="Not Applicable"){
                    tempOutput[["electronicMailAddress"]]<-clusterFileUpload[[
                        clusterInputs$responses_RespondentEmail]][j] #%>% URLencode(reserved = T)
                }
                
                # Add Name if included in results
                if(clusterInputs[[inputMap[["fullName"]]]]!="Not Applicable"){
                    tempOutput[["fullName"]]<-clusterFileUpload[[
                        clusterInputs[[inputMap[["fullName"]]]]]][j]
                }
                
                # Add Location if included in results
                if(clusterInputs[[inputMap[["location"]]]]!="Not Applicable"){
                    tempOutput[["location"]]<-clusterFileUpload[[
                        clusterInputs[[inputMap[["location"]]]]]][j]
                }
                
                # Add Response Time (in seconds) if start/end time columns exist and are valid
                # if(!("Not Applicable" %in% c(clusterInputs$responses_StartTime, clusterInputs$responses_EndTime))){
                #     if(as_datetime(clusterFileUpload[[
                #         clusterInputs$responses_StartTime]][i])%>%is.na()==F &
                #        as_datetime(clusterFileUpload[[
                #            clusterInputs$responses_EndTime]][i])%>%is.na()==F
                #     ){
                #         tempOutput[["responseTime"]]<-(as_datetime(
                #             clusterFileUpload[[clusterInputs$responses_EndTime]][i])-as_datetime(
                #                 clusterFileUpload[[clusterInputs$responses_StartTime]][i]))%>%
                #             as.period()%>%period_to_seconds()
                #     }
                # }
                
                
                #debug
                # write.csv(summaryRprof()$by.self %>% as.data.frame(), paste0("www/", UUIDgenerate(), ".csv"))
                
                
                
                return(tempOutput)
            }
            

            output<-list()
            
            # If vector was fed to function, recurse over each item in vector
            if(length(i)>1){
                output<-map(i, prepSurveyResponse)
            }else{
                # Otherwise, transform input and POST to ODS
                output<-tempFunction(j=i)
                lastOut<-POST(paste0(clusterInputs$ODS_URL, "surveyResponses"),
                              add_headers("Authorization" = paste0("Bearer ",
                                                                   clusterInputs$myToken$credentials$access_token),
                                          "Content-Type" = "application/json"),
                              body = toJSON(
                                  output,
                                  auto_unbox = T, pretty = T)
                )
                
                # Save a log of how it went
                
                return(data.frame(
                    postID = paste0(currentUser, postID),
                    sessionID = userSessionID,
                    timestamp = Sys.time(),
                    postBody = toJSON(output, auto_unbox = T, pretty = T) %>% as.character(),
                    headers = lastOut$headers %>% toJSON(auto_unbox = T, pretty = T) %>% as.character(),
                    apiResponse = lastOut$status %>% as.character()
                ))
                
            }
            
            #return(lastOut)
            
            #return(output)
            
        }
        
        print("Generated survey response transform load function")
        
        
        #runPrep<-function(x){for(i in x){prepSurveyResponse(i)}}
        ## Run Parallel SurveyResponse Function
        
         tempOut<-pblapply(seq_len(nrow(fileUpload())),# %>% split(length(clust)),#function(x){map(x, prepSurveyResponse)}
                           prepSurveyResponse
                           , cl = clust
                           ) %>% reduce(rbind)
         
         # ...And return a note about how it went
         con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
         dbAppendTable(con, "postLog", 
                       tempOut
         )

         sendSweetAlert(session, "Done!", paste0("Load complete. There were ", tempOut %>% filter(!apiResponse %>% str_detect("20")) %>% nrow(),
                                                 " errors. Upload results are shown in the table below."))
         
         if(length(tempOut)>1){
             surveyResponseJSON$list<-tempOut
         }else{
             surveyResponseJSON$list<-tempOut[[1]]
         }
        
        #surveyResponseJSON$list<-pblapply(seq_len(nrow(fileUpload())), prepSurveyResponse)
        print(length(surveyResponseJSON$list))
        
        #surveyResponseJSON$list<-parLapply(clust, seq_len(nrow(fileUpload())), prepSurveyResponse)
        
        ## Kill Parallel Processes
        stopCluster(clust)
        

    })
    
    # Survey Response JSON Cursor
    observeEvent(input$SR_previous, {
        if(surveyResponseJSON$cursor>1){
            surveyResponseJSON$cursor<-surveyResponseJSON$cursor-1
        }
    })
    observeEvent(input$SR_next, {
        if(surveyResponseJSON$cursor<nrow(fileUpload())){
            surveyResponseJSON$cursor<-surveyResponseJSON$cursor+1
        }
    })
    
    # Survey Response Output Object
    output$responses_surveyResponseJSON<-renderDT({
        #print(surveyResponseJSON$list)
         # toJSON(surveyResponseJSON$list[[surveyResponseJSON$cursor]],
         #                                                     pretty = T, auto_unbox = T)
        #""
        if(length(surveyResponseJSON$list)){
            surveyResponseJSON$list <- surveyResponseJSON$list %>% 
                mutate(Status = case_when(
                    apiResponse == 200 ~ "Updated (200)",
                    apiResponse == 201 ~ "Created (201)",
                    TRUE~paste0("Error (", apiResponse, ")")
                )) %>% 
                relocate(
                    Status,
                    postBody,
                    timestamp,
                    headers,
                    postID,
                    sessionID
                ) #%>% 
                # rename(
                #     "Content" = postBody,
                #     "Timestamp" = timestamp,
                #     "Headers" = headers,
                #     "Post ID" = postID,
                #     "Session ID" = sessionID
                # ) #%>% 
                # select(
                #     Status,
                #     "Content" = postBody,
                #     "Timestamp" = timestamp,
                #     "Headers" = headers,
                #     "Post ID" = postID,
                #     "Session ID" = sessionID
                # )
        }
        surveyResponseJSON$list 
    }, 
    rownames = F,
    filter = list(position = 'top', clear = FALSE),
    options = list(scrollY = "350px",
                   search = list(regex = TRUE)),
    fillContainer = T)
    
    ## POST Survey Responses to ODS ##
    SR_logPOST<-reactiveValues()
    
    observeEvent(input$Responses_POSTsurveyResponses, {
        
        progressSweetAlert(
            session = session, id = "SR_POSTprogress",
            title = "Loading Survey Responses",
            display_pct = TRUE, value = 0, striped = T
        )
        
        PostSurveyResponses<-list()
        
        # POST Data
        for(i in seq_along(surveyResponseJSON$list)){
            PostSurveyResponses[[i]]<-POST(paste0(input$ODS_URL, "surveyResponses"), 
                                           add_headers("Authorization" = paste0("Bearer ", 
                                                                                myToken()$credentials$access_token),
                                                       "Content-Type" = "application/json"),
                                           body = toJSON(
                                               surveyResponseJSON$list[[i]], 
                                               auto_unbox = T, pretty = T)
            )
            
            # Update Progress Bar
            updateProgressBar(
                session = session,
                id = "SR_POSTprogress",
                title = paste0("Loading Survey Responses",
                    "(Response ", i, " of ", 
                               length(seq_along(surveyResponseJSON$list)), ")"),
                value = 100*(i/(length(seq_along(surveyResponseJSON$list))))
            )
        }
        
        # Reproduce POST for log file
        SR_logPOST$results<-toJSON(
            imap(
                PostSurveyResponses, 
                ~list("POST_Body" = toJSON(surveyResponseJSON$list[[.y]], 
                                           auto_unbox = T, pretty = T),
                      "Status" = .x$status,
                      "Headers" = .x$headers)
            ), 
            auto_unbox = T, pretty = T)
        
        # Close Progress Bar
        closeSweetAlert(session = session)
        
        # Confirmation Message
        sendSweetAlert(
            session = session,
            title = "Survey Responses Submitted!", 
            text = paste0("Remember to download the log file! (There were ", 
                          ifelse(length(SR_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow()),
                                 SR_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow(),
                                 0),
                          " errors.)"),
            type = "success"
        )
    })
    

# Download POST Results
output$SR_downloadPostLog <- downloadHandler(
    filename = function() {
        paste0("ODS_Load_SurveyResponses_", Responses_selectedODSSurvey(), "_", 
               Sys.time()%>%str_replace_all(":", "_"), ".json")
    },
    content = function(file) {
        write(SR_logPOST$results, file)
    }
)
    
    #################################
    ### SURVEY QUESTION RESPONSES ###
    #################################
    
output$SQR_UI<-renderUI({
    req(input$Responses_ODSSurvey)
    
    ## Grab existing questions in ODS for the selected survey
    selectedSurveyQuestions<-getODSResource("surveyQuestions", input$ODS_URL, myToken(),
                                           moreParameters = paste0("&surveyIdentifier=",
                                                                   Responses_selectedODSSurvey()))%>%
        as.data.frame()%>%
        select(questionCode, questionText)
        
    ## Generate smart column selector drop-downs for each survey question
    list(
        column(width = 4,
               headerPanel(" "),
        map(seq_len(nrow(selectedSurveyQuestions)), function(x){
            matchStrength<-ifelse(
                stringsim(selectedSurveyQuestions$questionText[x],
                          fileUpload()%>%colnames(), method = "jaccard") %>% max()<0.85,
                stringsim(selectedSurveyQuestions$questionText[x],
                          fileUpload()%>%colnames(), method = "osa")%>%max(),
                stringsim(selectedSurveyQuestions$questionText[x],
                          fileUpload()%>%colnames(), method = "jaccard")%>%max()
            )
            selectInput(paste0("SQR_Q",x), paste0(ifelse(matchStrength<0.75, "! ", ""), "Column that maps to '",
                                                  selectedSurveyQuestions$questionText[x],
                                                  "'"),
                        choices = c("Not Applicable", fileUpload()%>%colnames()),
                        selected = colnames(fileUpload())[ifelse(
                            stringsim(selectedSurveyQuestions$questionText[x],
                                      fileUpload()%>%colnames(), method = "jaccard") %>% max()<0.85,
                            stringsim(selectedSurveyQuestions$questionText[x],
                                      fileUpload()%>%colnames(), method = "osa")%>%which.max(),
                            stringsim(selectedSurveyQuestions$questionText[x],
                                      fileUpload()%>%colnames(), method = "jaccard")%>%which.max()
                        )
                            ]
                        )
        })),
        column(width = 8,
               headerPanel(" "),
               actionBttn("SQR_Preview", "Upload!", style = "material-flat", size = "sm", color = "success"),
               #actionBttn("SQR_Post", "POST", style = "material-flat", size = "sm", color = "royal"),
               #downloadButton("SQR_downloadPostLog", "Download API POST Log"),
               #actionBttn("SQR_Previous", "<<", size = "xs", style = "material-flat"),
               #actionBttn("SQR_Next", ">>", size = "xs", style = "material-flat"),
               #verbatimTextOutput("SQR_PreviewJSON")
               DTOutput("SQR_PreviewJSON")
               )
    )})

surveyQuestionResponsesJSON<-reactiveValues()

## Prepare Survey Question Response Objects ##
observeEvent(input$SQR_Preview, {
    
    # progressSweetAlert(
    #     session = session, id = "SQR_PreviewProgress",
    #     title = "Loading Data",# "Transforming Data...",
    #     display_pct = TRUE, value = 0, striped = T
    # )
    
    print("attempting inputMap creation")
    # Configure property <--> input ID match-up
    inputMap<-list(
        parentReference = "Responses_RespondentType_Parent",
        studentReference = "Responses_RespondentType_Student",
        staffReference = "Responses_RespondentType_Staff",
        surveyReference = "Responses_ODSSurvey",
        electronicMailAddress = "Responses_RespondentEmail",
        fullName = "Responses_RespondentFullName",
        location = "Responses_RespondentLocation",
        responseDate = "Responses_EndTime"
    )
    print("inputMap Initiated")
    # Sub in user-selected date if manual date type chosen
    if(input$Responses_EndTimeType!="Column from my File Upload"){
        inputMap[["responseDate"]]<-input$Responses_EndDateManualInput
    }
    print("inputMap Updated")
    
    # Instantiate list objects
    surveyQuestionResponsesJSON$list<-list()
    PostSurveyQuestionResponses<-list()
    
    # Cache namespace for selected survey
    tempNamespace<-getODSResource("surveys", input$ODS_URL, myToken(), 
                   moreParameters = paste0("surveyIdentifier=",
                                           Responses_selectedODSSurvey()))%>%
        select(contains("namespace"))%>%
        head(1)%>%
        as.vector()%>%
        unlist()
    
    ## Grab existing questions in ODS for the selected survey
    selectedSurveyQuestions<-getODSResource("surveyQuestions", input$ODS_URL, myToken(),
                                           moreParameters = paste0("&surveyIdentifier=",
                                                                   Responses_selectedODSSurvey()))%>%
        as.data.frame()%>%
        select(questionCode, questionText)
    
    # Initiate parallel processors
    print(nrow(selectedSurveyQuestions))
    clust<-makeCluster(detectCores()-1)
    
    ## Pass required objects and functions to parallel processes
    ODS_URL<-input$ODS_URL
    API_Token<-myToken()$credentials$access_token
    clusterResponses_selectedODSSurvey<-Responses_selectedODSSurvey()
    clusterResponses_RespondentTypes<-input$Responses_RespondentTypes
    clusterStudentReference<-input[[inputMap$studentReference]]
    clusterStaffReference<-input[[inputMap$staffReference]]
    clusterParentReference<-input[[inputMap$parentReference]]
    clusterFileUpload<-fileUpload()
    postID<-UUIDgenerate()
    tokenExpiry<-tokenExpiration$timestamp
    apiKey<-input$apiKey
    apiSecret<-input$apiSecret
    currentUser<-currentUser()
    responses_EndTimeType<-input$Responses_EndTimeType
    
    # Sub in user-selected date if manual date type chosen
    if(input$Responses_EndTimeType!="Column from my File Upload"){
        clusterResponseDate<-input$Responses_EndDateManualInput
    }else{
        clusterResponseDate<-input[[inputMap$responseDate]]
    }
    
    print(tokenExpiry)
    print(Sys.time())
    
    clusterExport(clust, list("POST",
                           "add_headers", 
                           "toJSON",
                           #"dbConnect",
                           #"SQLite",
                           #"dbDisconnect",
                           #"dbAppendTable",
                           "fromJSON",
                           "map",
                           "map_df",
                           "%>%",
                           "as_datetime",
                           "UUIDgenerate",
                           "sha1",
                           "dbConnect",
                           "dbAppendTable"#,
                           #"userSessionID"
    )
    )
    
    clusterExport(clust, list(
        "inputMap",
        "tempNamespace",
        "selectedSurveyQuestions", 
        "ODS_URL", 
        "apiKey",
        "apiSecret",
        "API_Token",
        "tokenExpiry",
        "clusterResponses_selectedODSSurvey",
        "clusterResponses_RespondentTypes",
        "clusterStudentReference",
        "clusterStaffReference",
        "clusterParentReference",
        "clusterFileUpload",
        "userSessionID",
        "postID",
        "currentUser",
        "responses_EndTimeType",
        "clusterResponseDate"),
        envir = rlang::current_env())
    
    ## Send Survey Question / File Field Mapping Input Values to Cluster
    map(seq_len(nrow(selectedSurveyQuestions)), function(x){
        
        ## Create temporary object holding each input value for question mapping fields
        assign(paste0("surveyQuestionInput", x), 
               input[[paste0("SQR_Q",x)]])
        
        ## Create question mapping field objects in parent environment
        assign(paste0("surveyQuestionInput", x), 
               input[[paste0("SQR_Q",x)]], envir = parent.env(rlang::current_env()))
        
        ## Send the values to each parallel cluster
        clusterExport(clust, paste0("surveyQuestionInput", x), 
                      envir = rlang::current_env())
    })
    
    
    ## Load Required Libraries across Cluster
    clusterEvalQ(clust, library(httr))
    clusterEvalQ(clust, library(jsonlite))
    clusterEvalQ(clust, library(utils))
    clusterEvalQ(clust, library(RSQLite))
    clusterEvalQ(clust, library(rlang))
    
    ## Question Response Loader Function
    
    loadQuestionResponse<-function(x, ODS_URL, API_Token){
        currentPost<-POST(
            paste0(
                ODS_URL, "surveyQuestionResponses"), 
            add_headers("Authorization" = paste0("Bearer ", 
                                                 API_Token),
                        "Content-Type" = "application/json"),
            body = toJSON(
                x,
                auto_unbox = T, pretty = T)
        )
        
        
        ## Log POST Result to DB -> UPDATE: Set result df as output from function call, to be logged to DB 
        #ch<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
        # dbAppendTable(ch, "postLog", 
                      data.frame(
                          timestamp = Sys.time(),
                          postBody = toJSON(
                              currentPost$request$options$postfields%>%
                                  rawToChar()%>%
                                  fromJSON(), 
                              auto_unbox = T, pretty = T) %>% 
                              as.character(),
                          headers = currentPost$headers%>% unlist() %>% 
                              paste0(collapse = "\n"),
                          apiResponse = currentPost$status
                      )
        #)
        
    }
    
    
    ## surveyQuestionResponse Construction Function
    # Next step: copy the loop code below and convert to a function that can be called in parallel
    
    ## Construct surveyQuestionResponse Function
    transformLoadSurveyQuestionResponse<-function(i){
        
        if((Sys.time()+120)>tokenExpiry-60){
            
            # Create ODS App and Endpoint Objects
            assign("API_Token", oauth2.0_token(endpoint = oauth_endpoint(
                authorize = "authorize",
                access = "token",
                base_url = paste0(substr(ODS_URL, 1,regexpr("data", ODS_URL)[[1]]-1),"oauth")
            ),
            app = oauth_app(
                appname = "ODS Sandbox",
                key = apiKey,
                secret = apiSecret
            ), 
            client_credentials = T, 
            cache = F), envir = parent.env(current_env())
            )
            
            assign("tokenExpiry", Sys.time()+API_Token$credentials$expires_in, envir = parent.env(current_env()))
            
        }
        
        tempFunction<-function(j){
            
            currentResponse<-list()
            for(k in seq_len(nrow(selectedSurveyQuestions))){
                if(get(paste0("surveyQuestionInput", k))!="Not Applicable"){
                    currentResponse[[k]] = list(
                        "surveyQuestionReference" = list(
                            "namespace" = tempNamespace,
                            "questionCode" = selectedSurveyQuestions$questionCode[k],
                            "surveyIdentifier" = clusterResponses_selectedODSSurvey
                        ),
                        "surveyResponseReference" = list(
                            "namespace" = tempNamespace,
                            "surveyIdentifier" = clusterResponses_selectedODSSurvey,
                            "surveyResponseIdentifier" = paste0(
                                ifelse(nchar(clusterResponses_selectedODSSurvey)>40,
                                       sha1(clusterResponses_selectedODSSurvey),
                                       clusterResponses_selectedODSSurvey), switch(clusterResponses_RespondentTypes, 
                                                                      "Student" = ifelse(clusterStudentReference=="Not Applicable",
                                                                                         j,
                                                                                         clusterFileUpload[[clusterStudentReference]][j]),
                                                                      "Staff" = ifelse(clusterStaffReference=="Not Applicable",
                                                                                       j,
                                                                                       clusterFileUpload[[clusterStaffReference]][j]),
                                                                      "Parent" = ifelse(clusterParentReference=="Not Applicable",
                                                                                        j,
                                                                                        clusterFileUpload[[clusterParentReference]][j])
                                       ),
                                ifelse(responses_EndTimeType=="Column from my File Upload",
                                       clusterFileUpload[[clusterResponseDate]][j] %>%
                                           as_datetime()%>%as.integer(),
                                       clusterResponseDate
                                )
                                
                            )
                        ),
                        "values" = list(list(
                            "surveyQuestionResponseValueIdentifier" = k,
                            "textResponse" = clusterFileUpload[[get(paste0("surveyQuestionInput", k))]][j]
                        ))
                        
                    )
                }
            }
            return(currentResponse)
        }
        
        output<-list()
        
        # If vector was fed to function, recurse over each item in vector
        if(length(i)>1){
            output<-map(i, transformLoadSurveyQuestionResponse)
        }else{
            # Otherwise, transform input and POST to ODS
            output<-tempFunction(j=i)
            #lastOut<-
            map_df(output, function(x){
                # POST
                postRes<-POST(paste0(ODS_URL, "surveyQuestionResponses"),
                              add_headers("Authorization" = paste0("Bearer ",
                                                                   API_Token),
                                          "Content-Type" = "application/json"),
                              body = toJSON(
                                  x,
                                  auto_unbox = T, pretty = T)
                )
                
                # Save a log of how it went
                return(data.frame(
                    postID = paste0(currentUser, postID),
                    sessionID = userSessionID,
                    timestamp = Sys.time(),
                    postBody = toJSON(x, auto_unbox = T, pretty = T) %>% as.character(),
                    headers = postRes$headers %>% toJSON(auto_unbox = T, pretty = T) %>% as.character(),
                    apiResponse = postRes$status %>% as.character()
                ))
                # Output DF of POST results
            }) %>% return()
            
            
            #return(lastOut)
        }
    }
    
    ## Run Parallel SurveyResponse Function
    tempOut<-pblapply(seq_len(nrow(fileUpload())),
                      transformLoadSurveyQuestionResponse
                      , cl = clust
    ) %>% reduce(rbind)
    
    # ...And return a note about how it went
    con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
    dbAppendTable(con, "postLog", 
                  tempOut
    )
    
    
    sendSweetAlert(session, "Done!", paste0("Load complete. There were ", tempOut %>% filter(!apiResponse %>% str_detect("20")) %>% nrow(),
                                            " errors. Upload results are shown in the table below."))
    
    if(length(tempOut)>1){
        surveyQuestionResponsesJSON$list<-tempOut
    }else{
        surveyQuestionResponsesJSON$list<-tempOut[[1]]
    }
    
    #surveyResponseJSON$list<-pblapply(seq_len(nrow(fileUpload())), prepSurveyResponse)
    print(length(surveyQuestionResponsesJSON$list))
    
    #surveyResponseJSON$list<-parLapply(clust, seq_len(nrow(fileUpload())), prepSurveyResponse)
    
    ## Kill Parallel Processes
    stopCluster(clust)
    
    # ################
    # ### ORIGINAL ###
    # ################
    # 
    # ## Construct surveyQuestionResponse resource for each row in file upload
    # for(i in seq_len(nrow(fileUpload()))){
    #     
    #     currentResponse<-list()
    #     for(j in seq_len(nrow(selectedSurveyQuestions))){
    #         if(input[[paste0("SQR_Q",j)]]!="Not Applicable"){
    #             currentResponse[[j]] = list(
    #             "surveyQuestionReference" = list(
    #                 "namespace" = tempNamespace,
    #                 "questionCode" = selectedSurveyQuestions$questionCode[j],
    #                 "surveyIdentifier" = Responses_selectedODSSurvey()
    #             ),
    #             "surveyResponseReference" = list(
    #                 "namespace" = tempNamespace,
    #                 "surveyIdentifier" = Responses_selectedODSSurvey(),
    #                 "surveyResponseIdentifier" = paste0(
    #                     Responses_selectedODSSurvey(), switch(input$Responses_RespondentTypes, 
    #                                                           "Student" = ifelse(input[[inputMap$studentReference]]=="Not Applicable",
    #                                                                              i,
    #                                                                              fileUpload()[[input[[inputMap$studentReference]]]][i]),
    #                                                           "Staff" = ifelse(input[[inputMap$staffReference]]=="Not Applicable",
    #                                                                            i,
    #                                                                            fileUpload()[[input[[inputMap$staffReference]]]][i]),
    #                                                           "Parent" = ifelse(input[[inputMap$parentReference]]=="Not Applicable",
    #                                                                             i,
    #                                                                             fileUpload()[[input[[inputMap$parentReference]]]][i])
    #                     ),
    #                     ifelse(input[[inputMap$responseDate]]=="Not Applicable", 
    #                            "", 
    #                            fileUpload()[[input[[inputMap$responseDate]]]][i]%>%
    #                                as_datetime()%>%as.integer())
    #                 )
    #             ),
    #             "values" = list(list(
    #                 "surveyQuestionResponseValueIdentifier" = j,
    #                 "textResponse" = fileUpload()[[input[[paste0("SQR_Q",j)]]]][i]
    #             ))
    #             
    #         )
    #         }
    #     }
    #     
    #     
    #     #################
    #     ## POST TO ODS ##
    #     #################
    #     
    #     if((Sys.time()+300)>tokenExpiration$timestamp){
    #         sendSweetAlert(session, "Auto-Refreshing ODS Data Connection", type = "info")
    #         click("connectODS")
    #         tokenExpiration$timestamp<-Sys.time()+1800
    #         
    #         progressSweetAlert(
    #             session = session, id = "SQR_PreviewProgress",
    #             title = "Transforming Data...",
    #             display_pct = TRUE, value = 0, striped = T
    #         )
    #     }
    #     
    #     
    #     ## Pass current response item to parallel processes
    #     #clusterExport(cl, "currentResponse")
    #     
    #     ch<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
    #     #print(currentResponse)
    #     #clusterExport(cl, "currentResponse", envir = environment())
    #     # tempLog<-parLapply(cl, 
    #     #                    currentResponse, loadQuestionResponse,
    #     #           ODS_URL=ODS_URL,
    #     #           API_Token=API_Token) %>% 
    #     #     reduce(rbind) 
    #     #     
    #     # dbAppendTable(ch, "postLog", tempLog)
    #     tempPayload<-map(currentResponse, function(x){
    #         toJSON(x, auto_unbox = T, pretty = T) %>% 
    #             as.character()
    #     }) %>% reduce(c)
    #     
    #     dbAppendTable(ch, "tempLoad",
    #                   data.frame(
    #         convoyID = convoyID,
    #         payload = tempPayload
    #     ))
    #     dbDisconnect(ch)
    #     
    #     # for(j in seq_along(currentResponse)){
    #     #     # print(toJSON(
    #     #     #     surveyQuestionResponsesJSON$list[[i]][[j]],#%>%
    #     #     #     #unlist(recursive = F), 
    #     #     #     auto_unbox = T, pretty = T))
    #     #     # print(PostSurveyQuestionResponses)
    #     # 
    #     #     #PostSurveyQuestionResponses[[length(PostSurveyQuestionResponses)+1]]
    #     #     currentPost<-POST(
    #     #         paste0(
    #     #             input$ODS_URL, "surveyQuestionResponses"), 
    #     #         add_headers("Authorization" = paste0("Bearer ", 
    #     #                                              myToken()$credentials$access_token),
    #     #                     "Content-Type" = "application/json"),
    #     #         body = toJSON(
    #     #             currentResponse[[j]],#%>%
    #     #             #unlist(recursive = F), 
    #     #             auto_unbox = T, pretty = T)
    #     #     )
    #     #     
    #     #     
    #     #     ## Log POST Result to DB
    #     #     ch<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
    #     #     dbAppendTable(ch, "postLog", 
    #     #                   data.frame(
    #     #                       timestamp = Sys.time(),
    #     #                       postBody = toJSON(
    #     #                           currentPost$request$options$postfields%>%
    #     #                               rawToChar()%>%
    #     #                               fromJSON(), 
    #     #                           auto_unbox = T, pretty = T) %>% 
    #     #                           as.character(),
    #     #                       headers = currentPost$headers%>% unlist() %>% 
    #     #                           paste0(collapse = "\n"),
    #     #                       apiResponse = currentPost$status
    #     #                   )
    #     #     )
    #     #     dbDisconnect(ch)
    #     # 
    #     # }
    #     
    #     
    #     
    #     # Update Progress Bar
    #     updateProgressBar(
    #         session = session,
    #         title = paste0("Step 1 (of 2): Transforming Data (Response ", 
    #                        i, " of ", nrow(fileUpload()), ")"), 
    #         id = "SQR_PreviewProgress",
    #         value = 100*(i/(nrow(fileUpload())))
    #     )
    #     
    # }
    # 
    # ## Close parallel processes
    # #stopCluster(cl)
    # 
    # # Reproduce POST for log file (POST)
    # # SQR_logPOST$results<-toJSON(
    # #     imap(
    # #         PostSurveyQuestionResponses, 
    # #         ~list("POST_Body" = toJSON(#PostSurveyQuestionResponses[[.y]]
    # #             .x$request$options$postfields%>%
    # #                 rawToChar()%>%
    # #                 fromJSON(), 
    # #             auto_unbox = T, pretty = T),
    # #             "Status" = .x$status,
    # #             "Headers" = .x$headers)
    # #     ), 
    # #     auto_unbox = T, pretty = T)
    # 
    # 
    # # Close Progress Bar
    # closeSweetAlert(session = session)
    # 
    # # Disconnect from App DB
    # #dbDisconnect(ch)
    # 
    # ### POST ###
    # print("Attempting Load")
    # ch<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
    # tempPayload<-dbReadTable(ch, "tempLoad") %>% 
    #     filter(convoyID==convoyID) %>% 
    #     select(payload) %>% 
    #     unlist() %>% 
    #     map(function(x){print(which(.==x))
    #         fromJSON(x)}) %>% 
    #     toJSON(auto_unbox = T,
    #            pretty = T)
    # print("Fetched payloads")
    # 
    # tempLog<-parLapply(cl,
    #                    tempPayload, loadQuestionResponse,
    #           ODS_URL=ODS_URL,
    #           API_Token=API_Token) %>%
    #     reduce(rbind)
    # 
    # dbAppendTable(ch, "postLog", tempLog)
    # dbDisconnect(ch)
    # 
    # # Confirmation Message (POST)
    # sendSweetAlert(
    #     session = session,
    #     title = "Survey Question Responses Submitted!", 
    #     text = paste0("Remember to download the log file! (There were ", 
    #                   ifelse(length(SQR_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow()),
    #                          SQR_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow(),
    #                          0),
    #                   " errors.)"),
    #     type = "success"
    # )
    
    # (re)set JSON preview cursor value
    surveyQuestionResponsesJSON$cursor<-1
})
    
    # Survey Question Response JSON Cursor
    observeEvent(input$SQR_Previous, {
        if(surveyQuestionResponsesJSON$cursor>1){
            surveyQuestionResponsesJSON$cursor<-surveyQuestionResponsesJSON$cursor-1
        }
    })
    observeEvent(input$SQR_Next, {
        if(surveyQuestionResponsesJSON$cursor<nrow(fileUpload())*ncol(fileUpload())){
            surveyQuestionResponsesJSON$cursor<-surveyQuestionResponsesJSON$cursor+1
        }
    })
    
    ## Survey Question Response JSON Preview
    output$SQR_PreviewJSON<-#renderText({toJSON(
    #     surveyQuestionResponsesJSON$list#[[surveyQuestionResponsesJSON$cursor]]
    #     ,#%>%
    #         #unlist(recursive = F),
    #     pretty = T, auto_unbox = T)
    # })
        renderDT({
            
            if(length(surveyQuestionResponsesJSON$list)){
                surveyQuestionResponsesJSON$list <- surveyQuestionResponsesJSON$list %>% 
                    mutate(Status = case_when(
                        apiResponse == 200 ~ "Updated (200)",
                        apiResponse == 201 ~ "Created (201)",
                        TRUE~paste0("Error (", apiResponse, ")")
                    )) %>% 
                    relocate(
                        Status,
                        postBody,
                        timestamp,
                        headers,
                        postID,
                        sessionID
                    )
            }

            surveyQuestionResponsesJSON$list 
        }, 
        #class = 'table-condensed',
        rownames = F,
        filter = list(position = 'top', clear = FALSE),
        options = list(scrollY = "350px",
                       search = list(regex = TRUE)
                       ),
        fillContainer = T)

#---------->
# POST Survey Question Responses to ODS ##
#---------->
SQR_logPOST<-reactiveValues()

observeEvent(input$SQR_Post, {
    
    progressSweetAlert(
        session = session, id = "SQR_POSTprogress",
        title = "Loading Survey Question Responses",
        display_pct = TRUE, value = 0, striped = T
    )
    
    PostSurveyQuestionResponses<-list()
    
    # POST Data
    for(i in seq_along(surveyQuestionResponsesJSON$list)){
        
        if((Sys.time()+300)>tokenExpiration$timestamp){
            sendSweetAlert(session, "Auto-Refreshing ODS Data Connection", type = "info")
            click("connectODS")
            tokenExpiration$timestamp<-Sys.time()+1800
        }
        
        for(j in seq_along(surveyQuestionResponsesJSON$list[[i]])){
        PostSurveyQuestionResponses[[length(PostSurveyQuestionResponses)+1]]<-POST(
            paste0(
                input$ODS_URL, "surveyQuestionResponses"), 
                add_headers("Authorization" = paste0("Bearer ", 
                                                     myToken()$credentials$access_token),
                            "Content-Type" = "application/json"),
                body = toJSON(
                    surveyQuestionResponsesJSON$list[[i]][[j]],#%>%
                        #unlist(recursive = F), 
                    auto_unbox = T, pretty = T)
        )
        }
        # Update Progress Bar
        updateProgressBar(
            session = session,
            id = "SQR_POSTprogress",
            value = 100*(i/(length(seq_along(surveyQuestionResponsesJSON$list))))
        )
    }
    
    # Reproduce POST for log file
    SQR_logPOST$results<-toJSON(
        imap(
            PostSurveyQuestionResponses, 
            ~list("POST_Body" = toJSON(#PostSurveyQuestionResponses[[.y]]
                                       .x$request$options$postfields%>%
                                           rawToChar()%>%
                                           fromJSON(), 
                                       auto_unbox = T, pretty = T),
                  "Status" = .x$status,
                  "Headers" = .x$headers)
        ), 
        auto_unbox = T, pretty = T)
    
    # Close Progress Bar
    closeSweetAlert(session = session)
    
    # Confirmation Message
    sendSweetAlert(
        session = session,
        title = "Survey Question Responses Submitted!", 
        text = paste0("Remember to download the log file! (There were ", 
                      ifelse(length(SQR_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow()),
                             SQR_logPOST$results%>%str_locate_all('Status": 40')%>%pluck(1)%>%nrow(),
                             0),
                      " errors.)"),
        type = "success"
    )
    print(PostSurveyQuestionResponses[[1]]$content %>% rawToChar())
})


# Download POST Results
output$SQR_downloadPostLog <- downloadHandler(
    filename = function() {
        paste0("ODS_Load_SurveyQuestionResponses_", Responses_selectedODSSurvey(), "_", 
               Sys.time()%>%str_replace_all(":", "_"), ".json")
    },
    content = function(file) {
        write(SQR_logPOST$results, file)
    }
)

    
    ####################
    ##### TYPEFORM #####
    ####################
{
    # {
    # #----------------------------#
    # # Display Existing TypeForms #
    # #----------------------------#
    # 
    # ## FORMS REACTIVE 
    # # allTypeforms<-reactive({
    # #     if(checkPAT()==0){
    # #         
    # #      #   surveysODS<-getODSResource("surveys", input$ODS_URL, myToken())
    # #         
    # #         GET(paste0(baseURL, "/forms"),
    # #             add_headers("Authorization" = 
    # #                             paste0("Bearer ", input$typeformPAT)))$content%>%
    # #             rawToChar()%>%
    # #             fromJSON()%>%
    # #             as.data.frame()%>%
    # #             # left_join(surveysODS, by = c("items.id" = "surveyIdentifier"))%>%
    # #             # mutate(`In ODS` = case_when(is.na(surveyIdentifier)~"No", TRUE~"Yes"))%>%
    # #             select(
    # #                 ID = items.id,
    # #                 Title = items.title,
    # #                 URL = items._links
    # #                 # ,
    # #                 # `In ODS`
    # #             )
    # #     }
    # # })
    # 
    # ## FORMS OUTPUT
    # # output$forms<-renderDT({allTypeforms()}, 
    # #                        selection = "single",
    # #                        class = 'table-condensed',
    # #                        options = list(scrollY = "150px", 
    # #                                       pageLength=100))
    # 
    # #--------------------#
    # # TypeForm Questions #
    # #--------------------#
    # 
    # # questions<-eventReactive(input$forms_rows_selected, {
    # #     selected<-input$forms_rows_selected
    # #     if(length(selected)){
    # #         questionsRequest<-GET(paste0(baseURL, "/forms/", allTypeforms()$ID[selected]))$content%>%
    # #             rawToChar()%>%
    # #             fromJSON()#$fields%>%select(title, properties)
    # #         questionsRequest
    # #     }
    # # })
    # 
    # # output$questionInfo<-renderDT(questions()$fields%>%select(title, type)%>%
    # #                                   cbind(questions()$fields$properties%>%
    # #                                             unnest_wider(choices)%>%as.data.frame())%>%
    # #                                   select(
    # #                                       Question = title, 
    # #                                       Type = type, 
    # #                                       Options = label))
    # 
    
    #--------------#
    # Forms Inputs #
    #--------------#
    output$surveyInputs<-renderUI({
        selected<-input$forms_rows_selected
        
        ## Pull default namespace if pre-configured connection is used
        if(isTruthy(input$odsConnectionOptions)){
            con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
            
            defaultNamespace<-dbReadTable(con, "odsConnections") %>% 
                filter(connectionName==input$odsConnectionOptions) %>% 
                select(defaultNamespace) %>% 
                head(1) %>% 
                unlist() %>% 
                unname()
            
            dbDisconnect(con)
        }


        ## IF TYPEFORM ##
        if(length(selected)){
            fluidRow(
                column(width = 6,
                       textInput("surveyTitle", "Survey Title", allTypeforms()$Title[selected]),
                       textInput("surveyIdentifier", "Survey Identifier",
                                 allTypeforms()$ID[selected]),
                       textInput("surveyNamespace", "Namespace", placeholder = "uri://northallegheny.org"),
                       selectInput("surveySchoolYear", "School Year (spring-side)",
                                   c(getODSResource("schoolYearTypes", input$ODS_URL,
                                                    myToken())%>%
                                       select(schoolYear)%>%unique()
                                   )
                                )
                       ),
                column(width = 6,
                       selectInput("surveySessionName", "Session",
                                   c(getODSResource("sessions", input$ODS_URL,
                                                  myToken(), moreParameters = paste0("schoolYear=", input$surveySchoolYear
                                                                                    ))%>%
                                       select(sessionName)%>%unique(), "Not Applicable")),
                       selectInput("surveyEducationOrganization", "Education Organization",
                                   c(getODSResource("schools", input$ODS_URL,
                                                    myToken())%>%
                                         select(nameOfInstitution),
                                     getODSResource("localEducationAgencies", input$ODS_URL,
                                                    myToken())%>%
                                         select(nameOfInstitution)%>%
                                         unname(), "Not Applicable")),
                       selectInput("surveyCategory", "Category",
                                   getODSDescriptors("surveyCategoryDescriptors", input$ODS_URL, myToken())%>%
                                       select(codeValue)
                                   ),
                       #actionBttn("previewSurveyJSON", "Preview JSON", style = "material-flat", color = "warning"),
                       actionBttn("uploadSurvey", "Upload!", style = "material-flat"),
                       verbatimTextOutput("previewSurveyJSON")
                       )
            )
            ## IF FLAT FILE ##
        }else if(isTruthy(fileUpload())){
            fluidRow(
                column(width = 6,
                       textInput("surveyTitle", "Survey Title", input$fileIn$name) %>% tipify(
                           title = "This will be the display name for your survey. The default value is the name of the file you uploaded, but feel free to change this!",
                           placement = "right"
                       ),
                       textInput("surveyIdentifier", "Survey Identifier",
                                 input$fileIn$name) %>% tipify(
                                     title = "Choose a unique identifier for this survey. Default value is the name of the file you uploaded, but you may change this based on your organization\\'s naming conventions.",
                                     placement = "right"
                                 ),
                       textInput("surveyNamespace", "Namespace", placeholder = "uri://northallegheny.org",
                                 value = ifelse(exists("defaultNamespace"), defaultNamespace, NA)
                                 ) %>% tipify(
                           title = "\\'Namespace\\' will typically be \\'uri://[yourDomainName.org]\\' or similar (e.g. \\'uri://northallegheny.org\\'). Check with your data administrator if you are unsure.",
                           placement = "right"
                       ),
                       selectInput("surveySchoolYear", "School Year (spring-side)",
                                    c(getODSResource("schoolYearTypes", input$ODS_URL,
                                                      myToken())%>%
                                         select(schoolYear)%>%unique()
                                    )
                       )
                ),
                column(width = 6,
                       HTML("<p style='width:350px;'><i>The options for these dropdowns are based on existing values in the Data Store you connected to in Step 1 (at left). If you need additional options, ask your data administrator if they can update the descriptors in your Ed-Fi ODS (they should know what this means :).</i><br><br></p>"),
                       selectInput("surveySessionName", "Session",
                                   c(getODSResource("sessions", input$ODS_URL,
                                                  myToken()#, moreParameters = paste0("schoolYear=", input$surveySchoolYear)
                                                  )%>%
                                       select(sessionName)%>%unique(), "Not Applicable")) %>% tipify(
                                           title = "Session/Term associated with this survey. Choose \\'Not Applicable\\' if there is no particular session associated.",
                                           placement = "right"
                                       ),
                       selectInput("surveyEducationOrganization", "Education Organization",
                                   c(getODSResource("schools", input$ODS_URL,
                                                  myToken())%>%
                                       select(nameOfInstitution),
                                     getODSResource("localEducationAgencies", input$ODS_URL,
                                                    myToken())%>%
                                         select(nameOfInstitution)%>%
                                         unname(), "Not Applicable"))  %>% tipify(
                                             title = "School/Education Organization associated with this survey. Note: in some cases, your district/LEA may be a valid option in the list below. Choose \\'Not Applicable\\' if needed.",
                                             placement = "right"
                                         ),
                       selectInput("surveyCategory", "Category",
                                   #NA
                                   getODSDescriptors("surveyCategoryDescriptors", input$ODS_URL, myToken())%>%
                                       select(codeValue)
                       ) %>% tipify(
                           title = "Who is the target audience of this survey?",
                           placement = "right"
                       ),
                       #actionBttn("previewSurveyJSON", "Preview JSON", style = "material-flat", color = "warning"),
                       actionBttn("uploadSurvey", "Upload!", style = "material-flat", color = "success", size = "sm")
                       #, verbatimTextOutput("previewSurveyJSON")
                )
            )
        }else{
            h4("Click a row to select a form in the table above...")
        }
    })

    ## Prepare/Preview Survey Insert JSON Object ##
    previewSurveyJSON <- reactive({
        input$uploadSurvey

        # Grab EdOrg ID -- ASSUMPTION: all schoolIds and LEA ids in ODS are also valid EdOrg ids
        tempSchool <- if(getODSResource("schools", input$ODS_URL, # Check if type is school or LEA
                                     myToken())%>%
            filter(nameOfInstitution==input$surveyEducationOrganization)%>%nrow()>0){
            getODSResource("schools", input$ODS_URL, # If school, fetch school ID
                           myToken())%>%
                filter(nameOfInstitution==input$surveyEducationOrganization)%>%
                select(schoolId)%>%
                unlist()
        }else{
            getODSResource("localEducationAgencies", input$ODS_URL, # If LEA, fetch LEA ID
                           myToken())%>%
                filter(nameOfInstitution==input$surveyEducationOrganization)%>%
                select(localEducationAgencyId)%>%
                unlist()
        }

        ## Prep list to convert to JSON
        insertList<-list(
            namespace = input$surveyNamespace,
            surveyIdentifier = input$surveyIdentifier,
            schoolYearTypeReference = list(
                schoolYear = input$surveySchoolYear
            ),
            surveyCategoryDescriptor = formatDescriptor("surveyCategoryDescriptors", input$ODS_URL, input$surveyCategory, myToken()),
            surveyTitle = input$surveyTitle
        )

        ## Add Session Variable if Selected by User ##
        if(input$surveySessionName!="Not Applicable"){
            insertList[["sessionReference"]]<-list(
                schoolId = tempSchool, # ASSUMPTION: selected school has same term options as general dropdown for school year
                schoolYear = input$surveySchoolYear,
                sessionName = input$surveySessionName
            )
        }

        ## Add EdOrg Variable if Selected by User ##
        if(input$surveyEducationOrganization!="Not Applicable"){
            insertList[["educationOrganizationReference"]]<-list(
                educationOrganizationId = tempSchool
            )
        }

        preview<-toJSON(insertList, auto_unbox = T, pretty = T)

        preview

    })

    output$previewSurveyJSON<-renderText({
        req(previewSurveyJSON())
        previewSurveyJSON()})

    ## POST Survey to ODS ##
    observeEvent(input$uploadSurvey, {
        # POST Data
        PostSurvey<-POST(paste0(input$ODS_URL, "surveys"),
                             add_headers("Authorization" = paste0("Bearer ", myToken()$credentials$access_token),
                                         "Content-Type" = "application/json"),
                             body = previewSurveyJSON())


        # Tell the User How It Went
        sendSweetAlert(session = session,
                       title = "Survey POST Status",
                       text = ifelse(substr(PostSurvey$status_code, 1,1)=="2",
                                     paste0("Successful! (status code ", PostSurvey$status_code, ")"),
                                     paste0("Unsuccessful :(  (status code ", PostSurvey#$status_code
                                            , ")")),
                       type = ifelse(substr(PostSurvey$status_code, 1,1)=="2","success","error"))
    })
    
    
    
    #################
    ### Admin Tab ###
    #################
    
    ## Grab table of existing connection strings ##
    odsConnectionStrings<-reactive({
        input$selectedOdsConnectionSubmit
        input$newOdsConnectionSubmit
        input$selectedOdsConnectionDelete
        con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
        out<-dbReadTable(con, "odsConnections") %>% 
            select(2:4) %>% 
            rename("Connection" = 1,
                   "URL" = 2,
                   "API Key" = 3)
        
        dbDisconnect(con)
        
        out
    })
    
    adminPass<-reactive({
        con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
        out<-dbReadTable(con, "admin") %>% 
            filter(type=="adminPass") %>% 
            select(value) %>% 
            unlist() %>% 
            unname()
        
        dbDisconnect(con)
        
        out
    })
    
    ## Submitted pass value ##
    inputPass<-eventReactive(input$submitAdminPass, {
        input$adminPass %>% sha256() %>% as.character()
    })
    
    ## Connection Strings Table ##
    output$connectionStrings<-renderDT({odsConnectionStrings()},
                                       options = list(dom = 't'
                                                      #,scrollY = "250px"
                                                      ), 
                                       rownames = F, 
                                       #fillContainer = T,
                                       selection = "single")
    
    ## User-Based Access User List Input ##
    output$permittedUserUI<-renderUI({
        input$newOdsConnectionUserBasedAccess
        if(input$newOdsConnectionUserBasedAccess){
            textAreaInput("newOdsConnectionPermittedUsers", "Permitted Users", 
                          placeholder = "Separate each user with a comma or semicolon and a space!")
        }
    }
    )
    
    ## Admin Tab Selected Connection Data ##
    selectedConnection<-reactive({
        selected<-input$connectionStrings_rows_selected
        if(length(selected)){
            con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
            
            out<-dbReadTable(con, "odsConnections") %>% 
                filter(connectionName==odsConnectionStrings()$Connection[selected])
            
            dbDisconnect(con)
            out
        }
    })
    
    ## Admin Tab Sub-UI: Selected Connection Inputs ##
    output$selectedConnectionInputs<-renderUI({
        if(input$adminTabs=="Existing Connections"){
            box(id = "connectionDetails", title = "Connection Details", status = "black",
                textInput("selectedOdsConnectionName", "Connection Name", 
                          value = selectedConnection()$connectionName) %>% tipify(
                              "This field is required and must be unique! Users will see this value in the connection strings drop\\-down menu in the first tab of this app.",
                              placement = "left"
                          ),
                textInput("selectedOdsConnectionURL", "ODS API URL (ending in '/ed-fi/')", 
                          value = selectedConnection()$url),
                textInput("selectedOdsConnectionKey", "ODS API Key", 
                          value = selectedConnection()$key),
                passwordInput("selectedOdsConnectionSecret", "ODS API Secret", 
                              value = selectedConnection()$secret),
                textInput("selectedOdsConnectionDefaultNamespace", "Default Namespace", 
                          value = selectedConnection()$defaultNamespace) %>% tipify(
                              "This value will be pre\\-filled in the Survey Metadata page (step 2) when users select this connection string.",
                              placement = "left"
                          ),
                materialSwitch("selectedOdsConnectionUserBasedAccess", "User-Based Access?", 
                               value = selectedConnection()$userBasedAccess) %>% tipify(
                                   "Switching this on means that only users specified in the field below will be able to see and use this connection string. If this is switched on and the Permitted Users field is empty, no one will be able to use this connection.",
                                   placement = "left"
                               ),
                textAreaInput("selectedOdsConnectionPermittedUsers", "Permitted Users", 
                              value = selectedConnection()$permittedUsers) %>% tipify(
                                  "Only required if User\\-Based Access is switched on. This should be a comma or semicolon\\-separated list of usernames allowed to access this connection string. Default format is the user\\'s samaccountname.",
                                  placement = "left"
                              ),
                actionBttn("selectedOdsConnectionSubmit", "Update", style = "material-flat", color = "success",
                           size = "sm"),
                actionBttn("selectedOdsConnectionDelete", "DELETE", style = "material-flat", color = "danger",
                           size = "sm")
            )
        }
        
    })
    
    ## Re-set Password Modal ##
    admin_resetPassword <- function(failed = FALSE){
        modalDialog(
            list(h4("Please enter a new password below"),
            passwordInput("admin_newPassword", "Password"),
            passwordInput("admin_newPasswordValidation", "Re-Enter")),
        if(failed)
            div(tags$b("Please enter a new password or hit 'Cancel'",
                       style = "color:red;")),
        footer = tagList(
            modalButton("Cancel"),
            actionButton("admin_submitNewPassword", "Submit")
        )
        )
    }
    
    ## Re-set Password Actions ##
    observeEvent(input$admin_submitNewPassword, {
        req(input$admin_newPassword)
        req(input$admin_newPasswordValidation)
        
        # Check for matching PW inputs
        if(input$admin_newPassword!=input$admin_newPasswordValidation){
            sendSweetAlert(session, "Oops!", "Looks like those passwords don't match!",
                           type = "error")
        }else{
            # PW Length requirement (>=8 chars)
            if(input$admin_newPassword %>% nchar()<8 | 
               # PW non-exposure requirement (haveIBeenPwned database)
               GET(paste0("https://api.pwnedpasswords.com/range/", 
                          sha1(input$admin_newPassword) %>% 
                          as.character() %>% 
                          substr(1,5))) %>% 
               pluck("content") %>% 
               rawToChar() %>% 
               str_split("\\r") %>% 
               map(function(x){
                   str_detect(x, sha1(input$admin_newPassword) %>% 
                              as.character() %>% 
                              substr(6,40) %>% 
                              toupper())}) %>% 
               reduce(c) %>% 
               sum()>0){
                # If PW too short or has been exposed, send error message
                sendSweetAlert(session, 
                               "Yikes!", 
                               "Looks like that password is not secure or has been exposed in a data breach, according to https://haveibeenpwned.com/Passwords. Please try another!",
                               type = "warning")
            }else{
                # If password meets security requirements, save to DB and close modal
                con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
                dbExecute(con, paste0("
                          UPDATE admin SET value = '", 
                          input$admin_newPassword %>% sha256() %>% as.character(),
                          "' WHERE type='adminPass'"))
                dbDisconnect(con)
                
                removeModal()
            }
        }
    })
    
    ## User-Initiated PW Update ##
    observeEvent(input$admin_updatePassword, {
        showModal(admin_resetPassword())
    })
    
    ## Post Log Admin Table ##
    # adminPostLog<-reactive({
    #     con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
    #     out<-dbReadTable(con, "postLog")
    # })
    
    output$adminPostLogTable<-renderDT({
        con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
        out<-dbReadTable(con, "postLog") %>% 
            mutate(timestamp = timestamp %>% as_datetime() %>% as.character())
    },
    rownames = F,
    filter = list(position = 'top', clear = FALSE),
    options = list(scrollY = "350px",
                   pageLength = 100,
                   search = list(regex = TRUE)),
    fillContainer = T)
    
    ## Admin Tab UI ##
    output$adminPage<-renderUI({

        if(inputPass()==adminPass()){
            if(input$adminPass=="admin"){
                showModal(admin_resetPassword())
            }
            input$selectedOdsConnectionSubmit
            input$newOdsConnectionSubmit
            input$selectedOdsConnectionDelete
            list(
                column(width = 7,
                       tabBox(id = "adminTabs", side = "left",
                              
                              ## Table View of existing connections 
                              tabPanel("Existing Connections",
                                       DTOutput("connectionStrings")),
                              ## New Connection Creation Inputs
                              tabPanel("New Connection",
                                       textInput("newOdsConnectionName", "Connection Name") %>% tipify(
                                           "This field is required and must be unique! Users will see this value in the Data Store Connections drop\\-down menu in the first tab of this app.",
                                           placement = "top"
                                       ),
                                       textInput("newOdsConnectionURL", "ODS API URL (ending in '/ed-fi/')"),
                                       textInput("newOdsConnectionKey", "ODS API Key"),
                                       passwordInput("newOdsConnectionSecret", "ODS API Secret"),
                                       textInput("newOdsConnectionDefaultNamespace", "Default Namespace") %>% tipify(
                                           "This value will be pre\\-filled in the Survey Metadata page (step 2) when users select this connection string.",
                                           placement = "top"
                                       ),
                                       materialSwitch("newOdsConnectionUserBasedAccess", "User-Based Access?", value = F) %>% tipify(
                                           "Switching this on means that only users specified in the field below will be able to see and use this connection string. If this is switched on and the Permitted Users field is empty, no one will be able to use this connection.",
                                           placement = "top"
                                       ),
                                       uiOutput("permittedUserUI") %>% tipify(
                                           "Only required if User\\-Based Access is switched on. This should be a comma or semicolon\\-separated list of usernames allowed to access this connection string. Default format is the user\\'s samaccountname.",
                                           placement = "top"
                                       ),
                                       actionBttn("newOdsConnectionSubmit", "Submit!", style = "material-flat", color = "royal",
                                                  size = "sm")
                                       ),
                              tabPanel("Upload Logs", 
                                       DTOutput("adminPostLogTable", width = 750)
                                       )
                              )
                       ),
                column(width = 5,
                       uiOutput("selectedConnectionInputs")
                       #numericInput("connectionID", "Connection ID")
                       ),
                fluidRow(
                    column(width = 5,
                    actionBttn("admin_updatePassword", "Update Password", style = "material-flat",
                               color = "danger", size = "xs")
                    )
                )
                )
                
                    #numericInput("connectionID", "Connection ID")
                    
                
        }else{
            sendSweetAlert(session, "Oops!", "Incorrect admin key", type = "error")
        }
    })
    
    ## Submit New ODS Connection String ##
    observeEvent(input$newOdsConnectionSubmit, {
        # All New Connection Inputs
        connectInputs<-paste0("newOdsConnection", c("Name", "URL", "Key", "Secret", "DefaultNamespace"))
        
        # Empty Inputs
        empties<-map(connectInputs, function(x){
            isTruthy(input[[x]])==F
        }) %>% reduce(c)

        if(sum(empties)>0){
            # Send user alert if inputs are empty
            sendSweetAlert(session, "Hang on!",
                           paste0("Please fill in the following required field(s) for your new ODS connection: ", 
                                  connectInputs[empties] %>% paste0(collapse = ", ") %>% 
                                      str_remove_all("newOdsConnection")), type = "warning")
        }else{
            # Submit new connection string to database if all required inputs are completed
            con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
            maxID<-dbReadTable(con, "odsConnections") %>% select(connectionID) %>% max()+1
            if(!maxID>0){
                maxID<-1
            }
            out<-data.frame(
                connectionID = maxID,
                connectionName = input$newOdsConnectionName,
                url = input$newOdsConnectionURL,
                key = input$newOdsConnectionKey,
                secret = input$newOdsConnectionSecret,
                defaultNamespace = input$newOdsConnectionDefaultNamespace,
                userBasedAccess = as.integer(input$newOdsConnectionUserBasedAccess),
                permittedUsers = ifelse(isTruthy(input$newOdsConnectionPermittedUsers), 
                                                 input$newOdsConnectionPermittedUsers,
                                                 "")
            )
            
            dbAppendTable(con, "odsConnections", out)
            dbDisconnect(con)
        }
    })
    
    ## Update ODS Connection ##
    observeEvent(input$selectedOdsConnectionSubmit, {
        
        # All Current Connection Inputs
        connectInputs<-paste0("selectedOdsConnection", c("Name", 
                                                         "URL", 
                                                         "Key", 
                                                         "Secret", 
                                                         "DefaultNamespace",
                                                         "UserBasedAccess",
                                                         "PermittedUsers"))
        
        names(connectInputs)<-c("connectionName", 
                                "url", 
                                "key", 
                                "secret", 
                                "defaultNamespace",
                                "userBasedAccess",
                                "permittedUsers")
        
        # Empty Inputs
        empties<-map(connectInputs, function(x){
            if(x=="selectedOdsConnectionUserBasedAccess"){
                F
            }else if(x=="selectedOdsConnectionPermittedUsers" & (input[["selectedOdsConnectionUserBasedAccess"]]==F)){
                F
            }else{
                isTruthy(input[[x]])==F
            }
            
        }) %>% reduce(c)
        
        if(sum(empties)>0){
            # Send user alert if inputs are empty
            sendSweetAlert(session, "Hang on!",
                           paste0("Please fill in the following required field(s) for your selected ODS connection: ", 
                                  connectInputs[empties] %>% paste0(collapse = ", ") %>% 
                                      str_remove_all("selectedOdsConnection")), type = "warning")
        }else{
            # Update connection string in database if all required inputs are completed
            con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
            
            map(connectInputs, function(x){
                dbExecute(con, paste0("
                          UPDATE odsConnections
                          SET ",  which(connectInputs==x) %>% names(),
                          " = '", ifelse(x=="selectedOdsConnectionUserBasedAccess", input[[x]] %>% as.integer,
                                         input[[x]]), "' 
                          WHERE connectionName = '", input$selectedOdsConnectionName, "'"
                          ))
            })
            
            
            #dbAppendTable(con, "odsConnections", out)
            dbDisconnect(con)
            
            sendSweetAlert(session, "Submitted!", "Your connection details have been updated", "success")
        }        
    })
    
    ## Delete ODS Connection ##
    observeEvent(input$selectedOdsConnectionDelete, {
        
        # Connect to App DB
        con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
        
        # Do the deed
        dbExecute(con, paste0("
                          DELETE FROM odsConnections
                          WHERE connectionName = '", input$selectedOdsConnectionName, "'"
        ))

        dbDisconnect(con)
        
        sendSweetAlert(session, "Done!", paste0("The ", input$selectedOdsConnectionName, " connection has been deleted! (This cannot be undone, but you can recreate the connection using the 'Create New Connection' button on this screen.)"), 
                                                "success")
    })
    
    ## ODS Connection Options Drop-Down ##
    output$odsConnectionOptions<-renderUI({
        
        # Take dependencies on connection update/submit/delete buttons from admin tab
        input$selectedOdsConnectionSubmit
        input$newOdsConnectionSubmit
        input$selectedOdsConnectionDelete
        
        con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
        
        connections<-dbReadTable(con, "odsConnections")
        if(nrow(connections>0)){
            connections<-connections %>% 
                filter(userBasedAccess==0 | (userBasedAccess==1 & 
                                                 permittedUsers %>% unlist() %>% 
                                                 map(
                                                     ~str_split(., ",") %>% 
                                                         unlist() %>% 
                                                         str_split(";") %>% 
                                                         unlist() %>% 
                                                         str_trim() %>% 
                                                         unlist() %>% 
                                                         tolower()
                                                 ) %>% map(~currentUser() %in% .) %>% reduce(c)
                )) %>% 
                select(connectionName) %>% 
                unlist() %>% 
                unname()
        }else{
            connections<-"Admin hasn't pre-set any connections. Please use fields below."
        }

    
        dbDisconnect(con)
        
        selectInput("odsConnectionOptions", "Choose a Data Store Connection", choices = connections) %>% 
            tipify("If your Data Administrator has configured this app with preset connection(s), they will show here, and selecting one will auto\\-fill the fields below. Otherwise, skip this dropdown and complete the fields below.",
                   placement = "bottom")
    })
    
    ## Update Connection Inputs based on selected connection
    observeEvent(input$odsConnectionOptions, {
        con<-dbConnect(RSQLite::SQLite(), "dbapp.sqlite")
        
        activeConnection<-dbReadTable(con, "odsConnections") %>% 
            filter(connectionName==input$odsConnectionOptions)
        
        dbDisconnect(con)
        
        updateTextInput(session, "ODS_URL", value = activeConnection$url[1])
        updateTextInput(session, "apiKey", value = activeConnection$key[1])
        updateTextInput(session, "apiSecret", value = activeConnection$secret[1])
        updateTextInput(session, "surveyNamespace", value = activeConnection$defaultNamespace[1])
    })
    
    
    
    ###############################
    ### UI Helpers and Tooltips ###
    ###############################
    
    ## Sample Survey Table ##
    output$sampleTable<-renderDT({
        
        sampleEmails<-map_chr(1:5, function(x){
            paste0(sample(letters, 8) %>% paste0(collapse = ""), "@",sample(letters, 5) %>% paste0(collapse = ""), ".com")
        })
        
        sampleStartTime<-map(1:5, function(x){
            Sys.Date()-sample(1:100, 1)
        }) %>% reduce(c)
        
        data.frame(
            responseID = seq_len(5),
            email = sampleEmails,
            completionDate = sampleStartTime,
            item1 = map_chr(1:5, ~sample(mtcars %>% unlist() %>% as.character() %>% unname(),1)),
            item2 = map_chr(1:5, ~sample(c(names(mtcars)),1)),
            item3 = map_chr(1:5, ~sample(mtcars %>% unlist() %>% as.character() %>% unname(),1)),
            item4 = map_chr(1:5, ~sample(mtcars %>% unlist() %>% as.character() %>% unname(),1))
        )
    }, options = list(dom = 't', scrollY = "210px"), fillContainer = T, rownames = F)
    
    #######################
    ### User Intro Tips ###
    #######################
    
    introSteps<-reactive({
      switch(input$sideBarTabs, 
             "appConnections" = data.frame(
               element = c(NA, 
                           "#connectOdsBox", 
                           "#uploadFileBox", 
                           NA),
               intro = c("<b>Welcome to the Ed-Fi Survey Loader Tool!</b>
                  <p>This interface is built to help you upload survey or form data to an Ed-Fi Operational Data Store (ODS), 
                  whether you're a data user, analyst, adminstrator, developer, technologist, or otherwise.</p>
                  <p>You'll upload your data using this workflow, which matches the tabs 
                  on the left side of the page:
                  <ol>
                    <li><b>Connect</b> - Connect to your Ed-Fi ODS API and select the file you wish to upload.
                    <li><b>Survey Metadata</b> - Tell your data store the high-level details about the survey you're going to upload (what's its name, who's the audience, what's it do for fun, yada yada...)
                    <li><b>Item Metadata</b> - Tell your data store about the questions in your survey. The app will pre-load most of this based on your file upload, but you'll check through to be sure it's all right.
                    <li><b>Load Results</b> - Upload the actual response data, now that the data store knows what to expect.
                  </ol></p>
                  <p><b>Note:</b> Once you've done steps 2 and 3 (survey and item metadata) for a survey,
                  you don't need to repeat every time you upload a new batch of results--if the metadata has been loaded, just skip to step 4!</p>
                  <p><i>Note: if the sizing of this box is a little funny, try zooming in or out in your browser (Ctrl+/-)</i></p>",
                         "To start, use this box to connect to an Ed-Fi Operational Data Store (ODS).</p>
                  <p>If your data administrator has pre-set connection info for you (using the Admin tab, at the left),
                  you will see those pre-set connection options in the drop-down menu here. If not, type in your ODS API URL, 
                  API Key, and API Secret in the fields below (contact your data administrator if you don't know this info or what it means).</p>
                  <p> Once you have either chosen a connection or entered your connection info manually, 
                  click 'Connect' to validate the data connection.</p>", 
                         "Next, upload your flat file (XLSX or CSV) with the survey/form response data you want to load. Keep in mind: 
                  your file should have one row per response, with each column being a survey item or survey response property (like respondent name or response date).
                  Avoid any blank rows, merged cells, or special formatting. Generally, your file should match the tabular format of a standard Google Forms or Microsoft Forms results spreadsheet.",
                         "Once you've successfully connected to an ODS and uploaded a file, proceed to Step 2: 'Survey Metadata' in the left-side menu!")
             ),
             "surveyMetadata" = data.frame(
               element = c(NA, "#surveyInputs"),
               intro = c("<b> Step 2: Survey Metadata </b> <br>
                             This step allows the user to enter basic information about the survey administered.<br>", 
                         "<b>Survey Information</b>
                             <br>
                              <b>Survey Identifier</b> is a unique identifier used to store each survey set that you will be loading to the ODS.
                                 Some larger surveys can be split into multiple files to load into the ODS for processing speed.  To do this, use the same survey identifier
                                 to load all files <u>for the same survey administration</u>.
                             <br><br>
                                 After the initial load of this information, any updates to the survey information can be made, including <b>Survey Name</b>, by making modifications on the screen and clicking <b>Upload</b> 
                                 <u>using the same Survey ID</u>.  <b>This will update the survey details in the ODS instead of creating a new survey record.</b>")
             ),
             "itemMetadata" = data.frame(
               element = c(NA
                           , "#questionColumns"
                           , "#questionPropertiesUI"
                           , NA),
               intro = c("<b> Step 3: Item Metadata </b><br>
                              This step allows the user to define an item code (identifier), label, response type, and response options for each question in your survey.</p>
                              <p>By default, the app will try to map the questions to the appropriate response type based on the survey data file you uploaded in Step 1.</p>
                              <p>You have the option to make updates to the pre-set values but this is not required if loading the survey is for analytics and reporting purposes.</p>"
                         , 
                         "<b>Questions</b><br>
                         All survey questions (i.e., columns in the file you uploaded in Step 1) are listed here.  Select one (<i>go ahead...click it now!</i>) to update the question text, item type, or response choices.
                         " ,
                         "<b>Question Details</b>
                         <p><i>Note: this section will show fully only if you have clicked on a question title from the table to the left</i></p>
                         <p>After making changes to the question details, make sure to click the <b>Update Item</b> button to save.  To add a response choice (for multiple-choice items), 
                         click <b>Add Response Choice</b>.  To remove a response choice, simply click backspace or delete in the Response Choices textbox (again, making sure to click <b>Update Item</b> to save your changes).
                         ",
                         "<p><b>Upload Survey Questions!</b></p>
                         <p>Click the green <span style = 'color:green'><b>UPLOAD!</b></span> button when you are ready to load your question mappings to your data store. A pop-up message will tell you if there were any errors--if there are, be sure to click the <b>Download API Post Log</b> button and share this info with your data admin for help resolving the issue.</p>
                         <p>Once you've successfully uploaded question metadata, you're ready to move on to <b>Step 4. Load Results</b>, in the left-hand menu!"
               )
             ),
             "loadResults" = data.frame(
               element = c(NA
                           , "#surveyResponseMetadataBlock"
                           , "#SR_ResponseMapping"
                           , "#SR_ResponseMapping"),
               position = c(NA,
                            "top",
                            NA, 
                            NA),
               intro = c(
                 "<p><strong>Step 4: Load Results</strong></p>
                          <p>In this step, you will load the survey results into the ODS! &nbsp;If you have either &hellip;</p>
                          <ol>
                              <li><strong>Already defined the survey</strong> and questions (Steps 1-3) and are loading responses for the first time, or</li>
                              <li><strong>Loaded responses for the same survey</strong> into the ODS in the past</li>
                          </ol>
                          <p>&hellip;you can select the survey from the <strong>Choose Survey&nbsp;</strong>drop-down list at the top.&nbsp;</p>
                          <p>The survey will need to be <strong>re-defined</strong> using steps 2 and 3 if new questions are added in the future, or if questions that were included in a prior load have been removed. &nbsp;</p>
                          <p><em>By default, the Ed-Fi Survey Loader will create a new unique survey ID every time a new file is selected in Step 1. &nbsp;If this is not desired, please select a previously-used survey from the <strong>Choose Survey&nbsp;</strong>drop-down list.</em></p>"
                 , "<h4>Important!</h4>
                 <p>This section is used to identify the survey data being loaded to the ODS.</p>
                    <p>The correct field in your survey will need to be selected in the <strong>Column holding Student/Parent/Staff ID</strong> drop-down<br>
                    in order for the ODS to map to the correct student/parent/staff (depending on survey audience) record.</p>
                    <p><u>If <b>Respondent Type</b> is not set to 'NA' and the Student/Parent/Staff ID listed for a given row in your file is <em>not found</em> in the ODS, that record will fail to load</u>.</p>"
                 , "<p><strong>Part A: Survey Responses</strong></p>
                          <p>This section is used to identify the person submitting the survey response for this particular survey administration. &nbsp;Select the appropriate fields from your survey data.</p>
                          <ol>
                              <li><strong>Column Holding Respondent Full Name:&nbsp;</strong>if survey was submitted <u>anonymously</u>, select Not Applicable.<strong>&nbsp;&nbsp;</strong>You may also choose the email address or other survey field if name is not provided in the survey response.</li>
                              <li><strong>Column Holding Respondent Email:&nbsp;</strong>if Google Forms, MS Forms, Typeform, Survey Monkey, etc. survey platform was used to administer the survey, the respondent&apos;s email address should be recorded with the submission. &nbsp;If not, choose Not Applicable.</li>
                              <li><strong>Column Holding Respondent Location:&nbsp;</strong>if the respondent&apos;s location, such as the school, is informative and was collected in the survey, select the corresponding survey field from this drop-down list.&nbsp;</li>
                          </ol>
                          <p>Don&apos;t forget to click <strong>'Upload!'&nbsp;</strong>after selecting the field values. &nbsp;A blank progress bar labeled 'message' will appear in the bottom right-hand corner of the screen while loading initiates. &nbsp;Please wait until the progress bar appears. &nbsp;When the load is complete, you will receive a <b>'Done!'</b> pop-up message, and a table will appear showing the upload results. &nbsp;You can use the search filters atop each column to view records that were not loaded (e.g. search '4' in the <b>Status</b> column to find any records that returned an error (codes 4xx)).</p>
                          <p><strong>Download the API POST Log&nbsp;</strong>file to review errors if any, as those records with <u>errors have not been loaded to the ODS</u>.</p>
                          <p><strong><a href='https://techdocs.ed-fi.org/display/EFDS30/Response+Codes' target='_blank'>Post Status&nbsp;</strong>reference</a></p>"
                 , "<p><strong>Part B: Survey Question Responses</strong></p>
                            <p>Use the drop-down menus in this section (<i>go ahead and click the 'Part B' tab now, if you wish</i>) to match each <u>question/item from this survey</u> (as defined in the 'Item Metadata' step, at left) to the proper <u>column name</u> from your file upload. 
                          <br><p>The app will attempt to match these automatically for you; however, <strong>Re-mapping</strong> of survey fields to the <u>survey&nbsp;</u><u>file selected in</u><u>&nbsp;Step 1: Connect</u> is <strong><em>necessary</em></strong> if (A) your file's column headers don't quite match the Question Text that was defined for this survey in the ODS, or (B) the survey file format has changed since the Item Metadata step was first completed in this app. &nbsp;Scroll through the survey fields to ensure the correct fields are mapped to the file.</p>
                          <p>Click '<strong>UPLOAD!</strong>' after confirming the survey fields have been mapped properly. &nbsp;<strong>Please wait for the progress bar to get started</strong> (it will show a blank pop-up with the word 'message' while the load is initializing). Upon completion of the load, please review the ODS upload results in the table that appears, just as in Part A, and <strong>Download the API Post Log</strong> for future reference.</p>
                          <p><strong><u>Note: # Errors = # Surveys submitted x # Questions</u></strong></p>
                          <p><strong><a href='https://techdocs.ed-fi.org/display/EFDS30/Response+Codes' target='_blank'>Post Status&nbsp;</strong>reference</a></p>"
                          # <ul>
                          #     <li><strong>200</strong> The resource was updated. An updated ETag value is available in the ETag header of the response.</li>
                          #     <li><strong>201</strong> The resource was created. An ETag value is available in the ETag header, and the location of the resource is available in the Location header of the response.</li>
                          #     <li><strong>400</strong> Bad Request. The request was invalid and cannot be completed. See the response body for specific validation errors. This will typically be an issue with the query parameters or their values.</li>
                          #     <li><strong>401</strong> Unauthorized. The request requires authentication. The OAuth bearer token was either not provided or is invalid. The operation may succeed once authentication has been successfully completed.</li>
                          #     <li><strong>403</strong> Forbidden. The request cannot be completed in the current authorization context. Contact your administrator if you believe this operation should be allowed.</li>
                          #     <li><strong>409</strong> Conflict. The request cannot be completed because it would result in an invalid state. See the response body for details.</li>
                          #     <li><strong>412</strong> The resource&apos;s current server-side ETag value does not match the supplied If-Match header value in the request. This indicates the resource has been modified by another consumer.</li>
                          #     <li><strong>500</strong> An unhandled error occurred on the server. See the response body for details.</li>
                          # </ul>"
               )),
             "admin" = data.frame(
               element = c(NA, "#adminTabs", "#connectionDetails", "#selectedOdsConnectionPermittedUsers", "#admin_updatePassword", NA),
               position = c(NA, NA, NA, "left", NA, NA),
               intro = c(
                   # Page Overview
                   "<p><strong>Welcome to the Admin page!</strong></p>
                              <p>If this is the first time logging in after installation, please use password &quot;admin&quot; and click <strong>SUBMIT!</strong> &nbsp;(No ODS connections will have been loaded at this point, and the connections can be configured after <u>re-setting the password</u>, which you will be prompted to do after first entry.)</p>
                              <p>There are three tabs in the Admin page:</p>
                              <ol>
                                  <li><strong>Existing Connections</strong> - allows Admin to view and edit existing ODS connections</li>
                                  <li><strong>New Connection</strong> - allows Admin to add new ODS connections</li>
                                  <li><strong>Upload Logs</strong> - allows Admin to review user upload logs for troubleshooting purposes</li>
                              </ol>
                   <p><i>We recommend closing out of this tutorial and relaunching it after you have entered/updated your admin password.</i>",
                   # New Connection
                   "<p><strong>Creating a New Connection</strong>
                   <p>Use the <b>New Connection</b> tab to pre-configure a new ODS connection for application users. The inputs in this tab will prompt you to designate a connection name, Ed-Fi ODS URL, API credentials, default namespace, and (optionally) a list of user names that should be able to access the connection (when they use the <b>Step 1 - Connect</b> screen).",
                   # Connection Details
                   "<p><strong>Connection Details</strong> 
                   <p> Once you have configured at least one ODS connection, it will show in the table in the <b>Existing Connections</b> tab, to the left, and this set of inputs here will allow you to update existing connection information. By selecting an existing connection from the table, the fields in this box will pre-populate for updating.</p>
                              ",
                   # User-Based Access
                   "<p><strong>What is User-Based Access?</strong></p>
                              <p>Turning on the <b>User-Based Access?</b> switch (right above this highlighted box) for a given connection will limit user access to it in the Survey Loader. 
                   <p>Instead of giving everyone in your network with the link access to the survey loader content, you can specify <strong>Permitted Users</strong> in your network. 
                   <p>As an example, you would use <strong>User Principal Name (UPN)</strong> in <strong>Active Directory</strong> separated by semicolon (;) for multiple users (e.g. bgood; tfoster). &nbsp;This user list will determine what connections the user sees in <strong>Step 1: Connect &gt; Choose a Data Store Connection. &nbsp;</strong></p>
                   <p>If all connections are User-Based and the user opening this survey loader app is not defined in the <strong>Permitted Users</strong> box, this user will not be able to select any ODS to load the survey (though they can enter their own ODS connection info manually, if they know it).</p>
                              ", 
                   # Update Password
                   "<p><strong>Update Password</strong></p>
                   <p>Use this button any time to update your admin password. Your new password will need to be at least 8 characters; passwords that have been exposed in a known breach (see <a href='https://haveibeenpwned.com/Passwords'>https://haveibeenpwned.com</a> for reference) will not be accepted.
                   <p><b>Be sure to record your password somewhere safe</b>, as, for security purposes, there is no way to recover it (other than the options in the next blurb) if you lose track of it!",
                   # Forgot Password
                   "<p><strong>What if I forget my Admin password?</strong></p>
                              <p>There are two ways to reset the Admin password. &nbsp;</p>
                              <ol>
                                  <li>If reconfiguring the ODS connections is not a burdensome task for your LEA, you can let the system re-generate the application database by removing the application's sqlite database at the root of the app folder.</li>
                                  <li>If you are hosting the Ed-Fi Survey Loader for multiple LEAs, open the database referenced in Step 1 and use a sha256 hash of a known text string to save to the application database and use that password to log in.</li>
                              </ol>
                              <p><i>While these workarounds may be necessary in certain circumstances, the app authors kindly remind you it is your/your organization's responsibility to ensure the security of the app and app database, according to your hosting/deployment approach.")
             )
      )
      
    })
    
    observeEvent(input$startIntro, {
        introjs(session, options = list(steps = introSteps()))
    })
    
    }

}

# Run the application 
shinyApp(ui = ui, server = server)
