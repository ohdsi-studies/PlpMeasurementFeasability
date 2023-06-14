# Load necessary packages
library(SqlRender) # SQL editor
library(DatabaseConnector) # SQL connector
library(ggplot2)
library(dplyr)
library(tensorflow)
library(data.table)
library(gt) # nicer output format
library(treemapify) # for pie chart

## 1. Counts (Measurements) Table by Percentages of Population & Percentages Distribution in all Databases

# SQL Query
sql <- "
  WITH person_measurement AS (
    SELECT m.person_id, m.measurement_concept_id
    FROM @cdmSchema.measurement m
    INNER JOIN @resultsSchema.cohort c ON m.person_id = c.subject_id
    WHERE c.cohort_definition_id = 5430
    AND ABS(DATEDIFF(DAY, m.measurement_date, c.cohort_start_date) / 
        CASE 
          WHEN DATEPART(YEAR, c.cohort_start_date) % 4 = 0 
          THEN 366.0
          ELSE 365.0
        END) < 1.0
  ),
  measurement_frequency AS (
    SELECT measurement_concept_id, COUNT(DISTINCT person_id) as person_count,
    ROUND(COUNT(DISTINCT person_id) * 100.0 / (SELECT COUNT(DISTINCT person_id) FROM person_measurement), 0) as frequency
    FROM person_measurement
    GROUP BY measurement_concept_id
  )
SELECT 
  '@cdmName' AS \"Database\",
  COUNT(CASE WHEN frequency >= 5 THEN 1 END) AS \"5% count\",
  COUNT(CASE WHEN frequency >= 10 THEN 1 END) AS \"10% count\",
  COUNT(CASE WHEN frequency >= 25 THEN 1 END) AS \"25% count\",
  COUNT(CASE WHEN frequency >= 50 THEN 1 END) AS \"50% count\",
  COUNT(CASE WHEN frequency >= 75 THEN 1 END) AS \"75% count\",
  COUNT(CASE WHEN frequency >= 95 THEN 1 END) AS \"95% count\",
  COUNT(CASE WHEN frequency = 100 THEN 1 END) AS \"100% count\"
FROM 
  measurement_frequency"

# List of CDMS
cdms <- list(
  # optum_ehr
  list(
    cdmName = "optum_ehr",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/optum_ehr",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_optum_ehr_v2447",
    resultsSchema = "results_optum_ehr_v2447"
  ),
  # Optum SES
  list(
    cdmName = "optum_ses",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/optum_extended_dod",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_optum_extended_dod_v2434",
    resultsSchema = "results_optum_extended_dod_v2434"
  ),
  # ccae
  list(
    cdmName = "ccae",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_ccae",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_ccae_v2435",
    resultsSchema = "results_truven_ccae_v2435"
  ),
  # mdcd
  list(
    cdmName = "mdcd",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_mdcd",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_mdcd_v2359",
    resultsSchema = "results_truven_mdcd_v2359"
  ),
  # mdcr
  list(
    cdmName = "mdcr",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_mdcr",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_mdcr_v2433",
    resultsSchema = "results_truven_mdcr_v2433"
  )
)

# Function to be applied to each CDM
processCdm <- function(cdm) {
  connection <- connect(cdm$connectionDetails)
  
  # Ensure that we disconnect even if there's an error
  on.exit(disconnect(connection))
  
  # Render and Translate the SQL
  df <- renderTranslateQuerySql(connection = connection, 
                                sql = sql, 
                                cdmSchema = cdm$cdmSchema, 
                                resultsSchema = cdm$resultsSchema,
                                cdmName = cdm$cdmName
  )
  
  # Disconnect
  disconnect(connection)
  
  # Assign to a new variable with a dynamic name
  assign(paste("dis", cdm$cdmName, sep = "_"), df, envir = .GlobalEnv)
}

# Apply the function to each CDM
lapply(cdms, processCdm)

dis_all <- rbind(dis_optum_ehr, dis_optum_ses, dis_ccae, dis_mdcd, dis_mdcr)

# Create a gt table
gt_table <- gt::gt(dis_all)

# Save the gt table
gt::gtsave(gt_table, "results/CountsForAllTable.html")


## 2.  Find common measurement w/o missing in units

### 2.1 Measurements' List in 5% Count

# SQL Query
sql <- "WITH person_measurement AS (
    SELECT m.person_id, m.measurement_concept_id
    FROM @cdmSchema.measurement m
    INNER JOIN @resultsSchema.cohort c ON m.person_id = c.subject_id
    WHERE c.cohort_definition_id = 5430
    AND ABS(DATEDIFF(DAY, m.measurement_date, c.cohort_start_date) / 
        CASE 
          WHEN DATEPART(YEAR, c.cohort_start_date) % 4 = 0 
          THEN 366.0
          ELSE 365.0
        END) < 1.0
  ),
  measurement_frequency AS (
    SELECT measurement_concept_id, COUNT(DISTINCT person_id) as person_count,
    ROUND(COUNT(DISTINCT person_id) * 100.0 / (SELECT COUNT(DISTINCT person_id) FROM person_measurement), 0) as frequency
    FROM person_measurement
    GROUP BY measurement_concept_id
  )
  SELECT frequency.measurement_concept_id, frequency.frequency, c.concept_name
  FROM measurement_frequency AS frequency
  INNER JOIN @cdmSchema.concept c ON frequency.measurement_concept_id = c.concept_id
  WHERE frequency.frequency >= 5
  ORDER BY frequency.frequency DESC"

# List of CDMS
cdms <- list(
  # optum_ehr
  list(
    cdmName = "optum_ehr",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/optum_ehr",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_optum_ehr_v2447",
    resultsSchema = "results_optum_ehr_v2447"
  ),
  # Optum SES
  list(
    cdmName = "optum_ses",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/optum_extended_dod",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_optum_extended_dod_v2434",
    resultsSchema = "results_optum_extended_dod_v2434"
  ),
  # ccae
  list(
    cdmName = "ccae",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_ccae",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_ccae_v2435",
    resultsSchema = "results_truven_ccae_v2435"
  ),
  # mdcd
  list(
    cdmName = "mdcd",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_mdcd",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_mdcd_v2359",
    resultsSchema = "results_truven_mdcd_v2359"
  ),
  # mdcr
  list(
    cdmName = "mdcr",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_mdcr",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_mdcr_v2433",
    resultsSchema = "results_truven_mdcr_v2433"
  )
)

# Function to be applied to each CDM
processCdm <- function(cdm) {
  connection <- connect(cdm$connectionDetails)
  
  # Ensure that we disconnect even if there's an error
  on.exit(disconnect(connection))
  
  # Render and Translate the SQL
  df <- renderTranslateQuerySql(connection = connection, 
                                sql = sql, 
                                cdmSchema = cdm$cdmSchema, 
                                resultsSchema = cdm$resultsSchema
  )
  
  # Disconnect
  disconnect(connection)
  
  # Assign to a new variable with a dynamic name
  assign(paste("percentage5", cdm$cdmName, sep = "_"), df, envir = .GlobalEnv)
}

# Apply the function to each CDM
lapply(cdms, processCdm)

percentage5_all <- rbind(percentage5_optum_ehr, percentage5_optum_ses, percentage5_ccae, percentage5_mdcd, percentage5_mdcr)
percentage5_all %>%
  head(20) %>%
  gt()

### 2.2 42 Common Measurements in 5% Count across Databases
# first, subset the relevant columns from each data frame
df1 <- percentage5_optum_ehr[c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")]
df2 <- percentage5_optum_ses[c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")]
df3 <- percentage5_ccae[c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")]
df4 <- percentage5_mdcd[c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")]
df5 <- percentage5_mdcr[c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")]

# then, create a list of these data frames
df_list <- list(df1, df2, df3, df4, df5)

# next, use Reduce() to find the intersection of all data frames
# common_rows <- Reduce(function(x, y) merge(x, y, by=c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")), df_list)

# inner join all data frames on MEASUREMENT_CONCEPT_ID and CONCEPT_NAME
common_rows <- df1 %>%
  inner_join(df2, by=c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")) %>%
  inner_join(df3, by=c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")) %>%
  inner_join(df4, by=c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")) %>%
  inner_join(df5, by=c("MEASUREMENT_CONCEPT_ID", "CONCEPT_NAME")) %>% arrange(CONCEPT_NAME)

# print the common rows
gt::gt(common_rows)
# save
common_rows_gt <- gt::gt(common_rows) 
gt::gtsave(common_rows_gt, "results/MeasurementConceptTable.html")


### 2.3 Find 38 common measurements w/o missing in units

# SQL Query
sql <- "
  SELECT 
    c2.concept_name AS measurement_concept_name,
    c2.concept_id AS measurement_concept_id,
    c3.concept_name AS unit_concept_name,
    m.unit_source_value,
    COUNT(*) AS count
  FROM 
    @cdmSchema.measurement m
  INNER JOIN 
    @resultsSchema.cohort c1
  ON 
    m.person_id = c1.subject_id
  INNER JOIN 
    @cdmSchema.concept c2 
  ON
    m.measurement_concept_id = c2.concept_id 
  INNER JOIN 
    @cdmSchema.concept c3
  ON
    COALESCE(m.unit_concept_id,0) = c3.concept_id
  WHERE 
    c1.cohort_definition_id = 5430
  AND 
    ABS(DATEDIFF(DAY, m.measurement_date, c1.cohort_start_date) / 
        CASE 
          WHEN DATEPART(YEAR, c1.cohort_start_date) % 4 = 0 
          THEN 366.0
          ELSE 365.0
        END) < 1.0
  AND 
    c2.concept_id IN (@common_ids)
  GROUP BY 
    c2.concept_name,
    c2.concept_id,
    c3.concept_name,
    m.unit_source_value
"

# List of CDMS
cdms <- list(
  # optum_ehr
  list(
    cdmName = "optum_ehr",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/optum_ehr",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_optum_ehr_v2447",
    resultsSchema = "results_optum_ehr_v2447"
  ),
  # Optum SES
  list(
    cdmName = "optum_ses",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/optum_extended_dod",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_optum_extended_dod_v2434",
    resultsSchema = "results_optum_extended_dod_v2434"
  ),
  # ccae
  list(
    cdmName = "ccae",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_ccae",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_ccae_v2435",
    resultsSchema = "results_truven_ccae_v2435"
  ),
  # mdcd
  list(
    cdmName = "mdcd",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_mdcd",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_mdcd_v2359",
    resultsSchema = "results_truven_mdcd_v2359"
  ),
  # mdcr
  list(
    cdmName = "mdcr",
    connectionDetails = createConnectionDetails(dbms="redshift", 
                                                server="***REMOVED***/truven_mdcr",
                                                user = "***REMOVED***", 
                                                password = keyring::key_get("ohda_prob_1"), 
                                                port = 5439),
    cdmSchema = "cdm_truven_mdcr_v2433",
    resultsSchema = "results_truven_mdcr_v2433"
  )
)

# Function to be applied to each CDM
processCdm <- function(cdm) {
  connection <- connect(cdm$connectionDetails)
  
  # Ensure that we disconnect even if there's an error
  on.exit(disconnect(connection))
  
  # Render and Translate the SQL
  df <- renderTranslateQuerySql(connection = connection, 
                                sql = sql, 
                                cdmSchema = cdm$cdmSchema, 
                                resultsSchema = cdm$resultsSchema, 
                                common_ids = common_rows$MEASUREMENT_CONCEPT_ID) %>%
    arrange(MEASUREMENT_CONCEPT_NAME)
  
  # Disconnect
  disconnect(connection)
  
  # Assign to a new variable with a dynamic name
  assign(paste("result", cdm$cdmName, sep = "_"), df, envir = .GlobalEnv)
}

# Apply the function to each CDM
lapply(cdms, processCdm)

df_list <- list(result_optum_ehr = result_optum_ehr, 
                result_optum_ses = result_optum_ses, 
                result_ccae = result_ccae, 
                result_mdcd = result_mdcd, 
                result_mdcr = result_mdcr)

df_all <- list()

for(i in names(df_list)){
  df <- df_list[[i]]
  
  result <- df %>% 
    filter(!is.na(UNIT_SOURCE_VALUE) & UNIT_CONCEPT_NAME != 'No matching concept') %>% 
    select(MEASUREMENT_CONCEPT_ID, MEASUREMENT_CONCEPT_NAME, UNIT_SOURCE_VALUE, UNIT_CONCEPT_NAME)
  
  df_all[[i]] <- result
}

df_combined <- bind_rows(df_all, .id = "DATABASE")

# Define a helper function to remove duplicates from a character vector
remove_duplicates <- function(x) {
  return(paste(unique(x), collapse = ","))
}

# Assuming df_combined is your data frame
df_result <- df_combined %>%
  group_by(MEASUREMENT_CONCEPT_ID, MEASUREMENT_CONCEPT_NAME) %>%
  summarise(
    COUNT_OF_DATABASES = n_distinct(DATABASE),
    DATABASES = remove_duplicates(DATABASE)
  ) %>%
  arrange(desc(COUNT_OF_DATABASES))

# Output the df_result table as an HTML file
library(knitr)
library(kableExtra)

# Convert the df_result table to a formatted HTML table
df_result_knit <- knitr::kable(df_result, format = "html") %>%
  kableExtra::kable_styling(full_width = FALSE)

df_result_knit
# Save the HTML table to a file
writeLines(df_result_knit, con = "results/result_table.html")

## 3. 75% measurements (22) in optum_ehr

### 3.1 Building Connection with Database

connectsample = createConnectionDetails(dbms="redshift", server="***REMOVED***/optum_ehr",
                                        user = "***REMOVED***", password = keyring::key_get("ohda_prob_1"), port = 5439)
connection = connect(connectsample)

### 3.2 Measurements' List in 75% Count
optum_ehr1 <- renderTranslateQuerySql(connection, 
                               "WITH person_measurement AS (
    SELECT m.person_id, m.measurement_concept_id
    FROM cdm_optum_ehr_v2447.measurement m
    INNER JOIN results_optum_ehr_v2447.cohort c ON m.person_id = c.subject_id
    WHERE c.cohort_definition_id = 5430
    AND ABS(DATEDIFF(DAY, m.measurement_date, c.cohort_start_date) / 
        CASE 
          WHEN DATEPART(YEAR, c.cohort_start_date) % 4 = 0 
          THEN 366.0
          ELSE 365.0
        END) < 1.0
  ),
  measurement_frequency AS (
    SELECT measurement_concept_id, COUNT(DISTINCT person_id) as person_count,
    ROUND(COUNT(DISTINCT person_id) * 100.0 / (SELECT COUNT(DISTINCT person_id) FROM person_measurement), 0) as frequency
    FROM person_measurement
    GROUP BY measurement_concept_id
  )
  SELECT frequency.measurement_concept_id, frequency.frequency, c.concept_name
  FROM measurement_frequency AS frequency
  INNER JOIN cdm_optum_ehr_v2447.concept AS c ON frequency.measurement_concept_id = c.concept_id
  WHERE frequency.frequency >= 75
  ORDER BY frequency.frequency DESC")

#### If 'UNIT_CONCEPT_NAME' = 'No matching concept', then 'UNIT_SOURCE_VALUE' = 'others'

optum_ehr2 = renderTranslateQuerySql(connection, 
                              "WITH measurement_units AS (
  SELECT 
    c2.concept_name AS measurement_concept_name,
    c3.concept_name AS unit_concept_name,
    CASE 
      WHEN c3.concept_name = 'No matching concept' THEN 'others'
      ELSE m.unit_source_value
    END AS unit_source_value,
    COUNT(*) AS count
  FROM 
    cdm_optum_ehr_v2447.measurement m
  INNER JOIN 
    results_optum_ehr_v2447.cohort c1
  ON 
    m.person_id = c1.subject_id
  INNER JOIN 
    cdm_optum_ehr_v2447.concept c2 
  ON
    m.measurement_concept_id = c2.concept_id 
  INNER JOIN 
    cdm_optum_ehr_v2447.concept c3
  ON
    m.unit_concept_id = c3.concept_id
  WHERE 
    c1.cohort_definition_id = 5430
  AND 
    ABS(DATEDIFF(DAY, m.measurement_date, c1.cohort_start_date) / 
        CASE 
          WHEN DATEPART(YEAR, c1.cohort_start_date) % 4 = 0 
          THEN 366.0
          ELSE 365.0
        END) < 1.0
  GROUP BY 
    c2.concept_name, 
    c3.concept_name,
    CASE 
      WHEN c3.concept_name = 'No matching concept' THEN 'others'
      ELSE m.unit_source_value
    END
),
total_counts AS (
  SELECT 
    measurement_concept_name,
    SUM(count) AS total_count
  FROM 
    measurement_units
  GROUP BY 
    measurement_concept_name
)
SELECT 
  mu.measurement_concept_name,
  mu.unit_concept_name,
  mu.unit_source_value,
  mu.count,
  tc.total_count,
  CAST(mu.count AS FLOAT) / tc.total_count * 100 AS percentage
FROM 
  measurement_units mu
INNER JOIN 
  total_counts tc 
ON 
  mu.measurement_concept_name = tc.measurement_concept_name
ORDER BY 
  mu.measurement_concept_name, 
  percentage DESC
")

measurements_75 <- optum_ehr2[optum_ehr2$MEASUREMENT_CONCEPT_NAME %in% optum_ehr1$CONCEPT_NAME, ]

# Ordering the data frame
measurements_75 <- measurements_75 %>%
  filter(PERCENTAGE > 10) %>%
  select(MEASUREMENT_CONCEPT_NAME, UNIT_CONCEPT_NAME,UNIT_SOURCE_VALUE, PERCENTAGE) %>%
  arrange(MEASUREMENT_CONCEPT_NAME, desc(PERCENTAGE))

measurements_75 <- rename(measurements_75, Measurement = MEASUREMENT_CONCEPT_NAME)

# filter out "No matching concept"
measurements_75 <- subset(measurements_75, UNIT_CONCEPT_NAME != "No matching concept")

measurements_75 <- measurements_75 %>%
  group_by(Measurement) %>%
  summarise(
    Units = paste(UNIT_CONCEPT_NAME, collapse = ", "),
    `Unit Source Values` = paste(UNIT_SOURCE_VALUE, collapse = ", "),
    `Percent Coverage` = paste0(round(sum(PERCENTAGE), 2), "%")
  )

# Creating the gt table
gt(measurements_75)

# Saving to HTML
gt::gtsave(gt(measurements_75), "results/measurements_75.html")


## 4. Pie Chart for "Body Weight" in optum_ehr

optum_ehr2_bw = optum_ehr2 %>%
  filter(MEASUREMENT_CONCEPT_NAME == "Body weight")%>% arrange(desc(PERCENTAGE))

# Update UNIT_SOURCE_VALUE if UNIT_CONCEPT_NAME is "pound (US)"
optum_ehr2_bw[optum_ehr2_bw$UNIT_CONCEPT_NAME == "pound (US)", "UNIT_SOURCE_VALUE"] <- "others"

# Create a separate data frame for "pound (US)" and "others", recalculate the counts and percentages
pound_us_others <- optum_ehr2_bw %>%
  filter(UNIT_CONCEPT_NAME == "pound (US)") %>%
  group_by(UNIT_CONCEPT_NAME, UNIT_SOURCE_VALUE) %>%
  summarise(COUNT = sum(COUNT), .groups = "drop") %>%
  mutate(PERCENTAGE = COUNT / sum(optum_ehr2_bw$COUNT) * 100) %>%
  mutate(MEASUREMENT_CONCEPT_NAME = "Body weight") %>%
  mutate(TOTAL_COUNT = 86232548)

# Remove all rows with "pound (US)" from the original data frame
optum_ehr2_bw <- optum_ehr2_bw %>%
  filter(UNIT_CONCEPT_NAME != "pound (US)")

# Add the updated "pound (US)" data back to the original data frame
optum_ehr2_bw <- bind_rows(optum_ehr2_bw, pound_us_others)

# Arrange the data frame in descending order of PERCENTAGE
optum_ehr2_bw <- optum_ehr2_bw %>%
  arrange(desc(PERCENTAGE))

# Identify rows where PERCENTAGE is below 1
small_percentage_rows <- optum_ehr2_bw[optum_ehr2_bw$PERCENTAGE < 1, ]

# Calculate the sum of PERCENTAGE and COUNT for those rows
combined_row <- data.frame(
  UNIT_CONCEPT_NAME = "other small unit concept names",
  UNIT_SOURCE_VALUE = "other small unit source values",
  MEASUREMENT_CONCEPT_NAME = "Body weight",
  TOTAL_COUNT = first(small_percentage_rows$TOTAL_COUNT),
  COUNT = sum(small_percentage_rows$COUNT),
  PERCENTAGE = sum(small_percentage_rows$PERCENTAGE)
)

# Remove the individual rows
optum_ehr2_bw <- optum_ehr2_bw[!optum_ehr2_bw$PERCENTAGE < 1, ]

# Append the combined row to the dataframe
optum_ehr2_bw <- rbind(optum_ehr2_bw, combined_row)

# Arrange the dataframe in descending order of PERCENTAGE
optum_ehr2_bw <- optum_ehr2_bw[order(-optum_ehr2_bw$PERCENTAGE), ]

### pie chart for body weight

# Group by UNIT_LABEL and count rows in each group
optum_ehr2_bw$UNIT_LABEL <- paste(optum_ehr2_bw$UNIT_CONCEPT_NAME, "(", optum_ehr2_bw$UNIT_SOURCE_VALUE, ")", sep = "")

# Create new labels for the legend that include the percentages
optum_ehr2_bw$LEGEND_LABEL <- paste(optum_ehr2_bw$UNIT_LABEL, " (", round(optum_ehr2_bw$PERCENTAGE, 2), "%)", sep = "")

# Generate the list of labels for scale_fill_discrete
labels <- as.character(optum_ehr2_bw$LEGEND_LABEL)

treemap_chart <- ggplot(optum_ehr2_bw, aes(area = PERCENTAGE, fill = UNIT_LABEL, 
                                    label = paste(UNIT_LABEL, "\n", round(PERCENTAGE, 2), "%"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 8) +
  theme_minimal() +
  labs(title = "Measurement: Body Weight",
       x = "", 
       y = "",
       fill = "Unit Label",
       caption = "Data Source: Optum EHR") +
  scale_fill_discrete(labels = labels) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.5, "cm"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.caption = element_text(hjust = 2.5, vjust=3))

print(treemap_chart)

