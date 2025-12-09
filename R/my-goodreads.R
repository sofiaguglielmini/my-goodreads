library(dplyr)
library(stringr)
library(lubridate)
library(WikidataQueryServiceR)
library(httr)
library(jsonlite)
library(ggplot2)
library(rnaturalearth)
library(epitools)
library(tidyr)
library(widyr)
library(igraph)
library(ggraph)
library(scales)
library(glmnet)
library(randomForest)
data <- read.csv("../goodreads_library_export.csv")
# Table with description of variables
data_dict <- data.frame(
  Variable = colnames(data),
  Description = c("Goodreads book ID",
                  "Book title",
                  "Book author",
                  "Book author (last name, first name)",
                  "Additional authors",
                  "ISBN number",
                  "ISBN-13 number",
                  "My rating (1-5 stars)",
                  "Average rating by all Goodreads users",
                  "Publisher",
                  "Binding (hardcover, paperback, etc.)",
                  "Number of pages",
                  "Year of publication of the edition I read",
                  "Original publication year",
                  "Date read",
                  "Date added to my Goodreads library",
                  "What custom book shelves I have assigned the book to",
                  "Position in the bookshelf",
                  "Read, to-read, currently-reading",
                  "My review of the book",
                  "Spoilers in the review (TRUE/FALSE)",
                  "Private notes about the book",
                  "Times read",
                  "Number of copies I own")
)
data_dict$Variable <- paste0("`", data_dict$Variable, "`")

data <- data %>%
  mutate(
    Date.Added = as.Date(Date.Added),
    Date.Read  = as.Date(Date.Read),
    ISBN13     = str_remove_all(ISBN13, '^="|"$'),
    ISBN       = str_remove_all(ISBN, '^="|"$'),
    Title      = gsub("\\s*\\([^\\)]+\\)", "", Title),
    Title      = str_replace_all(Title, c("&" = "&amp;", '"' = '\\"')),
    Original.Publication.Year = ifelse(Original.Publication.Year <= 0, NA, Original.Publication.Year),
    My.Rating  = na_if(My.Rating, 0),
    Number.of.Pages = na_if(Number.of.Pages, 0))

# I started recording in 2020
data$Date.Read[data$Date.Read < as.Date("2020-01-01")] <- NA

get_author_info <- function(author) {
  
  # SPARQL query
  query <- paste0('
    SELECT ?author ?authorLabel # name
           ?P21 ?P21Label       # gender
           ?P27 ?P27Label       # nationality
    WHERE {
      ?author rdfs:label "', author, '"@en.
      OPTIONAL { ?author wdt:P21 ?P21. }
      OPTIONAL { ?author wdt:P27 ?P27. }
      SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    } LIMIT 1
  ')
  
  # run the query
  res <- query_wikidata(query)
  
  # keep the properties of interest
  res <- res %>% select(authorLabel, P21Label, P27Label)
  return(res)
}

get_book_info <- function(title, author) {
  
  # url of the search "Title Author" on openlibrary
  url <- paste0("https://openlibrary.org/search.json?q=", URLencode(paste(title, author)))
  res <- try(GET(url), silent = TRUE)
  if(inherits(res, "try-error")) return(NA) 
  
  # retrieve the content of the request as a character vector (text)
  jsonfile <- content(res, "text", encoding = "UTF-8")
  
  # read it as an R object
  data <- fromJSON(jsonfile)
  if(is.null(data$docs) || length(data$docs) == 0) return(NA)
  key <- data$docs$key[1] # key of the first result
  if(is.null(key)) return(NA)
  
  # use the key in the url to get the book details
  work_url <- paste0("https://openlibrary.org", key, ".json")
  res2 <- try(GET(work_url), silent = TRUE)
  if(inherits(res2, "try-error")) return(NA)
  
  data2 <- fromJSON(content(res2, "text", encoding = "UTF-8"))
  if(is.null(data2$subjects)) return(NA)
  subjects_clean <- data2$subjects[!is.na(data2$subjects) & data2$subjects != ""]
  out <- c(title, paste(data2$subjects, collapse = "; "))
  return(out) 
}

# author_data <- lapply(unique(data[,"Author"]), get_author_info)
# author_data <- do.call(rbind, author_data)
# colnames(author_data) <- c("Author", "AuthorGender", "AuthorNationality")
# write.csv(author_data, "author_data")
# 
# book_data <- lapply(seq_len(nrow(data)), function(i) {
#   get_book_info(data$Title[i], data$Author[i])
# })
# book_data <- as.data.frame(do.call(rbind, book_data))
# colnames(book_data) <- c("Title", "Subjects")
# write.csv(book_data, "book_data")

author_data <- read.csv("author_data", stringsAsFactors = FALSE, row.names = 1)
book_data <- read.csv("book_data", stringsAsFactors = FALSE, row.names = 1)
mybooks <- data %>%
  left_join(book_data, by = "Title") %>%
  left_join(author_data, by = "Author")

mybooks <- mybooks %>% select(Title, Author, My.Rating, Average.Rating, Number.of.Pages, Original.Publication.Year, Date.Read, Exclusive.Shelf, Subjects, AuthorGender, AuthorNationality)
mybooks <- mybooks %>%
  mutate(Season.Read = case_when(month(Date.Read) %in% c(12, 1, 2)  ~ "Winter",
                                 month(Date.Read) %in% c(3, 4, 5)   ~ "Spring",
                                 month(Date.Read) %in% c(6, 7, 8)   ~ "Summer",
                                 month(Date.Read) %in% c(9, 10, 11) ~ "Autumn",
                                 TRUE ~ NA_character_))
# For NA, put Unknown
mybooks <- mybooks %>%
  mutate(AuthorNationality = case_when(AuthorNationality == "United States" ~ "United States of America",
                                       AuthorNationality %in% c("United Kingdom of Great Britain and Ireland", "Kingdom of Great Britain") ~ "United Kingdom",
                                       AuthorNationality == "People's Republic of China" ~ "China",
                                       AuthorNationality == "Kingdom of the Netherlands" ~ "Netherlands",
                                       AuthorNationality == "Kingdom of Italy" ~ "Italy",
                                       AuthorNationality == "Russian Empire" ~ "Russia",
                                       TRUE ~ AuthorNationality)) %>%
  mutate(AuthorNationality = ifelse(is.na(AuthorNationality), "Unknown", AuthorNationality))

mybooks <- mybooks %>%
  mutate(AuthorGender = case_when(AuthorGender == "trans man"   ~ "male",
                                  AuthorGender == "trans woman" ~ "female",
                                  TRUE ~ AuthorGender))

# Table with description of variables
data_dict2 <- data.frame(
  Variable = colnames(mybooks),
  Description = c("Book title",
                  "Book author",
                  "My rating (1-5 stars)",
                  "Average rating by all Goodreads users",
                  "Number of pages",
                  "Original publication year",
                  "Date read",
                  "Read, to-read, currently-reading",
                  "Tags/subjects from Open Library",
                  "Author gender",
                  "Author nationality",
                  "Season read"))
data_dict2$Variable <- paste0("`", data_dict2$Variable, "`")

# Set == testing is to-read, Set == training on 70% of read, Set == validation on 30% of read
set.seed(1)
mybooks <- mybooks %>%
  mutate(Set = case_when(Exclusive.Shelf == "to-read" ~ "Test",
                         Exclusive.Shelf == "read" & runif(n()) <= 0.7 ~ "Train",
                         Exclusive.Shelf == "read" & runif(n()) > 0.7 ~ "Validation"))
booksread <- mybooks %>% filter(Exclusive.Shelf == "read")
bookstrain <- booksread %>% filter(Set == "Train")
bookstbr <- mybooks %>% filter(Exclusive.Shelf == "to-read")

my_theme <- theme(axis.title.x = element_text(vjust = 0, size = 14),
                  axis.title.y = element_text(vjust = 2, size = 14),
                  axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 12),
                  axis.text.y = element_text(size = 12),
                  plot.title = element_text(hjust = 0, size = 15, face = "bold.italic")) + theme_minimal()

theme_set(my_theme)

ggplot(bookstrain %>% filter(!is.na(Date.Read)),
       aes(x = Date.Read)) +
  geom_histogram(binwidth = 100, fill = "lightsteelblue", color = "white") +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  labs(title = "Books read over time", x = "Date", y = "Number of books") +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-10-07"), ymin = -Inf, ymax = Inf, fill = "lightsteelblue4", alpha = 0.15) +
  annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-09-22"), ymin = -Inf, ymax = Inf, fill = "lightsteelblue4", alpha = 0.15)

ggplot(bookstrain %>% filter(!is.na(Season.Read)), aes(x = Season.Read, fill = Season.Read)) +
  geom_bar() +
  scale_fill_manual(values = c("Autumn" = "#D6A166", "Winter" = "#A1A5EC", "Spring" = "#7EBA68", "Summer" = "#ED90A4")) +
  labs(title = "Books by season read", x = "Season", y = "Number of books") +
  theme(legend.title = element_blank())

avg_pages <- mean(bookstrain$Number.of.Pages[bookstrain$Number.of.Pages < 2000], na.rm = TRUE)
prop_long <- sum(bookstrain$Number.of.Pages > 500 & bookstrain$Number.of.Pages < 2000, na.rm=T)/sum(!is.na(bookstrain$Number.of.Pages))
prop_short <- sum(bookstrain$Number.of.Pages < 200, na.rm=T)/sum(!is.na(bookstrain$Number.of.Pages))
cor_pages <- cor(bookstrain$Number.of.Pages, bookstrain$My.Rating, use = "complete.obs")

books_per_npages <- bookstrain %>%
  filter(!is.na(Number.of.Pages) & Number.of.Pages < 2000) %>%
  group_by(Pages = (Number.of.Pages %/% 50)*50) %>%
  summarize(count = n())

# Exclude outliers (collections)
ggplot(bookstrain %>%
         filter(!is.na(Number.of.Pages) & Number.of.Pages < 2000),
       aes(x = Number.of.Pages)) +
  geom_histogram(binwidth = 100, fill = "lightsteelblue", color="white") +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Books read by number of pages", x = "Pages", y = "Number of books")

ggplot(bookstrain %>% filter(!is.na(Number.of.Pages) & Number.of.Pages < 2000),
       aes(x = Number.of.Pages, y = My.Rating)) +
  geom_jitter(width = 5, height = 0.2, alpha = 0.5, color = "lightsteelblue4", pch="\u2605", cex=5) +
  stat_smooth(color = "lightsteelblue3", fill = "lightsteelblue1", method="lm") +
  scale_x_continuous(n.breaks = 20) +
  labs(title = "Ratings by number of pages", x = "Number of pages", y = "Rating")
cor_year <- cor(bookstrain$Original.Publication.Year, bookstrain$My.Rating, use = "complete.obs")

ggplot(bookstrain %>% filter(!is.na(Original.Publication.Year) & Original.Publication.Year > 1750),
       aes(x = Original.Publication.Year)) +
  geom_histogram(binwidth = 10, fill = "lightsteelblue", color="white") +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Books read by publication year", x = "Year", y = "Number of books")

ggplot(bookstrain %>% filter(!is.na(Original.Publication.Year) & Original.Publication.Year > 1830), aes(x = Original.Publication.Year, y = My.Rating)) +
  geom_jitter(width = 0.5, height = 0.2, alpha = 0.5, color = "lightsteelblue4",  pch="\u2605", cex=5) +
  stat_smooth(color = "lightsteelblue3", fill = "lightsteelblue1", method="lm") +
  scale_x_continuous(n.breaks = 10) +
  labs(title = "Ratings by publication year", x = "Year", y = "Rating")
world <- ne_countries(scale = "medium", returnclass = "sf")
country_counts <- bookstrain %>% filter(!is.na(AuthorNationality)) %>% count(AuthorNationality)
world_counts <- world %>% left_join(country_counts, by = c("name" = "AuthorNationality")) %>% mutate(books_per_capita = (n / pop_est)*1000000)

# Plot
ggplot(world_counts) +
  geom_sf(aes(fill = books_per_capita)) +
  scale_fill_viridis_c(option = "plasma", na.value = "gray93", trans = "sqrt") +
  labs(title = "Books read per author nationality") +
  theme(legend.title = element_blank())

n_gender <- sum(!is.na(bookstrain$AuthorGender))
n_male <- sum(bookstrain$AuthorGender == "male", na.rm = TRUE)
n_female <- sum(bookstrain$AuthorGender == "female", na.rm = TRUE)
avg_rating <- bookstrain %>% filter(!is.na(AuthorGender) & !is.na(My.Rating)) %>%
  group_by(AuthorGender) %>%
  summarise(avg_rating = mean(My.Rating, na.rm = TRUE)) %>%
  mutate(x = ifelse(AuthorGender == "male", -20, 20))  # x per gender for plotting average ratings

# test for difference in means
t_test <- t.test(My.Rating ~ AuthorGender, data = bookstrain %>% filter(!is.na(AuthorGender) & !is.na(My.Rating)))

# Compute confidence intervals
ci_male <- epitools::binom.exact(n_male, n_gender)
ci_female <- epitools::binom.exact(n_female, n_gender)

group <- c(rep("My authors", 2), rep("Population", 2))
gender <- rep(c("Female", "Male"), 2)
prop <- c(n_female/n_gender, n_male/n_gender, 0.33, 0.67)
lower <- c(ci_female$lower, ci_male$lower, NA, NA)
upper <- c(ci_female$upper, ci_male$upper, NA, NA)
gender_props <- data.frame(group, gender, prop, lower, upper)
gender_props$gender <- factor(gender_props$gender, levels = c("Male", "Female"))

ggplot(gender_props, aes(fill=gender, y=prop, x=group)) +
  geom_bar(position="stack", stat="identity") +
  geom_hline(yintercept = 0.5, linewidth = 0.5, linetype = 2, col = "grey30") +
  geom_errorbar(data = gender_props %>% filter(group == "My authors", gender == "Female"),
                aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_point(data = gender_props %>% filter(group == "My authors", gender == "Female"), size = 2, show.legend = FALSE) +
  labs(title = "Books read by author gender", x = "", y = "Proportion of books") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#A1A5EC", "#ED90A4"))

ratings_gender <- bookstrain %>% filter(!is.na(AuthorGender) & !is.na(My.Rating)) %>%
  count(AuthorGender, My.Rating) %>%
  mutate(count2 = ifelse(AuthorGender == "male", -n, n),
         My.Rating = factor(My.Rating, levels=1:5))

ggplot(ratings_gender, aes(x = count2, y = My.Rating, fill = AuthorGender)) +
  geom_col(width = 0.8) +
  scale_fill_manual(
    name = "Author Gender",
    values = c("female" = "#ED90A4", "male" = "#A1A5EC"),
    labels = c("Female", "Male"),
    guide = guide_legend(override.aes = list(shape = 15, color = NA))  # remove black border
  ) +
  geom_point(
    data = avg_rating,
    aes(x = x, y = avg_rating, shape = "Average"),
    color = "black", size = 5,
    show.legend = TRUE,
    inherit.aes = FALSE
  ) +
  scale_shape_manual(name = "", values = c("Average" = "\u2605"), label = "Average\n rating") +
  scale_x_continuous(labels = abs, name = "Books") +
  scale_y_discrete(name = "Rating") +
  labs(title = "Ratings by author gender") +
  theme(legend.title = element_blank())

ggplot(bookstrain %>% filter(!is.na(My.Rating) & !is.na(Average.Rating)),
       aes(x = Average.Rating, y = My.Rating)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5, color = "lightsteelblue4", pch="\u2605", cex=5) +
  stat_smooth(color = "lightsteelblue3", fill = "lightsteelblue1", method="lm", formula="y~poly(x,2)") +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(breaks = 1:5) +
  labs(title = "My ratings vs. average ratings", x = "Average rating", y = "My rating")

mybooks_tags <- mybooks %>%
  filter(!is.na(Subjects)) %>%
  separate_rows(Subjects, sep = "; ") %>%
  mutate(Subjects = str_replace_all(Subjects, ",", "")) %>%
  mutate(Subjects = tolower(Subjects)) %>%
  mutate(Subjects = iconv(Subjects, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  # Remove "general" and "fiction" when inside phrases
  mutate(Subjects = ifelse(Subjects != "fiction",
                           str_replace_all(Subjects, "\\b(general|fiction)\\b", "") %>% str_squish(),
                           Subjects)) %>%
  mutate(Subjects = str_replace_all(Subjects, c(
    "science fiction" = "sci-fi",
    "ciencia-ficcion" = "sci-fi",
    "ficcion" = "fiction",
    "fiction, general" = "fiction",
    "fiction & literature" = "fiction",
    "fiction / thriller" = "thriller",
    "fiction / suspanse" = "suspanse",
    "gender studies" = "gender",
    "fantasy fiction" = "fantasy",
    "historical fiction" = "historical",
    "young adult fiction" = "young adult",
    "children's fiction" = "children's",
    "graphic novels" = "graphic novel",
    "graphic fiction" = "graphic novel",
    "nonfiction" = "non-fiction",
    "memoirs" = "memoir",
    "biographies" = "biography",
    "history" = "historical",
    "literary fiction" = "fiction",
    "classics" = "classic",
    "novels" = "novel",
    "short stories" = "short story",
    "poetry" = "poem",
    "essays" = "essay",
    "crime fiction" = "crime",
    "mystery fiction" = "mystery",
    "thrillers" = "thriller",
    "horror fiction" = "horror",
    "romance fiction" = "romance",
    "fiction / literary" = "fiction",
    "science-fiction" = "sci-fi",
    "fiction historical general" = "historical",
    "fiction sci-fi general" = "sci-fi",
    "families" = "family",
    "fiction family life" = "family",
    "friendship fiction" = "friendship",
    "romans" = "romance",
    "fiction historical" = "historical",
    "fiction mystery & detective general" = "mystery",
    "fiction thriller suspense" = "thriller",
    "histoire" = "historical",
    "fiction humorous general" = "humor",
    "biografia" = "biography",
    "family life" = "family",
    "fiction / coming of age" = "coming of age",
    "fiction coming of age" = "coming of age",
    "fiction classic" = "classic"
  )))  %>%
  filter(!Subjects %in% c("fiction", "open_syllabus_project", "general", "",
                          "long now manual for civilization",
                          "large type books", "open library staff picks", "chang pian xiao shuo")) %>%
  mutate(Subjects = str_replace_all(Subjects, "[,\\(\\)&/\\-']", ""))

tags <- mybooks_tags %>%
  group_by(SubjectList = Subjects) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# only for read books, with at least 10 occurrences
tags_train <- mybooks_tags %>%
  filter(Set == "Train") %>%
  group_by(SubjectList = Subjects) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  filter(Count >= 10)

ggplot(tags_train, aes(x = reorder(SubjectList, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightsteelblue") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(title = "Most Common Tags", x = "Tag", y = "Occurrence in Read Books") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = 0, size = 14),
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0, size = 20, face = "bold.italic"))

tags_cooccur <- mybooks_tags %>%
  filter(Set == "Train") %>%
  select(Title, Subjects) %>%
  distinct() %>%
  pairwise_count(item = Subjects, feature = Title, sort = TRUE)
tags_cooccur <- tags_cooccur %>% filter(n >= 3)  # only keep edges with at least 3 co-occurrences

tags_nodes <- data.frame(name = unique(c(tags_cooccur$item1, tags_cooccur$item2)))
tags_edges <- tags_cooccur %>% rename(from = item1, to = item2, weight = n)
tags_graph <- graph_from_data_frame(d = tags_edges, vertices = tags_nodes, directed = FALSE)
V(tags_graph)$degree <- scales::rescale(degree(tags_graph), to = c(3,12))

ggraph(tags_graph, layout = "kk") +
  geom_edge_link(aes(alpha = rescale(weight)), width = 1, color = "gray70", show.legend = TRUE) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point(aes(size = rescale(degree)), color = "lightsteelblue3", show.legend = FALSE) +
  geom_node_text(aes(label = str_wrap(name, width = 10)), size = 4, max.overlaps = 10, repel = TRUE, family = "sans") +
  theme_void() + 
  labs(title = "Tag Co-Occurrence Network") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold.italic"))
# remove NAs in the features we want to use
mybooks_clean <- mybooks %>% filter(!is.na(Average.Rating) & !is.na(Number.of.Pages) & !is.na(Original.Publication.Year))

# Transform Authorgender into a dummy
mybooks_clean$FemaleAuthor <- ifelse(mybooks_clean$AuthorGender == "female", 1, 0)
mybooks_clean$FemaleAuthor[is.na(mybooks_clean$AuthorGender)] <- 0
mybooks_clean <- mybooks_clean %>% select(-AuthorGender)

# create a dummy column for each author nationality and keep the 10 most common
top_nationalities <- mybooks_clean %>%
  group_by(AuthorNationality) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  pull(AuthorNationality)

# attach the dummies for nationalities, without spaces
for(nation in top_nationalities) {
  mybooks_clean[[gsub(" ", "_", nation)]] <- ifelse(mybooks_clean$AuthorNationality == nation, 1, 0)
}
mybooks_clean <- mybooks_clean %>% select(-AuthorNationality)

top_tags <- tags$SubjectList[1:100]
for(tag in top_tags) {
  # remove space at the beginning and end, substitute with _ if in the middle
  clean_tag <- trimws(tag)
  col_name <- gsub(" ", "_", clean_tag)
  mybooks_clean[[col_name]] <- ifelse(grepl(paste0("\\b", clean_tag, "\\b"), mybooks_clean$Subjects), 1, 0)
}
mybooks_clean <- mybooks_clean %>% select(-Subjects)

# log-transform the response variable
# mybooks_clean$LogMyRating <- log(mybooks_clean$My.Rating)

read_clean <- mybooks_clean %>% filter(Exclusive.Shelf == "read")
tbr_clean <- mybooks_clean %>% filter(Exclusive.Shelf == "to-read")

# Predict my rating based on book features
# remove variables that are not predictors

read_clean <- read_clean %>% select(-c(Title, Author, Date.Read, Exclusive.Shelf, Season.Read))

# Use log-transform as response, remove NAs in response
read_clean <- read_clean %>% filter(!is.na(My.Rating))

# split read books into training and validation sets
validation <- read_clean %>% filter(Set == "Validation")
training <- read_clean %>% filter(Set == "Train")
testing <- tbr_clean

# first, scale the numeric predictors
training <- training %>%
  mutate(Average.Rating = scale(Average.Rating),
         Number.of.Pages = scale(Number.of.Pages),
         Original.Publication.Year = scale(Original.Publication.Year))

validation <- validation %>%
  mutate(Average.Rating = scale(Average.Rating),
         Number.of.Pages = scale(Number.of.Pages),
         Original.Publication.Year = scale(Original.Publication.Year))

testing <- testing %>%
  mutate(Average.Rating = scale(Average.Rating),
         Number.of.Pages = scale(Number.of.Pages),
         Original.Publication.Year = scale(Original.Publication.Year))

# Identify predictor columns present in all sets
predictor_cols <- intersect(intersect(colnames(training), colnames(validation)), colnames(testing))

# Remove response from predictors
predictor_cols <- predictor_cols[!predictor_cols %in% c("My.Rating", "Set")]

# Prepare the model matrix and the response in both sets
x_train <- as.matrix(training[, predictor_cols])

# we include the quadratic term for Average.Rating, as per our exploratory analysis
x_train <- cbind(x_train, training$Average.Rating^2)
colnames(x_train)[ncol(x_train)] <- "Average.Rating2"
y_train <- training$My.Rating
xy_train <- as.data.frame(cbind(y_train, x_train))
colnames(xy_train)[1] <- "My.Rating"

x_val <- as.matrix(validation[, predictor_cols])
x_val <- cbind(x_val, validation$Average.Rating^2)
colnames(x_val)[ncol(x_val)] <- "Average.Rating2"
y_val <- validation$My.Rating
xy_val <- as.data.frame(cbind(y_val, x_val))
colnames(xy_val)[1] <- "My.Rating"

x_test <- as.matrix(testing[, predictor_cols])
x_test <- cbind(x_test, testing$Average.Rating^2)
colnames(x_test)[ncol(x_test)] <- "Average.Rating2"

# Model 1: Elastic net regression
model_en <- cv.glmnet(x_train, y_train, alpha = 0.5, lambda.min.ratio = 1e-10, type.measure = "mse")
best_lambda <- model_en$lambda.min
plot(model_en, main = "Elastic Net Cross-Validation")
en_coef <- coef(model_en, s = best_lambda)
selected_vars <- rownames(en_coef)[which(en_coef != 0)]

# Evaluate on validation set
predictions_val <- predict(model_en, s = best_lambda, newx = x_val)
mse_en <- mean((y_val - predictions_val)^2)
print(paste("Validation MSE (Elastic net):", round(mse_en, 4)))

# Model 2: Random Forest
# select mtry
mtry_tuned <- tuneRF(x_train, y_train, stepFactor = 1.5, improve = 0.01, ntreeTry = 500)
mtry_tuned_value <- mtry_tuned[which.min(mtry_tuned[, 2]), 1]
model_rf <- randomForest(My.Rating ~ ., data = xy_train, 
                         ntree = 500, nodesize=1, mtry=mtry_tuned_value, importance = TRUE)
plot(model_rf, main="Random Forest out-of-bag Error")
varImpPlot(model_rf, main = "Variable Importance (Random Forest)")

# Evaluate on validation set
predictions_val_rf <- predict(model_rf, newdata = xy_val)
mse_rf <- mean((y_val - predictions_val_rf)^2)
print(paste("Validation MSE (Random Forest):", round(mse_rf, 4)))
