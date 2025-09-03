con <- dbConnect(RSQLite::SQLite(), dbname = "notes.sqlite")

tables_in_db <- dbListTables(con)
print(tables_in_db)

dbReadTable(con, "notes")

dbDisconnect(con)
