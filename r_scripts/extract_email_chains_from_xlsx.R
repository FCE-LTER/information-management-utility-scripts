# ============================================================
# Extract text boxes from worksheet "Email Correspondences"
# ============================================================

library(xml2) 

# Create temporary extraction directory
tmp_dir <- tempfile()
dir.create(tmp_dir)

unzip("im_notes/my_file.xlsx", exdir = tmp_dir)

doc <- read_xml( file.path(tmp_dir, "xl", "drawings", "drawing2.xml") ) 

shapes <- xml_find_all( doc, ".//*[local-name()='sp']" ) 

email_chains <- list()

for (i in seq_along(shapes)) {
  
  txt <- xml_text(
    xml_find_all(
      shapes[[i]],
      ".//*[local-name()='t']"
    )
  )
  
  txt <- txt[nzchar(trimws(txt))]
  
  email_chains[[i]] <- paste(
    txt,
    collapse = "\n"
  )
  
}

length(email_chains)

cat(email_chains[[1]])

add_email_separators <- function(text) {
  
  # Insert separator before each new email header
  gsub(
    "\n(?=From:)",
    "\n\n------------------------------------------------------------\n\n",
    text,
    perl = TRUE
  )

}

md <- c(
  "# Email Correspondence Archive",
  ""
)


for (i in seq_along(email_chains)) {
  
  md <- c(
    md,
    paste0("## Email Chain ", i),
    "",
    "```text",
    email_chains[[i]],
    "```",
    ""
  )
  
}

writeLines(
  md,
  "Email_Correspondence_Archive.md"
)