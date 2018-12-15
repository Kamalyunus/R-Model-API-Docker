
options(warn=2) # cause the build to fail if any packages fail to install

install.packages(
    c("dplyr",
    "caret",
    "gbm",
    "textstem",
    "tokenizers",
    "plumber",
    "jsonlite",
    "text2vec")
)