#!/usr/bin/env Rscript

tools::package_native_routine_registration_skeleton(".",
                                                    "src/registration.c",
                                                    character_only = FALSE)
readLines("src/registration.c")

