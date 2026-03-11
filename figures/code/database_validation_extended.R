library(dplyr)
library(data.table)
library(DT)
library(tidyr)
library(stringr)
library(tibble)
library(chRoma)


#Data cleaning function
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

# ============================================================================
# VALIDATION FUNCTION FOR ALIAS SHEETS
# ============================================================================
validate_alias_sheet <- function(alias_df, name) {
  cat("\n========================================\n")
  cat("Validating:", name, "\n")
  cat("========================================\n")
  
  # Check for duplicate aliases
  duplicate_alias <- alias_df[duplicated(alias_df$Alias), ]
  if (nrow(duplicate_alias) > 0) {
    cat("WARNING: Found duplicates in Alias column:\n")
    print(duplicate_alias)
  } else {
    cat("✓ No duplicate aliases\n")
  }
  
  # Determine the prime column name (Material or Item)
  if ("Material" %in% colnames(alias_df)) {
    prime_col <- "Material"
  } else if ("Item" %in% colnames(alias_df)) {
    prime_col <- "Item"
  } else if ("Use" %in% colnames(alias_df)) {
    prime_col <- "Use"
  } else {
    cat("ERROR: Could not determine prime column name\n")
    return(NULL)
  }
  
  # Check that all prime terms are also aliases
  prime_terms <- data.frame(prime = unique(alias_df[[prime_col]])) %>%
    add_column("invalid" = NA)
  prime_terms_clean <- mutate_all(prime_terms, cleantext)
  alias_terms <- data.frame(alias = unique(alias_df$Alias))
  alias_terms_clean <- mutate_all(alias_terms, cleantext)
  
  for (x in 1:nrow(prime_terms_clean)) {
    if (!any(alias_terms_clean$alias == prime_terms_clean$prime[[x]], na.rm = TRUE)) {
      prime_terms_clean$invalid[[x]] <- TRUE
    }
  }
  
  invalid_primes <- prime_terms_clean %>% drop_na(invalid)
  if (nrow(invalid_primes) > 0) {
    cat("WARNING: Found prime terms that are NOT aliases:\n")
    print(invalid_primes)
  } else {
    cat("✓ All prime terms are also aliases\n")
  }
  
  return(invisible(alias_df))
}

# ============================================================================
# VALIDATION FUNCTION FOR CHECKING ALIASES IN VECTORDB
# ============================================================================
validate_aliases_in_vectorDB <- function(alias_df, vectorDB, name) {
  cat("\n----------------------------------------\n")
  cat("Checking aliases in vectorDB for:", name, "\n")
  cat("----------------------------------------\n")
  
  # Get unique aliases from the alias dataframe
  alias_list <- unique(alias_df$Alias)
  alias_list_clean <- cleantext(alias_list)
  
  # Get unique terms from vectorDB metadata
  if (!is.null(vectorDB$metadata) && "text" %in% colnames(vectorDB$metadata)) {
    vectordb_terms <- vectorDB$metadata$text
    vectordb_terms_clean <- cleantext(vectordb_terms)
    vectordb_unique <- unique(vectordb_terms_clean)
    
    # Find aliases NOT in vectorDB
    missing_in_db <- setdiff(alias_list_clean, vectordb_unique)
    missing_in_db <- missing_in_db[!is.na(missing_in_db)]
    
    if (length(missing_in_db) > 0) {
      cat("⚠ WARNING: ", length(missing_in_db), " alias(es) NOT found in vectorDB:\n")
      print(head(missing_in_db, 20))
      if (length(missing_in_db) > 20) {
        cat("... and", length(missing_in_db) - 20, "more\n")
      }
    } else {
      cat("✓ All aliases found in vectorDB\n")
    }
    
    # Report counts
    cat("  - Total unique aliases:", length(unique(alias_list_clean)), "\n")
    cat("  - Total unique terms in vectorDB:", length(vectordb_unique), "\n")
  } else {
    cat("ERROR: Could not access vectorDB metadata\n")
  }
}

# ============================================================================
# LOAD DATA FOR TRASH VERSIONS
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                 TRASH VERSIONS VALIDATION                      ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

material_alias_trash <- read.csv("data/PrimeMaterials_trash.csv")
material_hierarchy_trash <- read.csv("data/MaterialsHierarchy_trash.csv")
item_alias_trash <- read.csv("data/PrimeItems_trash.csv")
item_hierarchy_trash <- read.csv("data/ITEMSHierarchy_trash.csv")

# Load vectorDBs (from data folder, not figures/data)
items_vectorDB <- readRDS("data/items_vectorDB.rda")
materials_vectorDB <- readRDS("data/materials_vectorDB.rda")

# Validate trash alias sheets
validate_alias_sheet(material_alias_trash, "PrimeMaterials_trash.csv")
validate_alias_sheet(item_alias_trash, "PrimeItems_trash.csv")

# Check trash aliases in vectorDBs
validate_aliases_in_vectorDB(material_alias_trash, materials_vectorDB, "Materials (Trash)")
validate_aliases_in_vectorDB(item_alias_trash, items_vectorDB, "Items (Trash)")

# ============================================================================
# LOAD DATA FOR MICROPLASTICS VERSIONS
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║              MICROPLASTICS VERSIONS VALIDATION                 ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

material_alias_microplastic <- read.csv("data/PrimeMaterials_microplastic.csv")
material_hierarchy_microplastic <- read.csv("data/MaterialsHierarchy_microplastic.csv")
item_alias_microplastic <- read.csv("data/PrimeItems_microplastic.csv")
item_hierarchy_microplastic <- read.csv("data/ITEMSHierarchy_microplastic.csv")

# Validate microplastics alias sheets
validate_alias_sheet(material_alias_microplastic, "PrimeMaterials_microplastic.csv")
validate_alias_sheet(item_alias_microplastic, "PrimeItems_microplastic.csv")

# Check microplastics aliases in vectorDBs
validate_aliases_in_vectorDB(material_alias_microplastic, materials_vectorDB, "Materials (Microplastics)")
validate_aliases_in_vectorDB(item_alias_microplastic, items_vectorDB, "Items (Microplastics)")

# ============================================================================
# HIERARCHY VALIDATION - CHECK ALL TERMS ARE IN ALIAS SHEETS
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                    HIERARCHY VALIDATION                        ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

cat("\n--- TRASH MATERIALS HIERARCHY ---\n")
material_hierarchy_terms_trash <- data.frame(term = unique(unlist(material_hierarchy_trash)))
material_hierarchy_terms_trash <- mutate_all(material_hierarchy_terms_trash, cleantext) %>% drop_na()
prime_material_terms_trash <- data.frame(term = unique(material_alias_trash$Material))
prime_material_terms_trash <- mutate_all(prime_material_terms_trash, cleantext)

material_hierarchy_only_trash <- setdiff(material_hierarchy_terms_trash, prime_material_terms_trash)
if (nrow(material_hierarchy_only_trash) > 0) {
  cat("⚠ WARNING: Terms in hierarchy but NOT in Material alias list:\n")
  print(material_hierarchy_only_trash)
} else {
  cat("✓ All hierarchy terms are in Material alias list\n")
}

material_prime_only_trash <- setdiff(prime_material_terms_trash, material_hierarchy_terms_trash)
if (nrow(material_prime_only_trash) > 0) {
  cat("⚠ WARNING: Prime material terms NOT in hierarchy:\n")
  print(material_prime_only_trash)
} else {
  cat("✓ All prime materials are in hierarchy\n")
}

cat("\n--- TRASH ITEMS HIERARCHY ---\n")
item_hierarchy_terms_trash <- data.frame(term = unique(unlist(item_hierarchy_trash)))
item_hierarchy_terms_trash <- mutate_all(item_hierarchy_terms_trash, cleantext) %>% drop_na()
prime_item_terms_trash <- data.frame(term = unique(item_alias_trash$Item))
prime_item_terms_trash <- mutate_all(prime_item_terms_trash, cleantext)

item_hierarchy_only_trash <- setdiff(item_hierarchy_terms_trash, prime_item_terms_trash)
if (nrow(item_hierarchy_only_trash) > 0) {
  cat("⚠ WARNING: Terms in hierarchy but NOT in Item alias list:\n")
  print(item_hierarchy_only_trash)
} else {
  cat("✓ All hierarchy terms are in Item alias list\n")
}

item_prime_only_trash <- setdiff(prime_item_terms_trash, item_hierarchy_terms_trash)
if (nrow(item_prime_only_trash) > 0) {
  cat("⚠ WARNING: Prime item terms NOT in hierarchy:\n")
  print(item_prime_only_trash)
} else {
  cat("✓ All prime items are in hierarchy\n")
}

cat("\n--- MICROPLASTICS MATERIALS HIERARCHY ---\n")
material_hierarchy_terms_micro <- data.frame(term = unique(unlist(material_hierarchy_microplastic)))
material_hierarchy_terms_micro <- mutate_all(material_hierarchy_terms_micro, cleantext) %>% drop_na()
prime_material_terms_micro <- data.frame(term = unique(material_alias_microplastic$Material))
prime_material_terms_micro <- mutate_all(prime_material_terms_micro, cleantext)

material_hierarchy_only_micro <- setdiff(material_hierarchy_terms_micro, prime_material_terms_micro)
if (nrow(material_hierarchy_only_micro) > 0) {
  cat("⚠ WARNING: Terms in hierarchy but NOT in Material alias list:\n")
  print(material_hierarchy_only_micro)
} else {
  cat("✓ All hierarchy terms are in Material alias list\n")
}

material_prime_only_micro <- setdiff(prime_material_terms_micro, material_hierarchy_terms_micro)
if (nrow(material_prime_only_micro) > 0) {
  cat("⚠ WARNING: Prime material terms NOT in hierarchy:\n")
  print(material_prime_only_micro)
} else {
  cat("✓ All prime materials are in hierarchy\n")
}

cat("\n--- MICROPLASTICS ITEMS HIERARCHY ---\n")
item_hierarchy_terms_micro <- data.frame(term = unique(unlist(item_hierarchy_microplastic)))
item_hierarchy_terms_micro <- mutate_all(item_hierarchy_terms_micro, cleantext) %>% drop_na()
prime_item_terms_micro <- data.frame(term = unique(item_alias_microplastic$Item))
prime_item_terms_micro <- mutate_all(prime_item_terms_micro, cleantext)

item_hierarchy_only_micro <- setdiff(item_hierarchy_terms_micro, prime_item_terms_micro)
if (nrow(item_hierarchy_only_micro) > 0) {
  cat("⚠ WARNING: Terms in hierarchy but NOT in Item alias list:\n")
  print(item_hierarchy_only_micro)
} else {
  cat("✓ All hierarchy terms are in Item alias list\n")
}

item_prime_only_micro <- setdiff(prime_item_terms_micro, item_hierarchy_terms_micro)
if (nrow(item_prime_only_micro) > 0) {
  cat("⚠ WARNING: Prime item terms NOT in hierarchy:\n")
  print(item_prime_only_micro)
} else {
  cat("✓ All prime items are in hierarchy\n")
}

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                   VALIDATION COMPLETE                          ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

cat("\nSummary of Alias Counts:\n")
cat("- PrimeMaterials_trash.csv:        ", nrow(material_alias_trash), "rows\n")
cat("- PrimeItems_trash.csv:            ", nrow(item_alias_trash), "rows\n")
cat("- PrimeMaterials_microplastic.csv: ", nrow(material_alias_microplastic), "rows\n")
cat("- PrimeItems_microplastic.csv:     ", nrow(item_alias_microplastic), "rows\n")

cat("\nVectorDB Metadata Counts:\n")
cat("- materials_vectorDB unique terms: ", length(unique(cleantext(materials_vectorDB$metadata$text))), "\n")
cat("- items_vectorDB unique terms:     ", length(unique(cleantext(items_vectorDB$metadata$text))), "\n")
