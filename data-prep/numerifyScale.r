numerifyScale <- function(overall, column.name, new.col.name, num.levels) {
	overall[[new.col.name]] = overall[[column.name]]
	
	overall[[new.col.name]] <- as.numeric(as.character(overall[[new.col.name]]))

	cat(paste(head(overall[[column.name]])), "became", head(overall[[new.col.name]]))    
	overall
}        
