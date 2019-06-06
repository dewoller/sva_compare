#!/usr/bin/env Rscript


detach_all_packages <- function() {
  basic.packages.blank <-  c("stats", 
                             "graphics", 
                             "grDevices", 
                             "utils", 
                             "datasets", 
                             "methods", 
                             "base",
                             "nvimcom",
                             "colorout")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")

  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, 
                                  TRUE, 
                                  FALSE)]

  package.list <- setdiff(package.list, basic.packages)

  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

detach_all_packages()

library(needs)
#detach_all_packages()

needs( 'workflowr')
needs("magrittr")
needs("stringr")
needs("knitr")
#needs("kableExtra")
#needs("pander")
needs("lubridate")

needs("wrapr" )   # for the qc function

#needs("ordinal" )
needs("tidyverse")
needs("tictoc")



# one of these need the newest version of libgdal

# sudo add-apt-repository -y ppa:ubuntugis/ppa

# agi libudunits2-dev libgdal-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev


#################################################################################
# usage - add '-sadgg' to the end of ggplot string to store the plot in a directory
# sadgg
#################################################################################

`-.gg` <- function(e1, e2) e2(e1)
sadgg <- function ( plot ) {

  library(janitor)

  base_dir='~/ggplots/'
  filetype='svg'
  if( !dir.exists( base_dir ) ) {
    dir.create( base_dir )
  }

  list.files( base_dir ) %>%
    enframe(name=NULL) %>%
    separate(value, into='n', sep="_", extra='drop') %>% 
    mutate( n=as.numeric(n) ) %>%
    summarise( maxn=max(n)) %$% maxn %>% 
    { . } -> maxn

  if (is.na( maxn) | maxn==-Inf) { maxn=0}    


  filename = paste0( sprintf( "%03d_", maxn+1), 
                    paste( plot$mapping, collapse='_'),
                    '.', filetype)

  filename = paste0( sprintf( "%03d_", maxn+1), make_clean_names( plot$labels$title) , '.', filetype )

  ggsave( filename, plot, device=filetype, path=base_dir)
  print(plot)
  #svgPanZoom( plot )

  #    if( system2('pidof', args='geeqie', stdout='/dev/null') ) {
#  system2( 'geeqie', args=c( '-r', '-t',  paste0( 'file:', base_dir, filename)), wait=FALSE)
  #system2( 'firefox', args=c( paste0(  base_dir, filename)), wait=FALSE)
  #system2( 'display', args=c( paste0(  base_dir, filename)), wait=FALSE)
  #    }
}

#################################################################################
is_theme_complete = function (x)  {TRUE}  # fix up bug in current tricolore library

# my.year.length  ------------------------------------------------------------------
#
my_year_length <- function( year ) {
  sapply( year, function(year) { year.length(as.character(year )) })
}

#################################################################################
really_is_numeric <- function(x) inherits(x, c("numeric","integer"))

#################################################################################
keep <- function(x, name) {assign(as.character(substitute(name)), x, pos = 1)}
#################################################################################
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))

#################################################################################
destring <- function(x,keep="0-9.-") {
  return( as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)) )
}

#################################################################################
save_data_single <- function( item ) {
	fwrite( eval(parse(text = item)), paste0(item, ".csv"))
}

#################################################################################
save_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, save_data_single )
	} else {
		save_data_single( item ) 
	}
}
#################################################################################
read_data_single <- function( item ) {
	 do.call("<<-",list(item, fread( paste0(item, ".csv")) %>% as.tibble()))
}
#################################################################################
read_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, read_data_single )
	} else {
		read_data_single( item ) 
	}
}
#################################################################################
# my.year.length  ------------------------------------------------------------------
#
my_year_length <- function( year ) {
	sapply( year, function(year) { year.length(as.character(year )) })
}


# seqNext ------------------------------------------------------------------
seqNext <- function(x1, y1) {
  dat <- data.frame( x= x1
  			 , y=y1) 
  unname(
    predict(lm(y ~ x, data=dat), newdata=list(x=c(2016)))
  )
}

# bothDiff  ------------------------------------------------------------------
both_diff <- function ( set1, set2, by=intersect(  names( set1 ), names( set2))) {

  print("first - second")
  set1 %>%
    anti_join( set2, by=by ) %>%
    arrange_( by ) %>%
    print()

  print("second - first")
  set2 %>%
    anti_join( set1, by=by ) %>%
    arrange_( by ) %>%
    print()

}
# ------------------------------------------------------------------
slurpTable <- function( name ) {

	state_geo_query = paste( "
			SELECT * FROM ", name
			, sep=""
			)
	dbGetQuery(con, state_geo_query ) %>% 
		as.tibble() %>%
	return( . )
}

# -------------------------------------------------
my_dbWriteTable <- function ( df, table_name ) {
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "mofi",
                   host = "localhost", port = 5432,
                   user = "dewoller", password = Sys.getenv('PASSWD'))
  on.exit(dbDisconnect(con))
  dbWriteTable( con, table_name, df )
}
# -------------------------------------------------

# -------------------------------------------------
my_db_get_query <- function ( query, db_name = 'mofi' ) {

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = db_name,
                   host = "localhost", port = 5432,
                   user = "dewoller", password = Sys.getenv("PASSWD") )
  on.exit(dbDisconnect(con))
  dbGetQuery( con, query )

}
#

#################################################################################
singleFacetPlot_boxplot = function( df_raw_input, var1, var2 ) {

		gb = c("lga", var1, var2 )
		df_raw_input %>%
			group_by_at( vars(gb)) %>%
			summarise( no_users = n()) %>%
			ggplot() +
			geom_boxplot( mapping=aes_string(x = var1, y= "no_users" , color=var1, fill=var1)) + 
			ggtitle( paste("The range of LGA number of users for each", var1, "facetted by", var2)) +
			facet_wrap( as.formula(paste("~", var2)), ncol=2, scales='fixed')
}
#-------------------------------------------------------------------------------------------

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
clipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}

#-------------------------------------------------------------------------------------------
ws <- function( ... ) {
  wideScreen( ... )
}

#-------------------------------------------------------------------------------------------
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}

#-------------------------------------------------------------------------------------------
sourcep <- function(file){
  coms <- parse(file)
  for (i in seq_along(coms)){
    print(coms[[i]])
    eval(coms[[i]],envir=.GlobalEnv)
    mess <- paste("Expression",i,"of",length(coms),"parsed. Press <return> to continue.")
    cat(mess)
    readLines(n=1)
  }
}

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

rows = function(x) lapply(seq_len(nrow(x)), function(i) lapply(x,"[",i))



#-----------------------------------------------------------------------------
# send formattded to clipboard

xc = function (df ) {
  df %>% 
    mutate_if( really_is_numeric, round, 2 ) %>%
    tableHTML::tableHTML() %>% clipr::write_clip()
} 


#-----------------------------------------------------------------------------
# multi assignment functions

# Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin

  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}


# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}


################################################################################
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
                                       fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
                             format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
                      as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()


################################################################################



# numeric variables

#generate_outlier( 1:10, 4 )


generate_outlier = function(field, n_outliers) {
  rng = range( field )
  diff = rng[2] - rng[1]
  field[ sample.int(  length( field ), n_outliers) ] =
    rng[2] + runif(n_outliers,  diff * 2, diff * 5 )
  field
}

################################################################################

#
#random_alter_string( 'sadfsdf' )
#random_alter_string( 'sadfsdf', str_range="AZ" )

random_alter_string = function( s, str_range = NA ) {
  slen = str_length(s)
  pos = sample.int( slen, 1 )
  chars = str_split(s, '')[[1]] 

  if ( is.na( str_range )) {
    char_to_replace = chars %>% gtools::asc() %>% range( )
  } else {
    char_to_replace = str_split(str_range, '')[[1]] %>% gtools::asc() %>% range( )
  }
  chars[ pos ] = runif( 1, char_to_replace[1], char_to_replace[2] + 1 ) %>% floor() %>%  gtools::chr()
  paste0( chars, collapse='')
}


################################################################################

#
#random_alter_date( as.Date( '1999-10-10' )  )
#random_alter_date( as.Date( '1999-10-10' ), date_range=365*10  )
#random_alter_date( rep( as.Date( '1999-10-10' ), 10 ), date_range=365*10  )

random_alter_date = function( in_date, date_range = 365 ) {
  offset = sample.int( date_range*2, length( in_date), replace=T ) - date_range

  runif( 1, 1,100)  # to keep standard
  in_date + as.difftime( offset, units='days')
}

################################################################################

