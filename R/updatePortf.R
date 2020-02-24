#' update Portfilio P&L over a Dates range
#'
#' The \code{updatePortf} function goes through each symbol and calculates the PL for each period prices are available.
#'
#' Note that the portfolio will be marked on every time stamp where prices are available.
#' As such, your \code{Dates} range must reflect timestamps which appear in the price stream.
#' Also note that you probably don't want to mark the portfolio on every tick,
#'
#'
#' @return assigns position information and PL into the environment
#'
#' @param Portfolio string identifying a portfolio
#' @param Symbols character vector identifying symbols to update the portfolio for, default NULL
#' @param Dates optional xts-style ISO-8601 time range to run updatePortf over, default NULL (will use times from Prices)
#' @param Prices optional xts object containing prices and timestamps to mark the book on, default NULL
#' @param Interval optional character string, containing one of "millisecond" (or "ms"), "microsecond" (or "us"),
#' "second", "minute", "hour", "day", "week", "month", "quarter", or "year".  This can optionally be preceded by
#' a positive integer, or followed by "s".
#' @param \dots any other passthrough parameters
#' @export
updatePortf <- function(Portfolio, colName, isLong, Symbols=NULL, Dates=NULL, Prices=NULL, Interval=Interval, ...)
{ #' @author Peter Carl, Brian Peterson
     pname<-Portfolio
     Portfolio<-.getPortfolio(pname) # TODO add Date handling
     if(length(grep('virtual',pname))>0) { is.virtual = TRUE # virtual portfolios will not be updated in .USD, and summary will be built from posPL
	 } else { is.virtual = FALSE };
     # FUNCTION
     if(is.null(Symbols)){
       Symbols = ls(Portfolio$symbols)
     }
     for(symbol in Symbols){
       # tmp_instr<-try(getInstrument(symbol), silent=TRUE) - WHY IS IT THERE ?
       .updatePosPL(Portfolio=pname, Symbol=as.character(symbol), Dates=Dates, Prices=Prices, Interval=Interval, virtual = is.virtual, prefer = colName, ...=...)
     }
     
     # Calculate and store portfolio summary table
     Portfolio<-.getPortfolio(pname) # refresh with an updated object
     if(is.null(Dates)) 
	 {
		Dates <- unique(do.call(c,c(lapply(Portfolio$symbols, function(x) index(x[["posPL"]])), use.names=FALSE, recursive=FALSE)))
     } else {
		if(class(Dates)=="character")
		{ # try ISO-parsing Dates, compare first of Dates to LAST of portfolio dates
			parsed.Dates <-.parseISO8601(Dates);
			if(!is.na(parsed.Dates$first.time))
			{
				available.Dates<-unique(do.call(c,c(lapply(Portfolio$symbols, function(x) index(x[["posPL"]])), use.names=FALSE, recursive=FALSE)));
				if(max(available.Dates)<parsed.Dates$first.time)
					return(pname); # we should not continue building summary for this portfolio. it will be null
			}
		}
	 }
     #Symbols = ls(Portfolio$symbols)
     Attributes = c('Net.Value',  'Period.Realized.PL', 'Period.Unrealized.PL',
                    'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL', 'Traded.Volume')
     summary = NULL
     tmp.attr=NULL
     for(attribute in Attributes) {
       result=NULL
       switch(attribute,
              Net.Value = {
                # all these use Pos.Value
                if(is.null(tmp.attr)){
                  table = .getBySymbol(Portfolio = Portfolio, Attribute = "Pos.Value", Dates = Dates, Symbols = Symbols, native = is.virtual)
                  tmp.attr="Pos.Value"
                }
                result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))	
              },
              Period.Realized.PL =,
              Period.Unrealized.PL =,
              Gross.Trading.PL =,
              Txn.Fees =,
              Net.Trading.PL = {
                table = .getBySymbol(Portfolio = Portfolio, Attribute = attribute, Dates = Dates, Symbols = Symbols, native = is.virtual)
                tmp.attr = NULL
                result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
              },
              Traded.Volume = {
                table = .getBySymbol(Portfolio = Portfolio, Attribute = 'Txn.Value', Dates = Dates, Symbols = Symbols, native = is.virtual)
                if(isLong) {
                  table[table < 0] <- 0 #keep only positive for long
                } else {
                  table[table > 0] <- 0 #keep only negative for short
                }
                result = xts(rowSums(table, na.rm=TRUE), order.by=index(table))
              }
       )
       
       colnames(result) = attribute
       if(is.null(summary)) {summary=result}
       else {summary=cbind(summary,result)}
     }
     
     # get rid of duplicated indices in the summary data,
     # thanks to Guy Yollin for the bug report and Josh Ulrich for the elegant approach to fixing it
     d <- duplicated(.index(summary)) | duplicated(.index(summary), fromLast=TRUE)
     if(any(d)){
       # extract duplicated rows; get last row for each duplicate
       summary.dups <- summary[d,]
       ds <- duplicated(.index(summary.dups)) & !duplicated(.index(summary.dups), fromLast=TRUE)
       # get the last value
       cLast <- c('Net.Value')
       lastCols <- summary.dups[which(ds),cLast]
       # sum values
       cSums <- c('Period.Realized.PL', 'Period.Unrealized.PL', 'Gross.Trading.PL', 'Txn.Fees', 'Net.Trading.PL', 'Traded.Volume')
       # take cumulative sum; keep last value for each duplicate
       sumCols <- cumsum(summary.dups[,cSums])[which(ds),cSums]
       # subtract previous value from current value, since we used cumsum
       sumColsLag <- lag(sumCols)
       sumColsLag[1,] <- 0
       sumCols <- sumCols - sumColsLag
       slist <- merge(sumCols,lastCols)      # combine aggregated objects
       slist <- slist[,colnames(summary)]    # order columns
       summary <- rbind(summary[!d,], slist) # put it all back together
     }
     
     # if(!is.timeBased(Dates)) Dates = xts:::time.xts(Portfolio$symbols[[1]][["posPL"]][Dates])
     #xts(,do.call(unlist,c(lapply(symbols,index),use.names=FALSE)))
     if(!is.timeBased(Dates)) Dates <- unique(do.call(c,c(lapply(Portfolio$symbols, function(x) index(x[["posPL"]][Dates]) ), use.names=FALSE, recursive=FALSE)))
     startDate = min(Dates)-.001
	 # logerror(paste('updating portfolio: ',pname,'\nDates = ',capture.output(Dates),'\nstartDate = ',startDate,'\ntrimmed Portfolio.summary:\n',
		#capture.output(Portfolio$summary[paste('::',format(as.POSIXct(startDate, origin = default.origin)),sep='')]),'\nnew summary:\n',capture.output(summary),'\n',sep=""))
     # trim summary slot to not double count, related to bug 831 on R-Forge, and rbind new summary
     
     #needed for swap
     attr(summary, 'is.virtual') <- is.virtual
     attr(summary, 'isLong') <- isLong
     
     if( as.POSIXct(attr(Portfolio,'initDate'))>=startDate || length(Portfolio$summary)==0 ){
       Portfolio$summary<-summary #changes to subset might not return a empty dimnames set of columns
     }else{
       Portfolio$summary<-rbind(window(Portfolio$summary, end = startDate),summary)
     }

     #portfolio is already an environment, it's been updated in place
     #assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
     
     return(pname) #not sure this is a good idea
}

###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/)
# Copyright (c) 2008-2015 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: updatePortf.R 1666 2015-01-07 13:26:09Z braverock $
#
###############################################################################
