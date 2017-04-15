require(R6)
require(quantmod)

# Db interface
YahooDb = R6Class("YahooDb",
   public = list(
      initialize = function(path="yahoo.sqlite") {
         private$path = path
      },

      get.symbol = function(symbol, force=F) {
         require(RSQLite)

         driver = SQLite()
         connection = dbConnect(driver, dbname=private$path)

         symbol = toupper(symbol)

         # Leading '^' (used for indexes, like ^DJI) is dropped when
         # quantmod constructs the variable name
         db.symbol = gsub('^\\^', '', symbol)

         if(!force) {
            # Try the database first
            rs = dbGetQuery(
                     connection,
                     paste(
                        " select date, open, high, low, close, volume, adjusted from bars ",
                        " where symbol = '", db.symbol, "'",
                        " order by date",
                        sep=""))
            if(NROW(rs) == 0) {
               rs = NULL
            } else {
               ss = xts(rs[,2:NCOL(rs)], as.Date(rs[,1]))
               colnames(ss) = private$col.names
            }
         } else {
            rs = NULL
         }

         if(NROW(rs) == 0) {
            ss = getSymbols(symbol, from="1900-01-01", auto.assign=F)
            ss = adjustOHLC(ss, use.Adjusted=F, adjust="split", symbol.name=symbol)
            colnames(ss) = private$col.names
            df = cbind(data.frame(symbol=db.symbol), as.character(index(ss)), data.frame(ss))
            colnames(df) = c("symbol","date",private$col.names)
            dbBegin(connection)
            query = paste("delete from bars where symbol='", symbol, "'", sep="")
            dbSendQuery(connection, query)
            query = paste(" insert or replace into bars (symbol,date,open,high,low,close,volume,adjusted) ",
                          "   values(@symbol,@date,@open,@high,@low,@close,@volume,@adjusted)",
                          sep="")
            RSQLite::dbGetPreparedQuery(connection, query, bind.data=df)

            if(db.symbol != symbol) {
               # If the database symbol (GSPC) is different than the yahoo symbol (^GSPC),
               # write the mapping.
               query = paste(" insert or ignore into map (symbol,yahoo_symbol) ",
                             "   values(@symbol,@yahoo_symbol)",
                             sep="")
               RSQLite::dbGetPreparedQuery(connection, query, bind.data=data.frame(symbol=db.symbol,yahoo_symbol=symbol))
            }

            dbCommit(connection)
         }


         dbDisconnect(connection)

         return(ss)
      },

      get.symbols = function(symbols, env, force=F) {
         require(RSQLite)

         driver = SQLite()
         connection = dbConnect(driver, dbname=private$path)

         symbols = toupper(symbols)

         # Leading '^' (used for indexes, like ^DJI) is dropped when
         # quantmod constructs the variable name
         db.names = gsub('^\\^', '', symbols)

         done = FALSE
         if(!force) {
            # Try the local cache first
            for(ss in db.names) {
               print(ss)
               rs = dbGetQuery(
                  connection,
                  paste(
                     " select date, open, high, low, close, volume, adjusted from bars ",
                     " where symbol = '",ss, "'",
                     " order by date",
                     sep=""))
               if(NROW(rs) == 0) {
                  rs = NULL
               } else {
                  env[[ss]] = xts(rs[,2:NCOL(rs)], as.Date(rs[,1]))
                  colnames(env[[ss]]) = private$col.names
               }
            }

            # Verify that we got all symbols, in which case we are done
            if(!any(is.na(match(db.names, ls(env))))) {
               done = TRUE
            }
         }

         if(!done) {
            # Either a symbol was not found in the cache, or the caller forced us to download and cache
            getSymbols(symbols, env=env, src="yahoo", from="1900-01-01", auto.assign=TRUE)
            dbBegin(connection)
            for(ss in ls(env)) {
               # Adjust only for splits
               env[[ss]] = adjustOHLC(env[[ss]], use.Adjusted=F, adjust="split", symbol.name=symbols[as.numeric(match(ss, db.names))])
               colnames(env[[ss]]) = private$col.names

               # Store into the database
               df = cbind(data.frame(symbol=ss), as.character(index(env[[ss]])), data.frame(env[[ss]]))
               colnames(df) = c("symbol","date",private$col.names)

               query = paste("delete from bars where symbol='", ss, "'", sep="")
               dbSendQuery(connection, query)
               query = paste(" insert or replace into bars (symbol,date,open,high,low,close,volume,adjusted) ",
                             "   values(@symbol,@date,@open,@high,@low,@close,@volume,@adjusted)",
                             sep="")
               RSQLite::dbGetPreparedQuery(connection, query, bind.data=df)
            }

            # Write all mappings
            for(ii in 1:NROW(symbols)) {
               if(db.names[ii] != symbols[ii]) {
                  # If the database symbol (GSPC) is different than the yahoo symbol (^GSPC),
                  # write the mapping.
                  query = paste(" insert or ignore into map (symbol,yahoo_symbol) ",
                                "   values(@symbol,@yahoo_symbol)",
                                sep="")
                  RSQLite::dbGetPreparedQuery(
                     connection,
                     query,
                     bind.data=data.frame(symbol=db.names[ii],yahoo_symbol=symbols[ii]))
               }
            }

            dbCommit(connection)
         }

         dbDisconnect(connection)
      },

      # Shortcuts
      g = function(symbol, force=F) {
         return(self$get.symbol(symbol, force))
      },


      gs = function(symbols, env, force=F) {
         return(self$get.symbols(symbols, env, force))
      },

      store.symbol = function(symbol, data) {
         require(RSQLite)

         driver = SQLite()
         connection = dbConnect(driver, dbname=private$path)

         df = cbind(data.frame(symbol=symbol), as.character(index(data)), data.frame(data))
         colnames(df) = c("symbol","date",private$col.names)
         dbBegin(connection)
         query = paste("delete from bars where symbol='", symbol, "'", sep="")
         dbSendQuery(connection, query)
         query = paste(" insert or replace into bars (symbol,date,open,high,low,close,volume,adjusted) ",
                       "   values(@symbol,@date,@open,@high,@low,@close,@volume,@adjusted)",
                       sep="")
         RSQLite::dbGetPreparedQuery(connection, query, bind.data=df)
         dbCommit(connection)
         dbDisconnect(connection)
      },

      init = function() {
         require(RSQLite)

         driver = SQLite()
         connection = dbConnect(driver, dbname=private$path)

         query = paste(" create table if not exists bars ( ",
                       " symbol varchar(30) not null, ",
                       " date datetime not null, ",
                       " open real not null, ",
                       " high real not null, ",
                       " low real not null, ",
                       " close real not null, ",
                       " volume real not null, ",
                       " adjusted real not null) ",
                       sep="")
         dbGetQuery(connection, query)
         query = paste(" create unique index if not exists bars_unique ",
                       "    on bars(symbol,date) ",
                       sep="")
         dbGetQuery(connection, query)

         query = paste(" create table if not exists map ( ",
                       " symbol varchar(30) not null, ",
                       " yahoo_symbol varchar(30) not null) ",
                       sep="")
         dbGetQuery(connection, query)
         query = paste(" create unique index if not exists map_unique ",
                       "    on map(symbol) ",
                       sep="")
         dbGetQuery(connection, query)

         dbDisconnect(connection)
      }
   ),

   private = list(
      path = "yahoo.sqlite",
      col.names = c("open","high","low","close","volume","adjusted")
   )
)