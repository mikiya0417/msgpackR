# MessagePack for R
# Author: @mikiya0417

.pack_nil <- function() {
	return(as.raw(0xC0))
}

.pack_true <- function() {
	return(as.raw(0xC3))
}

.pack_false <- function() {
	return(as.raw(0xC2))
}

.pack_pfixnum <- function(value) {
	return(as.raw(0x7F) & as.raw(value))
}

.pack_nfixnum <- function(value) {
	return(xor(as.raw(0xFF), as.raw(abs(value)-1)))
}

.pack_uint8 <- function(value) {
	return(c(as.raw(0xCC), as.raw(value)))
}

.pack_uint16 <- function(value) {
	return(c(as.raw(0xCD), as.raw(value/2^8), as.raw(value%%2^8)))
}

.pack_uint32 <- function(value) {
	return(c(as.raw(0xCE), as.raw(value/2^24), as.raw((value%%2^24)/2^16), as.raw((value%%2^16)/2^8), as.raw(value%%2^8)))
}

.pack_uint64 <- function(value) {
	return(c(as.raw(0xCF), as.raw(value/2^56), as.raw((value%%2^56)/2^48), as.raw((value%%2^48)/2^40), as.raw((value%%2^40)/2^32), as.raw((value%%2^32)/2^24), as.raw((value%%2^24)/2^16), as.raw((value%%2^16)/2^8), as.raw(value%%2^8)))
}

.pack_int8 <- function(value) {
	if ( value < 0 ) {
		value <- 2^8 + value
	}
	return(c(as.raw(0xD0), as.raw(value)))
}

.pack_int16 <- function(value) {
	if ( value < 0 ) {
		value <- 2^16 + value
	}
	return(c(as.raw(0xD1), as.raw(value/2^8), as.raw(value%%2^8)))
}

.pack_int32 <- function(value) {
	if ( value < 0 ) {
		value <- 2^32 + value
	}
	return(c(as.raw(0xD2), as.raw(value/2^24), as.raw((value%%2^24)/2^16), as.raw((value%%2^16)/2^8), as.raw(value%%2^8)))
}

.pack_int64 <- function(value) {
	if ( value < 0 ) {
		value <- 2^64 + value
	}
	return(c(as.raw(0xD3), as.raw(value/2^56), as.raw((value%%2^56)/2^48), as.raw((value%%2^48)/2^40), as.raw((value%%2^40)/2^32), as.raw((value%%2^32)/2^24), as.raw((value%%2^24)/2^16), as.raw((value%%2^16)/2^8), as.raw(value%%2^8)))
}

.pack_float <- function(value) {
	# R supports not "float" but "double".
	return(pack_double(value))
}

.pack_double <- function(value) {
	# the function to transform binary number into decimal number
	bit2dec <- function(bits) {
		n <- length(bits)
		result <- 0
		
		for ( i in 1:n ) {
			if ( bits[i] == 1 ) {
				result <- result + 2^(n-i)
			}
		}
		return(result)
	}
	
	# the function to transform fraction into 52 element vector 
	bit_fraction <- function(n) {
		result <- c()
		for ( i in 1:52 ) {
			b <- 0
			if ( n >= 2^(-i) ) {
				n <- n - 2^(-i)
				b <- 1
			}
			result <- c(result, b)
		}
		return(result)
	}
	# the end of the definition of two functions only used in ".pack_double()"
	
	result <- c(as.raw(0xCB))
	sign <- value<0
	exp <- floor(log2(abs(value)))
	bits <- bit_fraction(abs(value)*(2^(-exp))-1)
	exp <- exp + 1023
	
	# from 1st bit to 8th bit
	result <- c(result, as.raw(sign*0x80 + exp%/%2^4))
	# from 9th bit to 16th bit
	result <- c(result, as.raw(exp%%2^4*0x10 + bit2dec(bits[1:4])))
	# from 17th bit to 52nd bit
	result <- c(result, as.raw(bit2dec(bits[5:12])), as.raw(bit2dec(bits[13:20])), as.raw(bit2dec(bits[21:28])), as.raw(bit2dec(bits[29:36])), as.raw(bit2dec(bits[37:44])), as.raw(bit2dec(bits[45:52])))
	
	return(result)
}

.pack_map <- function(data) {
	# the function to return the datum and it's name
	.pack_map2 <- function(datum) {
		name <- names(datum)
		for ( d in datum ) {
			value <- d
		}
		return(c(pack(name), pack(value)))
	}
	# the end of the definition of two functions only used in ".pack_map()"
	
	result <- c()
	keys <- names(data)
	values <- as.list(data)
	n <- length(data)
	
	if ( n <= 15 ) {
		result <- c(result, (as.raw(0x8F) & as.raw(0x80+n)))
		for ( i in 1:n ) {
			result <- c(result, .pack_map2(data[i]))
		}
	}
	else if ( n <= 2^16-1 ) {
		result <- c(result, as.raw(0xDE), as.raw(n/2^8), as.raw(n%%2^8))
		for ( i in 1:n ) {
			result <- c(result, .pack_map2(data[i]))
		}
	}
	else if ( n <= 2^32-1 ) {
		result <- c(result, as.raw(0xDF), as.raw(n/2^24), as.raw((n%%2^24)/2^16), as.raw((n%%2^16)/2^8), as.raw(n%%2^8))
		for ( i in 1:n ) {
			result <- c(result, .pack_map2(data[i]))
		}
	}
	# if the length is more than 2^32-1, 
	else {
		# not implemented
	}
	return(result)
}

.pack_array <- function(data) {
	result <- c()
	n <- length(data)
	
	if ( n <= 15 ) {
		#FixArray
		result <- c(result, (as.raw(0x9F) & as.raw(0x90+n)))
		for ( datum in data ) {
			result <- c(result, pack(datum))
		}
	}
	else if ( n <= 2^16-1 ) {
		#array16
		result <- c(result, as.raw(0xDC), as.raw(n/2^8), as.raw(n%%2^8))
		for ( datum in data ) {
			result <- c(result, pack(datum))
		}
	}
	else if ( n <= 2^32-1 ) {
		#array32
		result <- c(result, as.raw(0xDD), as.raw(n/2^24), as.raw((n%%2^24)/2^16), as.raw((n%%2^16)/2^8), as.raw(n%%2^8))
		for ( datum in data ) {
			result <- c(result, pack(datum))
		}
	}
	# if the length is more than 2^32-1, 
	else {
		# not implemented
	}
	
	return(result)
}

.pack_raw <- function(data) {
	n <- length(data)
	
	if ( n <= 31 ) {
		h <- (as.raw(0xA0) | as.raw(n))
	}
	else if ( n <= 2^16-1 ) {
		h <- as.raw(0xDA)
	}
	else if ( n <= 2^32-1 ) {
		h <- as.raw(0xDB)
	}
	
	result <- c(h, data)
	return(result)
}

# Usage:
# test <- pack(c(1,2,3))
# print(test)  # => 93 01 02 03
pack <- function(data) {
	result <- c()	
	
	if ( is.vector(data) && (length(data) > 1 || !is.null(names(data))) ) {
		# if the vector has no name, pack to "array".
		if ( is.null(names(data)) ) {
			result <- c(result, .pack_array(data))
		}
		# if the vector has some name, pack to "map".
		else {
			result <- c(result, .pack_map(data))
		}
	}
	else if ( is.matrix(data) || is.data.frame(data) ) {
		result <- c(result, (as.raw(0x9F) & as.raw(0x90 + nrow(data))))
		# if the matrix has no name, pack to "array".
		if ( is.null(colnames(data)) ) {
			for ( i in 1:nrow(data) ) {
				result <- c(result, .pack_array(data[i,]))
			}
		}
		# if the matrix has some name, pack to "map".
		else {
			for ( i in 1:nrow(data) ) {
				result <- c(result, .pack_map(data[i,]))
			}
		}
	}
	# array
	else if ( is.array(data) ) {
		# not implemented
	}
	# factor
	else if ( is.factor(data) ) {
		# transfer factor into character
		mat <- .msgpack_factorToChar(data)
		result <- pack(mat)
	}
	# order
	else if ( is.ordered(data) ) {
		# not implemented
	}
	# atomic data type
	else {		
		# NULL
		if ( is.null(data) ) {
			result <- c(result, .pack_nil())
		}
		# boolean
		else if ( mode(data) == mode(TRUE) && data == TRUE ) {
			result <- c(result, .pack_true())
		}
		else if ( mode(data) == mode(FALSE) && data == FALSE ) {
			result <- c(result, .pack_false())
		}
		# character
		else if ( is.character(data) ) {
			result <- c(result, .pack_raw(charToRaw(data)))
		}
		# integer
		else if ( floor(data) == data ) {
			# positive fixnum
			if ( data <= 127 && data >= 0 ) {
				result <- c(result, .pack_pfixnum(data))
			}
			# negative fixnum
			else if ( data <= -1 && data >= -32 ) {
				result <- c(result, .pack_nfixnum(data))
			}
			# int8
			else if ( data < -32 && data >= -2^7 ) {
				result <- c(result, .pack_int8(data))
			}
			# int16
			else if ( data < -2^7 && data >= -2^15 ) {
				result <- c(result, .pack_int16(data))
			}
			# int32
			else if ( data < -2^15 && data >= -2^31 ) {
				result <- c(result, .pack_int32(data))
			}
			# int64
			else if ( data < -2^31 && data >= -2^63 ) {
				result <- c(result, .pack_int64(data))
			}
			else if ( data < -2^63 ) {
				# not implemented
			}
			# uint8
			else if ( data < 2^8 ) {
				result <- c(result, .pack_uint8(data))
			}
			# uint16
			else if ( data < 2^16 ) {
				result <- c(result, .pack_uint16(data))
			}
			# uint32
			else if ( data < 2^32 ) {
				result <- c(result, .pack_uint32(data))
			}
			# uint64
			else if ( data < 2^64 ) {
				result <- c(result, .pack_uint64(data))
			}
			
			# the other cases...
			else {
				# not implemented
			}
		}
		# floating point number
		else if ( floor(data) != data ) {
			result <- .pack_double(data)
		}
	}
	
	return(result)
}

# Usage:
# msgpack.WriteResult("test.txt", pack(c(1,2,3)))
msgpack.writeResult <- function(filename, result) {
	fl <- file(filename, "wb")
	writeBin(result, fl)
	close(fl)
}


.unpack_pfixnum <- function() {
	num <- .msgpack_data[.msgpack_index]
	.msgpack_index <<- .msgpack_index + 1
	return(as.integer(num & as.raw(0x7F)))
}

.unpack_fixmap <- function() {
	result <- list()
	N <- as.integer(.msgpack_data[.msgpack_index] & as.raw(0x0F))
	nms <- character(N)
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in 1:N ) {
		nms[i] <- .unpack_data()
		result[[i]] <- .unpack_data()
	}
	result <- .unpack_checkclass(result)
	names(result) <- nms
	return(result)
}

.unpack_fixarray <- function() {
	result <- list()
	N <- as.integer(.msgpack_data[.msgpack_index] & as.raw(0x0F))
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in 1:N ) {
		result[[i]] <- .unpack_data()
	}
	result <- .unpack_checkclass(result)
	
	return(result)
}

.unpack_fixraw <- function() {
	result <- ""
	result_byte <- list()
	N <- as.integer(.msgpack_data[.msgpack_index] & as.raw(0x1F))
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in 1:N ) {
		result_byte[[i]] <- rawToChar(.msgpack_data[.msgpack_index])
		.msgpack_index <<- .msgpack_index + 1
	}

	result_byte <- .unpack_checkclass(result_byte)
	
	for ( i in 1:length(result_byte) ) {
		result <- paste(result, result_byte[i], sep="")
	}

	return(result)
}

.unpack_nil <- function() {
	.msgpack_index <<- .msgpack_index + 1
	return(NULL)
}

.unpack_false <- function() {
	.msgpack_index <<- .msgpack_index + 1
	return(FALSE)
}

.unpack_true <- function() {
	.msgpack_index <<- .msgpack_index + 1
	return(TRUE)
}

.unpack_float <- function() {
	# R supports not "float" but "double".
	return(.unpack_double())
}

.unpack_double <- function() {
	result <- 0
	.msgpack_index <<- .msgpack_index + 1
	
	bits <- c()
	for ( i in 1:8 ) {
		bits[i] <- .msgpack_data[.msgpack_index]
		.msgpack_index <<- .msgpack_index + 1
	}
	
	# sign
	sign <- ifelse((bits[1] & as.raw(0x80)) != as.raw(0x00), -1, 1)
	# exponent
	exp <- as.integer(bits[1] & as.raw(0x7F))*2^4 + as.integer(bits[2] & as.raw(0xF0))/2^4 - 1023
	# fraction
	frac <- (rev(c(rawToBits(bits[2] & as.raw(0x0F)))))[5:8]
	for ( i in 3:8 ) {
		frac <- c(frac, as.integer(rev(rawToBits(bits[i]))))
	}
	
	for ( i in 1:52 ) {
		result <- result + frac[i]/2^i
	}
	result <- sign*(result+1)*2^exp
	
	return(result)
}

.unpack_uint8 <- function() {
	result <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	result <- result + as.integer(.msgpack_data[.msgpack_index])
	
	.msgpack_index <<- .msgpack_index + 1
	return(result)
}

.unpack_uint16 <- function() {
	result <- 0
	
	for ( i in seq(8,0,-8) ) {
		.msgpack_index <<- .msgpack_index + 1
		result <- result + as.integer(.msgpack_data[.msgpack_index])*2^i
	}
	
	.msgpack_index <<- .msgpack_index + 1
	return(result)
}

.unpack_uint32 <- function() {
	result <- 0
	
	for ( i in seq(24,0,-8) ) {
		.msgpack_index <<- .msgpack_index + 1
		result <- result + as.integer(.msgpack_data[.msgpack_index])*2^i
	}
	
	.msgpack_index <<- .msgpack_index + 1
	return(result)
}

.unpack_uint64 <- function() {
	result <- 0
	
	for ( i in seq(56,0,-8) ) {
		.msgpack_index <<- .msgpack_index + 1
		result <- result + as.integer(.msgpack_data[.msgpack_index])*2^i
	}
	
	.msgpack_index <<- .msgpack_index + 1
	return(result)
}

.unpack_int8 <- function() {
	result <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	sign <- ifelse((.msgpack_data[.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
	
	result <- result + as.integer(.msgpack_data[.msgpack_index])
	.msgpack_index <<- .msgpack_index + 1
	
	if ( sign < 0 ) {
		result <- -2^8 + result
	}
	
	return(result)
}

.unpack_int16 <- function() {
	result <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	sign <- ifelse((.msgpack_data[.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
	
	for ( i in seq(8,0,-8) ) {
		result <- result + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	if ( sign < 0 ) {
		result <- -2^16 + result
	}
	
	return(result)
}

.unpack_int32 <- function() {
	result <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	sign <- ifelse((.msgpack_data[.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
	
	for ( i in seq(24,0,-8) ) {
		result <- result + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	if ( sign < 0 ) {
		result <- -2^32 + result
	}
	
	return(result)
}

.unpack_int64 <- function() {
	result <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	sign <- ifelse((.msgpack_data[.msgpack_index] & as.raw(0x80)) != as.raw(0x00), -1, 1)
	
	for ( i in seq(56,0,-8) ) {
		result <- result + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	if ( sign < 0 ) {
		result <- -2^64 + result
	}
	
	return(result)
}

.unpack_raw16 <- function() {
	result <- list()
	N <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in seq(8,0,-8) ) {
		N <- N + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	for ( i in 1:N ) {
		result[[i]] <- rawToChar(.msgpack_data[.msgpack_index])
		.msgpack_index <<- .msgpack_index + 1
	}
	result <- .unpack_checkclass(result)
	
	return(result)
}

.unpack_raw32 <- function() {
	result <- list()
	N <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in seq(24,0,-8) ) {
		N <- N + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	for ( i in 1:N ) {
		result[[i]] <- rawToChar(.msgpack_data[.msgpack_index])
		.msgpack_index <<- .msgpack_index + 1
	}
	result <- .unpack_checkclass(result)
	
	return(result)
}

.unpack_array16 <- function() {
	result <- list()
	N <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in seq(8,0,-8) ) {
		N <- N + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	for ( i in 1:N ) {
		result[[i]] <- .unpack_data()
	}
	result <- .unpack_checkclass(result)
	
	return(result)
}

.unpack_array32 <- function() {
	result <- list()
	N <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in seq(24,0,-8) ) {
		N <- N + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	for ( i in 1:N ) {
		result[[i]] <- .unpack_data()

	}
	result <- .unpack_checkclass(result)
	
	return(result)
}

.unpack_map16 <- function() {
	result <- list()
	N <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in seq(8,0,-8) ) {
		N <- N + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	nms <- character(N)
	
	for ( i in 1:N ) {
		nms[i] <- .unpack_data()
		result[[i]] <- .unpack_data()
	}
	result <- .unpack_checkclass(result)
	names(result) <- nms
	
	return(result)
}

.unpack_map32 <- function() {
	result <- list()
	N <- 0
	
	.msgpack_index <<- .msgpack_index + 1
	
	for ( i in seq(24,0,-8) ) {
		N <- N + as.integer(.msgpack_data[.msgpack_index])*2^i
		.msgpack_index <<- .msgpack_index + 1
	}
	
	nms <- character(N)
	
	for ( i in 1:N ) {
		nms[i] <- .unpack_data()
		result[[i]] <- .unpack_data()
	}
	result <- .unpack_checkclass(result)
	names(result) <- nms
	
	return(result)
}

.unpack_nfixnum <- function() {
	num <- .msgpack_data[.msgpack_index]
	.msgpack_index <<- .msgpack_index + 1
	return(-32 + as.integer(num & as.raw(0x1F)))
}

.unpack_checkclass <- function(data) {
	classes <- unique(unlist(lapply(data, class)))

	for ( i in 1:length(classes) ) {
		if ( classes[i] == "integer" ) {
			classes[i] = "numeric"
		}
	}
	classes <- unique(classes)
	
	len <- unique(unlist(lapply(data, length)))
	if( length(classes) == 1 && length(len) == 1 && len[1] == 1) {
		class(data) <- classes
	}
	
	return(data)
}


.msgpack_index <- 1
.msgpack_data <- list()
.unpack_data <- function() {
	if ( is.null(.msgpack_data) ) {
		return(NULL)
	}
	# Positive FixNum
	else if( .msgpack_data[.msgpack_index] <= as.raw(0x7F) ) {
		return(.unpack_pfixnum())
	}
	# FixMap
	else if ( .msgpack_data[.msgpack_index] <= as.raw(0x8F) ) {
		return(.unpack_fixmap())
	}
	# FixArray
	else if ( .msgpack_data[.msgpack_index] <= as.raw(0x9F) ) {
		return(.unpack_fixarray())
	}
	# FixRaw
	else if ( .msgpack_data[.msgpack_index] <= as.raw(0xBF) ) {
		return(.unpack_fixraw())
	}
	# nil
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xC0) ) {
		return(.unpack_nil())
	}
	# (reserved)
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xC1) ) {
		return(NULL)
	}
	# false
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xC2) ) {
		return(.unpack_false())
	}
	# true
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xC3) ) {
		return(.unpack_true())
	}
	# (reserved)
	else if ( .msgpack_data[.msgpack_index] <= as.raw(0xC9) ) {
		return(NULL)
	}
	# float
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xCA) ) {
		return(.unpack_float())
	}
	# double
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xCB) ) {
		return(.unpack_double())
	}
	# uint8
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xCC) ) {
		return(.unpack_uint8())
	}
	# uint16
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xCD) ) {
		return(.unpack_uint16())
	}
	# uint32
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xCE) ) {
		return(.unpack_uint32())
	}
	# uint64
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xCF) ) {
		return(.unpack_uint64())
	}
	# int8
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xD0) ) {
		return(.unpack_int8())
	}
	# int16
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xD1) ) {
		return(.unpack_int16())
	}
	# int32
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xD2) ) {
		return(.unpack_int32())
	}
	# int64
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xD3) ) {
		return(.unpack_int64())
	}
	# (reserved)
	else if ( .msgpack_data[.msgpack_index] <= as.raw(0xD9) ) {
		return(NULL)
	}
	# raw16
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xDA) ) {
		return(.unpack_raw16())
	}
	# raw32
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xDB) ) {
		return(.unpack_raw32())
	}
	# array16
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xDC) ) {
		return(.unpack_array16())
	}
	# array32
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xDD) ) {
		return(.unpack_array32())
	}
	# map16
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xDE) ) {
		return(.unpack_map16())
	}
	# map32
	else if ( .msgpack_data[.msgpack_index] == as.raw(0xDF) ) {
		return(.unpack_map32())
	}
	# Negative FixNum
	else {
		return(.unpack_nfixnum())
	}

}

# Usage:
# test <- unpack("./test.bin") # test.bin => 93 01 02 03
# print(test)
# [1] 1 2 3
unpack <- function(filename) {
	fl <- file(filename, "rb")
	bits <- readBin(fl, raw(), file.info(filename)$size)
	close(fl)
	
	.msgpack_index <<- 1
	.msgpack_data <<- bits
	return(.unpack_data())
}

# Usage:
# test <- unpack_bin(pack(c(1,2,3)))
# print(test)
# [1] 1 2 3
unpack_bin <- function(bin) {
	.msgpack_index <<- 1
	.msgpack_data <<- bin
	
	return(.unpack_data())
}

# under construction...
msgpack_matrix <- function(data) {
	n <- 1
	n_row <- length(data)
	n_col <- length(data[[1]])
	nms <- names(data[[1]])
	result <- list()
	for ( row in data ) {
		for ( col in row ) {
			result[n] <- col
			n <- n+1
		}
	}

	if ( n_col == 1 && n_row == 1 ) {
		result <- result[[1]]
		if ( !is.null(nms) ) {
			names(result) <- nms
		}
	}
	else if ( n_row > 1 ) {
		result <- matrix(result, n_row, n_col, byrow=TRUE)
		if ( !is.null(nms) ) {
			colnames(result) <- nms
		}
	}
	
	return(result)
}

.msgpack_factorToChar <- function(vec) {
	if ( is.factor(vec) ) {
		result <- as.character(vec)
	}
	return(result)
}


