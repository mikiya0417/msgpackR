msgpackR
========

MessagePack for R  
(*1 MessagePack: http://msgpack.org/)

## Example
```
> (data <- c(1,2,3))
[1] 1 2 3
> (d <- pack(data))
[1] 93 01 02 03
> msgpack.writeResult("test.txt", d)
> unpack("test.txt")  # <= unpack from binary file
[1] 1 2 3
> unpack_bin(d)  # <= unpack from binary bits
[1] 1 2 3
> 
> # example to pack {"compact":true}, which appears at (*1).
> sample <- TRUE
> names(sample) <- c("compact")
> sample
compact 
   TRUE 
> pack(sample)
 [1] 81 a7 63 6f 6d 70 61 63 74 c3
> 
```

## advanced usage
```
# matrix
> (mat <- matrix(1:6, 2))
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6
> (m <- pack(mat))
[1] 92 93 01 03 05 93 02 04 06
> unpack_bin(m)
[[1]]
[1] 1 3 5

[[2]]
[1] 2 4 6

> msgpack_matrix(unpack_bin(m))  # <= transfer list format to matrix format
     [,1] [,2] [,3]
[1,] 1    3    5   
[2,] 2    4    6   
> 
> colnames(mat) <- c("A","B","C")
> mat
     A B C
[1,] 1 3 5
[2,] 2 4 6
> (m <- pack(mat))  # <= if data has colname, pack to "map"
 [1] 92 83 a1 41 01 a1 42 03 a1 43 05 83 a1 41 02 a1 42 04 a1 43 06
> unpack_bin(m)
[[1]]
A B C 
1 3 5 

[[2]]
A B C 
2 4 6 

> msgpack_matrix(unpack_bin(m))
     A B C
[1,] 1 3 5
[2,] 2 4 6
> 
```

# other sample codes
```
# Positive FixNum
> a <- 100
> pack(a)
[1] 64
> unpack_bin(pack(a))
[1] 100

# uint16
> a <- 2^10
> pack(a)
[1] cd 04 00
> unpack_bin(pack(a))
[1] 1024

# int16
> a <- -2^10
> pack(a)
[1] d1 fc 00
> unpack_bin(pack(a))
[1] -1024

# double
> a <- 10.1
> pack(a)
[1] cb 40 24 33 33 33 33 33 33
> unpack_bin(pack(a))
[1] 10.1

# FixRaw
> a <- "sample character"
> pack(a)
 [1] b0 73 61 6d 70 6c 65 20 63 68 61 72 61 63 74 65 72
> unpack_bin(pack(a))
[1] "sample character"
```
