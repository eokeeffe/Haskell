double x = x+x
quadruple = double . double
factorial n = product [1..n]

--any variable that begins or end in a 's' is generally a list
-- div is enclosed in back quotes to make it infix
average ns = div (sum ns) (length ns)