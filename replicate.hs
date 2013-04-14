mreplicate::Int->a->[a]
mreplicate n x	| n <= 0 = []
				| n>0 = x:replicate (n-1) x