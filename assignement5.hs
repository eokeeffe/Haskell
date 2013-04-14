--Take an ordered list and insert into the correct place
--i.e if the input number is <= the previous and less than
--the next
insert::(Ord a) => [a] -> a -> [a]
insert [] elem = elem:[]
insert (x:xs) elem	| elem<=x = elem:(x:xs)
					| elem>x = x:(insert xs elem)

--Insertion sort
insort::(Ord a) => [a] -> [a]
insort [] = []
insort (x:xs) = insert (insort xs) x

--Merge
merge:: (Ord a) => [[a]] -> [a]
merge [] = []
merge (x:[]) = x
merge (x:xs) = insort (x ++ merge (xs))

--Center
--arg1 = list of letters,arg2 = fill width,arg3 = fill character
center::(Ord a) => [a]->Int->a->[a]
center arg1 arg2 arg3	| length arg1 <= 0 = []
						| length arg1 == arg2 = arg1
						| not(length arg1 == arg2) = left++arg1++right
						where
							l = length arg1
							remaining = arg2 - l
							front = quot remaining 2
							last = remaining - front
							left = createlist arg3 front
							right = createlist arg3 last
--Create a list of characters of size l
createlist::a -> Int -> [a]
createlist a 0 = []
createlist a l	| l < 0 = []
				| l > 0 = a:createlist a (l-1)

--Largest
largest::(Ord a) => [a]->a
largest (x:[]) = x
largest (x:xs) = max x (largest xs)