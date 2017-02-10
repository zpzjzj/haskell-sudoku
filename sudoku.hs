boardsize = 9
boxsize = 3
cellvals = "123456789"
blank = (=='.')

board = ["..9748...","7........",".2.1.9...","..7...24.",".64.1.59.",".98...3..","...8.3.2.","........6","...2759.."]

-- sudoku :: Board -> [Board]
type Matrix a = [[a]]
type Board = Matrix Char

correct :: Board -> Bool
correct b = all nodups(rows b) && all nodups(cols b) && all nodups(boxs b)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

-- | cols acts like transpose => cols . cols = id
cols :: Matrix a -> Matrix a
cols [xs] = [ [x] | x <- xs ] -- wrap each element in row as array for base case
cols (xs : xss) = zipWith (:) xs (cols xss) -- ':' => cons => head : tail

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group
-- map group => divide cols in each row into box
-- group => divide rows into box
-- map cols => do transpose thing, make box array grouped in col direction
-- ungroup => flatten

group :: [a] -> [[a]]
group = groupBy boxsize

ungroup :: [[a]] -> [a]
ungroup = concat

groupBy :: Int -> [a] -> [[a]]
groupBy size [] = []
groupBy size xs = take size xs: groupBy size (drop size xs)


type Choices = [Char]

-- | choices returns constrains mapped from board
choices :: Board -> Matrix Choices
choices = map (map choose)

-- | choose e returns wrapped constrains set mapped from e
choose :: Char -> [Char]
choose e = if blank e then cellvals else [e]

-- | mcp returns matrix cartesian product
mcp :: Matrix [a] -> [Matrix a]
mcp = cp . map cp

-- | cp returns carteseian product of a list of list, enumerate all posiblilities
cp :: [[a]] -> [[a]]
cp[] = [[]]
cp(xs : xss) = [x:ys | x <- xs, ys <- cp xss]

-- sudoku version 1, most inefficient
-- sudoku :: Board -> [Board]
-- sudoku = filter correct . mcp .choices

-- > fixed ["123456789","123456789","9","7","4","8","123456789","123456789","123456789"] == "9748"
fixed :: [Choices] -> Choices
fixed = concat . filter single

single :: [a] -> Bool
single [xs] = True
single x = False

-- reduce css returns reduced constrains on an area
-- > reduce ["123456789","123456789","9","7","4","8","123456789","123456789","123456789"] == ["12356","12356","9","7","4","8","12356","12356","12356"]
reduce :: [Choices] -> [Choices]
reduce css = map(remove(fixed css)) css

-- > remove "9748" "123456789" == "12356"
-- > remove "9748" "9" == "9"
remove :: Choices -> Choices -> Choices
remove fs cs = if single cs then cs else delete fs cs

-- | delete fs cs remove fs from cs
-- delete fs cs = fs \\ cs
-- delete_fs = foldl . flip delete
-- > delete "9748" "123456789" == "12356"
delete :: Choices -> Choices -> Choices
delete fs [] = []
delete fs (cs:css) = if elem cs fs then delete fs css else cs : delete fs css

filter_correct :: [Board] -> [Board]
filter_correct = filter (all nodups . boxs) . filter (all nodups . cols) . filter (all nodups . rows)

-- | f can be rows, cols and boxs
-- | do f to choices to build area
-- | reduce on area
-- | do f to return id (f . f = id)
pruneBy :: (Matrix Choices -> Matrix Choices) -> (Matrix Choices -> Matrix Choices)
pruneBy f = f . map reduce . f

-- | prune return reduced board
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- sudoku version 2, still impractical
-- sudoku :: Board -> [Board]
-- sudoku = filter_correct . mcp . prune . choices


blocked :: Matrix Choices -> Bool
blocked cm = void cm || not (safe cm)

void :: Matrix Choices -> Bool
void = any(any null)

safe :: Matrix Choices -> Bool
safe cm = all(nodups . fixed)(rows cm) &&
            all(nodups . fixed)(cols cm) &&
            all(nodups . fixed)(boxs cm)


constrains = prune (choices board)

expand :: Matrix Choices -> [Matrix Choices]
expand cm = [rows1 ++ [row1 ++ [c]:row2] ++rows2 | c <- cs]
            where (rows1, row : rows2) = break (any best) cm
                  (row1, cs : row2) = break best row
                  best cs = (length cs == n) -- is dest length of chioces
                  n = minchoice cm

-- | minchoice choices returns min choice num among all cells
minchoice :: Matrix Choices -> Int
minchoice = minimum . filter(> 1) .concat . map(map length)

search :: Matrix Choices -> [Matrix Choices]
search cm
    | blocked cm = []
    | all(all single) cm = [cm]
    | otherwise = (concat . map(search . prune) . expand) cm

sudoku :: Board -> [Board]
sudoku = map(map head) . search . prune . choices
