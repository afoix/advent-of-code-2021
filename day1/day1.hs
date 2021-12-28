type Mesuraments = [Int]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

loadInput :: FilePath -> IO Mesuraments
loadInput = fmap (map read) . readLines

countIncreases :: Mesuraments -> Int
countIncreases mesuraments  =
  let _:pairs = zip mesuraments (0:mesuraments)
  in length(filter increasingPair pairs)
  where increasingPair (x, y) = x > y   

compareGroups :: Mesuraments -> Int
compareGroups mesuraments = 
  countIncreases sumGroups
  where _:_:groups = zip3 mesuraments (0:mesuraments) (0:0:mesuraments)
        sumGroup (x, y, z) = x + y + z
        sumGroups = map sumGroup groups  
