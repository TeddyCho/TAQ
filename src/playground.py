import os
import csv
import time

myPath = "C:\\Users\\eung.cho\\workspace\\TAQ\\data\\"
myFile = "7a4da2397f14c7d9.csv"
myFullFileName = os.path.join(myPath, myFile)

with open(myFullFileName, 'r') as f:
    reader = csv.DictReader(f)
    print(next(reader))
    print(next(reader))
    print(next(reader))
    
    t = time.time()
    print(sum(1 for row in reader))
    elapsed = time.time() - t
    print("Time elapsed: %s seconds" % elapsed)