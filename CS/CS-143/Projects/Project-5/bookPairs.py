# Imports
from pyspark import SparkContext

# Helper function to convert data into tuples
def getTuples(line):
    res = []
    # Get array of book IDs in asc order
    bookIDs = line.split(":")[1].split(",")
    # Build tuples of all books reviewed together
    for i in range(len(bookIDs)):
        for j in range(i + 1, len(bookIDs)):
            res.append((bookIDs[i], bookIDs[j]))
    return res

# Global variables
INPUT = "/home/cs143/data/goodreads.user.books"
OUTPUT = "./output"

# Set up SparkContext
sc = SparkContext("local", "BookPairs")

# Read from target file
lines = sc.textFile(INPUT)

# Map to tuples of form (book_pair, 1)
bookPairs = lines.flatMap(getTuples).map(lambda pair : (pair, 1))
# Reduce
bookPairCnt = bookPairs.reduceByKey(lambda a, b : a + b)

# Filter out all counts <= 20
res = bookPairCnt.filter(lambda tuple : tuple[1] > 20)

# Write to output directory
res.saveAsTextFile(OUTPUT)
