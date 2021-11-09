// Import fs module to read/write file
const fs = require('fs');

// Load JSON data into program
let file = fs.readFileSync("/home/cs143/data/nobel-laureates.json");

// Get laureates object
let data = JSON.parse(file).laureates;

// For each laureate in laureates
for (let laureate of data)
    // Append laureate's JSON to laureates.import
    fs.writeFileSync('./laureates.import', JSON.stringify(laureate, null, 4), {
        flag: "a+",
    });


