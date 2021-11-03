// Import fs module to read/write file
const fs = require("fs");

// Load JSON data
const file = fs.readFileSync("/home/cs143/data/nobel-laureates.json");
const data = JSON.parse(file).laureates;

// Init sets
let npKeys = new Set();
let placeDict = {};

// IDs
let placeID = 1;

// Iterate through all laureates
for (let laureate of data) {
  // Get laureate attributes
  let id = laureate.id;
  if (laureate.orgName) {
    // Get organization attribute
    let orgName = laureate.orgName.en;
    // Write to organization.del
    fs.writeFileSync("./organization.del", `${id}\t${orgName}\n`, {
      flag: "a+",
    });
  } else {
    // Get person attributes
    let givenName = laureate.givenName ? laureate.givenName.en : "\\N";
    let familyName = laureate.familyName ? laureate.familyName.en : "\\N";
    let gender = laureate.gender ? laureate.gender : "\\N";
    // Write to person.del
    fs.writeFileSync(
      "./person.del",
      `${id}\t${givenName}\t${familyName}\t${gender}\n`,
      {
        flag: "a+",
      }
    );
  }
  // Get Place attributes
  let city, country;
  if (laureate.birth) {
    city = laureate.birth.place.city ? laureate.birth.place.city.en : "\\N";
    country = laureate.birth.place.country
      ? laureate.birth.place.country.en
      : "\\N";
  } else if (laureate.founded) {
    city = laureate.founded.place.city ? laureate.founded.place.city.en : "\\N";
    country = laureate.founded.place.country
      ? laureate.founded.place.country.en
      : "\\N";
  } else {
    city = "\\N";
    country = laureate.birthCountry ? laureate.birthCountry : "\\N";
  }
  let placeKey = city + "#" + country;
  // Check if place has already been inserted
  if (!(placeKey in placeDict)) {
    // Insert into set
    placeDict[placeKey] = placeID;
    // Write to place.del
    fs.writeFileSync("./place.del", `${placeID}\t${city}\t${country}\n`, {
      flag: "a+",
    });
    // Get next unique ID
    placeID++;
  }
  // Handle Born/Founded
  if (laureate.orgName) {
    // Get Founded attributes
    let foundingDate = laureate.founded ? laureate.founded.date : "\\N";
    // Write to founded.del
    fs.writeFileSync("./founded.del", `${id}\t${placeID}\t${foundingDate}\n`, {
      flag: "a+",
    });
  } else {
    // Get Born attributes
    let birthDate = laureate.birth ? laureate.birth.date : "\\N";
    // Write to born.del
    fs.writeFileSync("./born.del", `${id}\t${placeID}\t${birthDate}\n`, {
      flag: "a+",
    });
  }
  // Get NobelPrize attributes
  for (let nobel of laureate.nobelPrizes) {
    let awardYear = nobel.awardYear;
    let category = nobel.category.en;
    let sortOrder = nobel.sortOrder;
    let npKey = awardYear + "#" + category + "#" + sortOrder;
    if (!npKeys.has(npKey)) {
      // Insert into set
      npKeys.add(npKey);
      // Write to nobelprize.del
      fs.writeFileSync(
        "./nobelprize.del",
        `${awardYear}\t${category}\t${sortOrder}\n`,
        {
          flag: "a+",
        }
      );
    }
    // Write to awarded.del
    fs.writeFileSync(
      "./awarded.del",
      `${id}\t${awardYear}\t${category}\t${sortOrder}\n`,
      {
        flag: "a+",
      }
    );
    // Handle affiliations
    if (nobel.affiliations) {
      for (let affiliation of nobel.affiliations) {
        // Get Affiliated attributes
        let affKey = affiliation.city + "#" + affiliation.country;
        let pid;
        if (affKey in placeDict) pid = placeDict[affKey];
        else {
          // Update ID
          pid = placeID;
          placeDict[affKey] = pid;
          placeID++;
          // Write to place.del
          fs.writeFileSync(
            "./place.del",
            `${placeID}\t${affiliation.city}\t${affiliation.country}\n`,
            {
              flag: "a+",
            }
          );
        }
        let affName = affiliation.name.en;
        // Write to affiliated.del
        fs.writeFileSync(
          "./affiliated.del",
          `${awardYear}\t${category}\t${sortOrder}\t${pid}\t${affName}\n`,
          {
            flag: "a+",
          }
        );
      }
    }
  }
}
