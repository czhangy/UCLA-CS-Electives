const aggregate = [
    { $unwind : "$nobelPrizes" },
    { $unwind : "$nobelPrizes.affiliations" },
    { $project : { _id : 0, "nobelPrizes.affiliations" : 1 }},
    { $match : { "nobelPrizes.affiliations.name.en" : "CERN" }},
    { $group : { _id : "$nobelPrizes.affiliations.name.en", country : { "$push" : "$nobelPrizes.affiliations.country.en" }}},
    { $project : { _id : 0, "country" : 1 }},
    { $unwind : "$country" },
    { $limit : 1 }
];

db.laureates.aggregate(aggregate).pretty();
