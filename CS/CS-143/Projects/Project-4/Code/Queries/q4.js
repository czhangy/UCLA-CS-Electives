const aggregate = [
    { $unwind: "$nobelPrizes" },
    { $unwind: "$nobelPrizes.affiliations" },
    { $match: { "nobelPrizes.affiliations.name.en": "University of California" } },
    { $group: { _id: { "city": "$nobelPrizes.affiliations.city.en", "country": "$nobelPrizes.affiliations.country.en" }, "num": { "$sum": 1 } } },
    { $count: "locations" }
];

db.laureates.aggregate(aggregate).pretty();
