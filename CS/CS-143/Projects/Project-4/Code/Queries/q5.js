const aggregate = [
    { $match : { "orgName" : { $ne : null } } },
    { $unwind : "$nobelPrizes" },
    { $group : { _id : "$nobelPrizes.awardYear" } },
    { $count : "years" }
];

db.laureates.aggregate(aggregate).pretty();
