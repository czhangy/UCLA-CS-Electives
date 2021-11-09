const aggregate = [
    { $unwind : "$nobelPrizes" },
    { $match : { "familyName.en" : { $ne : null }}},
    { $group : { _id : "$familyName.en", num : { $sum : 1 }}},
    { $match : { num : { $gte : 5 }}},
    { $project : { _id : 0, "familyName" : "$_id" }}
];

db.laureates.aggregate(aggregate);
