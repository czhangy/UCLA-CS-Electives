db.laureates.find({ "givenName.en": "Marie", "familyName.en": "Curie" },
                  { _id: 0, "id": 1 });
