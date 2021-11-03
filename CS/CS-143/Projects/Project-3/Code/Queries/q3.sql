SELECT familyName
FROM Person, Awarded
WHERE id = laureate_id
GROUP BY familyName
HAVING COUNT(*) >= 5;