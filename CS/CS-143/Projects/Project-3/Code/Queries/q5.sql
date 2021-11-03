SELECT COUNT(DISTINCT awardYear)
FROM Awarded, Organization
WHERE laureate_id = id;
