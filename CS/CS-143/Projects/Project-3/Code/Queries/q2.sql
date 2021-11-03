SELECT country
FROM Place
WHERE id = (SELECT DISTINCT place_id
            FROM Affiliated
            WHERE name = 'CERN');
