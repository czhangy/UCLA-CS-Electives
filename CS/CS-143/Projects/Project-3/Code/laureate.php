<?php
// Get the id parameter from the request
$id = intval($_GET['id']);

// Set the Content-Type header to JSON
header('Content-Type: application/json');

// Connect to class_db
$db = new mysqli('localhost', 'cs143', '', 'class_db');
if ($db->connect_errno > 0)
    die('Unable to connect to database [' . $db->connect_error . ']'); 

// Get data
$query = "SELECT * FROM Person WHERE id = $id";
$res = $db->query($query);

// Refetch if the laureate is an organization
if ($res->num_rows == 0) {
    $query = "SELECT * FROM Organization WHERE id = $id";
    $res = $db->query($query);
}

// Check if ID exists
if ($laureate = $res->fetch_assoc()) {
    // Assign ID and Nobel Prize array
    $output = (object) [ "id" => strval($id) ];
    // Assign names
    if ($laureate["orgName"])
        $output->orgName->en = $laureate["orgName"];
    else {
        // Assign given/familyName if they exist
        if ($laureate["givenName"])
            $output->givenName->en = $laureate["givenName"];
        if ($laureate["familyName"])
            $output->familyName->en = $laureate["familyName"];
        // Assign gender
        $output->gender = $laureate["gender"];
    }
    // Check either founding or birth information
    if ($laureate["orgName"]) {
        // Fetch founding information
        $query = "SELECT * FROM Founded WHERE org_id = $id";
        $res = $db->query($query);
        // Check if data exists
        if ($founded = $res->fetch_assoc()) {
            // Assign founding date
            $output->founded->date = $founded["date"];
            // Fetch founding place
            $placeID = $founded["place_id"];
            $query = "SELECT * FROM Place WHERE id = $placeID";
            $foundingPlace = $db->query($query)->fetch_assoc();
            // Assign birth city/country if they exist
            if ($foundingPlace["city"])
                $output->founded->place->city->en = $foundingPlace["city"];
            if ($foundingPlace["country"])
                $output->founded->place->country->en = $foundingPlace["country"];
        }
    } else {
        // Fetch birth information
        $query = "SELECT * FROM Born WHERE person_id = $id";
        $res = $db->query($query);
        // Check if data exists
        if ($born = $res->fetch_assoc()) {
            // Assign birth date if it exists
            if ($born["date"])
                $output->birth->date = $born["date"];
            // Fetch birth place
            $placeID = $born["place_id"];
            $query = "SELECT * FROM Place WHERE id = $placeID";
            $birthPlace = $db->query($query)->fetch_assoc();
            // Assign birth city/country if they exist
            if ($birthPlace["city"])
                $output->birth->place->city->en = $birthPlace["city"];
            if ($birthPlace["country"])
                $output->birth->place->country->en = $birthPlace["country"];
        }
    }
    // Fetch Nobel Prizes awarded
    $query = "SELECT * FROM Awarded WHERE laureate_id = $id";
    $res = $db->query($query);
    // Assign all Nobel Prizes
    $output->nobelPrizes = array();
    while ($awarded = $res->fetch_assoc()) {
        // Assign NobelPrize attributes
        $awardYear = $awarded["awardYear"];
        $category = $awarded["category"];
        $sortOrder = $awarded["sortOrder"];
        $nobel = (object) [
            "awardYear" => strval($awardYear),
            "category" => (object) [
                "en" => $category
            ],
            "sortOrder" => strval($sortOrder)
        ];
        // Fetch affiliations
        $query = "SELECT * FROM Affiliated WHERE awardYear = $awardYear AND category = '$category'
                  AND sortOrder = $sortOrder";
        $aff_res = $db->query($query);
        // Check if any affiliations exist
        if ($aff_res->num_rows > 0) {
            $nobel->affiliations = array();
            // Gather affiliations data
            while ($affiliated = $aff_res->fetch_assoc()) {
                // Assign affiliation name
                $affiliation = (object) [ "name" => $affiliated["name"] ];
                // Fetch affiliation location
                $placeID = $affiliated["place_id"];
                $query = "SELECT * FROM Place WHERE id = $placeID";
                $aff_place_res = $db->query($query);
                // Check if affiliation location exists
                if ($place = $aff_place_res->fetch_assoc()) {
                    // Assign location data if it exists
                    if ($place["city"])
                        $affiliation->city->en = $place["city"];
                    if ($place["country"])
                        $affiliation->country->en = $place["country"];
                }
                // Add affiliation to result
                array_push($nobel->affiliations, $affiliation);
            }
        }
        // Append to array
        array_push($output->nobelPrizes, $nobel);
    }
}

// Close DB connections
$res->free();
$db->close();

// Convert to JSON
echo json_encode($output);
?>
