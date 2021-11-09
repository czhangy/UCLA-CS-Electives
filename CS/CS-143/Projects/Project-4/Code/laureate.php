<?php

    // Get the id parameter from the request
    $id = intval($_GET['id']);
    // Set the Content-Type header to JSON
    header('Content-Type: application/json');
    // Connect to MongoDB
    $db = new MongoDB\Driver\Manager("mongodb://localhost:27017");
    // Construct the filter and options
    $filter = [ 'id' => strval($id) ];
    $options = ["projection" => ['_id' => 0]];
    // Build the query
    $query = new MongoDB\Driver\Query($filter, $options);
    // Execute the query, transforming the results into an array
    $rows = $db->executeQuery("nobel.laureates", $query)->toArray();
    // Output the JSON encoding of the query result
    if ($rows[0]) echo json_encode($rows[0]);
?>
