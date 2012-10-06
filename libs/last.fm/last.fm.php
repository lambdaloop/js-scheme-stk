<?php

header('Content-type: text/xml');
ini_set('allow_url_fopen', 1);
$request = "http://ws.audioscrobbler.com/2.0/?";
foreach ($_GET as $key => $value) {
    $request .= "&$key=$value";
}
echo file_get_contents($request);

?>