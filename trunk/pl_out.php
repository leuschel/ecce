<?php
session_start();
$file="";
$explanation="";
?>
<head>
<title><?=$file?></title>
<link rel="stylesheet" type="text/css" href="mecce.css" />
<style>
* {font-family: Verdana; font-size: 10px;}
</style>
</head>
<body>
<div title="<?=$explanation?>">
<?
if (isset($_SESSION['pl_out'])) echo $_SESSION['pl_out'];
?>
</div>
</body>
