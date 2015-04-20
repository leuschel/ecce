<?php
session_start();
$file="";
$explanation="";
?>
<head>
<title><?php $file?></title>
<link rel="stylesheet" type="text/css" href="asap-online-demo.css" />
<style>
* {font-family: Verdana; font-size: 10px;}
</style>
</head>
<body>
<div title="<?php $explanation?>">
<?php
if (isset($_SESSION['pl_out'])) echo $_SESSION['pl_out'];
?>
</div>
</body>
