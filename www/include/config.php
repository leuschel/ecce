<?php

define("INCLUDE_PATH_HIDDEN",'/var/www/html/ecce/include');

define("MECCE_TPL",'tpl.php');

define("MAX_UPLOAD_SIZE",'100000'); 




define("DEBUG_ERROR_LOG_FILE","/var/www/html/ecce/mecce_error.log");
define("DEBUG_ERROR_HANDLER_LOG_FILE","/var/www/html/ecce/mecce_error_handler.log");
define("DEBUG_LOG_FILE","/var/www/html/ecce/mecce.log");
define("MECCE_BASEDIR","/var/www/html/ecce/");

define("MECCE_BIN","/var/www/html/ecce/bin/mecce");
define("RUL_BIN","/var/www/html/ecce/bin/rul.exe");
define("NFTA_BIN","/var/www/html/ecce/bin/nfta");
define("CIAOPPCL_BIN","/var/www/html/ecce/bin/ciaoppcl");

define("DOT_CMD","/usr/bin/dot -Tsvg");


$pref = array();
$pref['base_url']="http://wyvern.cs.uni-duesseldorf.de/ecce/";
$pref['runtime_error_url']=$pref['base_url']."runtime_error.php";

?>
