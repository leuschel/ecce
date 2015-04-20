<?php
require_once("../include/init.php");

require_once("button.php");
require_once("cgi_param.php");
require_once("gui_callback.php");


$dout=array();
$dout['prolog_examples']=$GLOBALS['prolog_examples'];
$dout['source']=source();
$dout['goal']=goal();
$dout['res']='';
$dout['res']=xce();

//if (! check_post_flag('form_data','present'))
//{
//	show_mecce($dout);
//	exit;
//}


execute_button();

show_mecce($dout);

?>
