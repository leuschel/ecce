<?php

define("MECCE_DEBUG",FALSE);

// if not debugging do ob_start();
ob_start();

require_once("config.php");
set_include_path(INCLUDE_PATH_HIDDEN.':.');

ini_set('error_log',DEBUG_ERROR_LOG_FILE);

if (MECCE_DEBUG) ini_set('display_errors',true);

require_once('debug.php');
if (! MECCE_DEBUG) set_error_handler('userErrorHandler');

error_reporting(E_ALL);



//if (MECCE_DEBUG) ini_set('save_mode',true);
//if (MECCE_DEBUG) ini_set('error_prepend_string','ecce_msg');

if (!isset($_SESSION)) session_start();


require_once('button.php');
require_once('gui_def.php');

function fill_tpl($tpl,$data)
{
	extract($data);
	ob_start();
	require($tpl);
	return ob_get_clean();
}


function show_mecce($data)
{
//	debug_print_array($data);
	$debug=ob_get_clean();	
	extract($data);
	ob_start();
	require(MECCE_TPL);
	echo ob_get_clean();
	exit();
}

function clear_meldung()
{
	$_SESSION['ntool_meldung']="";
}

function set_meldung($meldung)
{
	if (!isset($_SESSION['ntool_meldung'])) $_SESSION['ntool_meldung']="";
	$_SESSION['ntool_meldung'].=$meldung;
}


function html_redirect($url)
{
	$obs=ob_get_clean();
	header("Location: $url");
	exit();
}
?>