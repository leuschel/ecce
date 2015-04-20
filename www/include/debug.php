<?php

//show_runtime_error("testerror","grund dafuer");
//trigger_error("Cannot divide by zero", E_USER_ERROR)

function userErrorHandler($errno, $errmsg, $filename, $linenum, $vars)
{
   // timestamp for the error entry
   $dt = date("Y-m-d H:i:s (T)");

   $errortype = array (
               E_ERROR          => "Error",
               E_WARNING        => "Warning",
               E_PARSE          => "Parsing Error",
               E_NOTICE          => "Notice",
               E_CORE_ERROR      => "Core Error",
               E_CORE_WARNING    => "Core Warning",
               E_COMPILE_ERROR  => "Compile Error",
               E_COMPILE_WARNING => "Compile Warning",
               E_USER_ERROR      => "User Error",
               E_USER_WARNING    => "User Warning",
               E_USER_NOTICE    => "User Notice",
               E_STRICT          => "Runtime Notice"
               );
   // set of errors for which a var trace will be saved
   $user_errors = array(E_USER_ERROR, E_USER_WARNING, E_USER_NOTICE);
  
   $err = "<errorentry>\n";
   $err .= "\t<datetime>" . $dt . "</datetime>\n";
   $err .= "\t<errornum>" . $errno . "</errornum>\n";
   $err .= "\t<errortype>" . $errortype[$errno] . "</errortype>\n";
   $err .= "\t<errormsg>" . $errmsg . "</errormsg>\n";
   $err .= "\t<scriptname>" . $filename . "</scriptname>\n";
   $err .= "\t<scriptlinenum>" . $linenum . "</scriptlinenum>\n";

//   if (in_array($errno, $user_errors)) {
//       $err .= "\t<vartrace>" . wddx_serialize_value($vars, "Variables") . "</vartrace>\n";
//   }
   $err .= "</errorentry>\n\n";   

   $err = $dt." ".$errno." ".$errortype[$errno]." ".$errmsg." in ".$filename." line ".$linenum."\n";
  
   // save to the error log, and e-mail me if there is a critical user error
    error_log($err, 3, DEBUG_ERROR_HANDLER_LOG_FILE);
//   mail(DEBUG_ERROR_MAIL_TO, "Critical User Error", $err,$GLOBALS['pref']['mail_headers']);

   show_runtime_error("Interner Fehler","handled by userErrorHandler");
   exit();
}



// wenn die debug_on variable gesetzt ist werden debuginfos auf den Bildschirm gedruckt
function debug_print($content) 
{
  if(MECCE_DEBUG) {
    print "DEBUG: ". $content . "<br>";
  }
}

function debug_print_array($a)
{
  if(MECCE_DEBUG) {
	debug_print_array_2("debug:",$a);
	}
}

function debug_print_array_2($int,$a)
{
	foreach  ($a as $key => $value) 
	{
		echo $int." $key -> $value<br>\n";
		if (is_array($value)) debug_print_array_2($int.".".$key,$value);
		}
}

function show_runtime_error($msg,$internal_msg)
{
	$log_date = ntool_log("runtime error ".$msg." ".$internal_msg);

	if (isset($_SESSION))
	{
		$_SESSION = array();
		if (isset($_COOKIE[session_name()]))
		   setcookie(session_name(), '', time()-42000, '/');
		@session_destroy();
	}

	$obs=ob_get_clean();
	header("Location: {$GLOBALS['pref']['runtime_error_url']}?datum=".$log_date."&msg=".$msg);
	exit();
}


function ntool_log($msg)
{
	$log_date=date("Y-m-d H:i:s (T)");
	$log_msg = $log_date;
	if (isset($_SERVER["REMOTE_ADDR"])) $log_msg.=" ".$_SERVER["REMOTE_ADDR"];
	
	$log_msg.=" ".addcslashes ($msg,"");
	$log_msg.="\n";

	error_log($log_msg, 3, DEBUG_LOG_FILE);
	return ($log_date);
}
?>