<?php
function run_process($command)
	 {return run_process_with_input($command,'');}

function run_process_with_input($command,$input)
{
	debug_print('run_process:cmd='.$command);

	$descriptorspec = array(
        0 => array("pipe", "r"),
        1 => array("pipe", "w"),
        2 => array("pipe", "w")
    );


    $env=array('HOME'=>'/var/lib/wwwrun');

    $process = proc_open($command, $descriptorspec, $pipes,'tmp',$env);

    if(is_resource($process))
    {
        fwrite($pipes[0], $input);
        fclose($pipes[0]);

	       $starttime = microtime(true);
		if(!isset($maxtime))
		{
			$maxtime = 10;
		}
	$maxtime = 100;
		$return_value = 0;
		$timeout = false;

	$output='';
	$error='';

		while(true)
		{
			$status = proc_get_status($process);
			if(!$status['running'])
			{
				#print_r($status);
				break;
			}
			usleep(100000);
			if(microtime(true) - $starttime > $maxtime)
			{
				#print microtime(true) - $starttime;
				#print "<br>";
				proc_terminate($process);
				$timeout = true;
				$ret['time_out']=1;
				$return_value = -1;
				break;
			}
		}
	$output = stream_get_contents($pipes[1]);
	$error = stream_get_contents($pipes[2]);

		if(!$timeout)
		{
			$return_value = proc_close($process);
			$return_value = $status['exitcode'];
		}
		else
		{
			$return_value = -1;
		}

		if($timeout)
		{
			$error.="\nProcess timed out\n";
			$ret['time_out']=1;
		}


	$ret['exit']=$return_value;
	$ret['stdout']=$output;
	$ret['stderr']=$error;
	debug_print_array($ret);
	return $ret;
    }
    else
        show_runtime_error("Internal Error","Cannot run $command");
}

function file_with($contents)
{
	$fname=tempnam('/tmp','ecce');
	$GOLBALS['open_files'][$fname]=TRUE;
	file_put_contents($fname,$contents);
	return $fname;
}

function from_tmp($fname)
{
    return (file_get_contents($fname));
} 


function pl2html($pl)
{
	$pl=htmlspecialchars($pl);

	$pl=str_replace("\t",'&nbsp;&nbsp;&nbsp;&nbsp;',$pl);
	$pl=str_replace(' ','&nbsp;',$pl);
	$pl=nl2br($pl);
//	$pl='<pre>'.$pl.'</pre>';
	return $pl;
}

function check_time_out($ret)
{
	if (! isset($ret['time_out']))	return;
	debug_print("Process Time Out");
	set_meldung("Process Time Out");
	show_ecce($GLOBALS['dout']);
	exit();
}
?>
