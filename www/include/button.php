<?php

require_once('cgi_param.php');

function def_button($name,$value,$des,$fkt)
{
    $GLOBALS['button_list'][$name]=array('name'=>$name,'value'=>$value,'des'=>$des,'fkt'=>$fkt);
}

function show_button($name)
{
    $button=$GLOBALS['button_list'][$name];
    $GLOBALS['curname'] = $name;
    $GLOBALS['curbutton'] = $button;
?>
<input type="submit" name="<?php echo $name; ?>" value="<?php echo $button['value']; ?>" title="<?php echo $button['des']; ?>"
onClick="document.pressed=this.name;document.value=this.value;"/>

<?php
}

function execute_button()
{
	debug_print('execute_button');
//	phpinfo();
	foreach ($GLOBALS['button_list'] as $key => $button)
		if (check_post_flag($button['name'],$button['value']))
		{
			   $fkt=$button['fkt'];
			   debug_print("calling $fkt"); 
			   $fkt();
		}
}


function def_cb($type,$name,$value,$des,$default)
{
	$GLOBALS['cb_list'][$value]=array('type'=>$type,'name'=>$name,'value'=>$value,'des'=>$des, 'default'=>$default );
}


function show_cb($value)
{
$cb=$GLOBALS['cb_list'][$value];
?>
<input type="<?php echo $cb['type']; ?>" name="<?php echo $cb['name']; ?>" value="<?php echo $cb['value']; ?>" id="<?php echo $cb['value']; ?>"
<?php
	if ( (check_post_flag($cb['name'],$cb['value'])) 
	    or ( $cb['default'] and ! check_post_flag('form_data','present') ))  
	    	echo " checked ";

?>
<label for="<?php echo $cb['value']; ?>"><?php echo $cb['des']; ?></label><br/>
<?php
}

function check_post_flag($name,$value)
{
	if (isset($_REQUEST[$name]) and $_REQUEST[$name]==$value) return TRUE;
	return FALSE;
}

function source()
{
    debug_print('parse_source');
    //hack to enable call form asap:
    if (isset($_SESSION['from_asap']))
    {
	$t=$_SESSION['from_asap'];
	unset($_SESSION['from_asap']);
	return($t);
    }
    return cgi_param('source','',100000,'text');
}

function xce()
{
    debug_print('parse_xce');
    return cgi_param('xce','',100000,'text');
}

function goal()
{
    return cgi_param('goal','',100,'text');
}



function sgoal()
{
    return escapeshellarg(cgi_param('goal','',100,'text'));
}

?>
