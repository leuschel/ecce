<?php
require_once('help_funktions.php');

function mecce_opts()
{
	$opt=" ";
	$config=cgi_param('config','',20,'alnum');
	switch($config){
	    case "":
	    case "config_default": break;
	    case "classic":
            case "fast":
            case "mixtus":
            case "minimal":
            case "classic-fast":
            case "deforest":
            case "term":
                  $opt.=" -config ".$config;
                  break;
        default: die("<h2>Operation $config not allowed!</h2>");
	}
	$pp=cgi_param('pp','',20,'alnum');
	switch($pp){
	   case "":
	   case "pp_default": break;
	   case "pp_off": $opt.=' -pp off';break;
           case "pp_max": $opt.=" -pp max"; break;
          default: die("<h2>Operation $pp not allowed!</h2>");
	  }
	  return($opt." ");
}


function fkt_pre_load()
{
	$sel=cgi_param('preload_example','ex01',10,'alnum');
	$example=$GLOBALS['prolog_examples'][$sel];
	debug_print("loading example ".$example['file']);
	$GLOBALS['dout']['source']=file_get_contents(MECCE_BASEDIR.$example['file']);
	if (isset ($example['goal']))
	    $GLOBALS['dout']['goal']=$example['goal'];
	else
	    $GLOBALS['dout']['goal']='';
	$_SESSION['source_name']=basename($example['file'],'.pl');
	$_SESSION['specialized']='';
	$_SESSION['pl_out']='';
}

function fkt_load_file()
{
	debug_print('load_file');
	
	$size=$_FILES['InputFile']['size'];

	if ($_FILES['InputFile']['error'] or $size <=0 )
        {
		exit();
        }
	if ($size > MAX_UPLOAD_SIZE )
        {
		exit();
        }

    $_SESSION['source_name']=basename($_FILES['InputFile']['name'],'.pl');
    $GLOBALS['dout']['source']=file_get_contents($_FILES['InputFile']['tmp_name']);
	$_SESSION['specialized']='';
	$_SESSION['pl_out']='';
}

function fkt_save_file()
{
	debug_print('save_file');
	$debug=ob_get_clean();  
	header("Content-type: prolog");
	header("Content-Disposition: attachment; filename={$_SESSION['source_name']}.pl");
	echo $GLOBALS['dout']['source'];
	exit;
}

function fkt_save_xce()
{
	debug_print('save_xce');
	$debug=ob_get_clean();  
	header("Content-type: prolog");
	header("Content-Disposition: attachment; filename={$_SESSION['source_name']}_xce.pl");
	echo $_SESSION['specialized'];
	exit;
}

function fkt_swap()
{
	debug_print('swap');
	$GLOBALS['dout']['source']=$_SESSION['specialized'];
	$_SESSION['specialized']='';
	$_SESSION['pl_out']='';

}

function fkt_dot()
{
	debug_print('dot');
	$debug=ob_get_clean();
?>
<html lang="en"><head><title>Feedback</title>
<link rel="stylesheet" type="text/css" href="mecce.css"/>

</head><body>
<embed name="svgGraph" src="index.php?svg_out=true" width="680" height="450" type="image/svg+xml">
<div style="border: dashed 1px; padding: 0px 10px">
<p>To view the Specialisation Tree you need an <ahref="http://www.adobe.com/svg/viewer/install/main.html" target="_blank">SVG Viewer</a>.
Green edges labelled with <code>unf(L,N)</code> show the unfolding that has been done; where <code>L</code> is the literal number selected and <code>N</code> is the number of the clause resolved with.
Blue edges denote the descendency relationship at the global control level. 
Solid rectangles containing a definition with a double equality are global tree nodes for which code has been generated. 
Gray nodes are global tree nodes for which no code was generated, either because the node was abstracted or because it is an instance of another node.
</p><p><b>Note:</b> the tree is displayed before the post-processing is run, which may remove certain nodes, e.g., by determinate post-unfolding. 
Dashed gray edges show instance relationships in the global tree, while dashed red edges show were generalisation occurred.</p>
</div></body></html>
<?
    exit;
}

function fkt_svg_out()
{
	debug_print('svg_out');
	$cmd=DOT_CMD;

	$dot=$_SESSION['dot_data'];

	$ret=run_process_with_input($cmd,$dot);

	$debug=ob_get_clean();
	echo $ret['stdout'];
	exit();
}

function fkt_pe()
{
	debug_print('pe');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$dotfile=file_with('');
	$infile=file_with($source);

	$cmd=MECCE_BIN.mecce_opts()."-pe ".sgoal()." -dot $dotfile -o $outfile $infile";

	$ret=run_process($cmd);

	$xce=from_tmp($outfile);
	$_SESSION['dot_data']=from_tmp($dotfile);
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);
	
}

function fkt_slice()
{
	debug_print('slice');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$dotfile=file_with('');
	$infile=file_with($source);

	$cmd=MECCE_BIN.mecce_opts()."-slice ".sgoal()." -dot $dotfile -os $outfile $infile";

	$ret=run_process($cmd);

	$_SESSION['dot_data']=from_tmp($dotfile);

	set_slice($source,from_tmp($outfile),'ecce_slice');
}

function set_slice($source,$slice_des,$parser)
{
    $flags=$parser($slice_des);
    $sl=explode("\n",$source);
    
    foreach($sl as $key=>$line)
	if (isset($flags[$key]) and $line !="" and $line!="\r")
	    {
		$pl[$key]='%'.$line;
		$html[$key]='<div class="sliced">%'.htmlspecialchars($line).'</div>';
		}
	    else
	    {
		$pl[$key]=$line;
		$html[$key]=htmlspecialchars($line).'<br>';
	    }
	$_SESSION['specialized']=implode("\n",$pl);
	$_SESSION['pl_out']=implode("\n",$html);
}

function ecce_slice($delete)
{
    	$del_list=explode("\n",$delete);
	$flag=array();
	foreach($del_list as $key => $line )
	{	
		$h=explode(':',$line);
		if ($h[0]=='del') $s_line=$h[1]-1;
		else if ($h[0]==' to')
		{
			$e_line=$h[1]-1;
//			debug_print("startline ".$s_line."endline ".$e_line);
			for ($i=$s_line;($i<=$e_line);$i++) $flag[$i]['useless']=TRUE;
		}
	}
	return $flag;
}

function nfta_slice($delete)
{
	$flag=array();
    	$del_list=explode("\n",$delete);
	$regs=array();

	foreach($del_list as $key => $line )
	{
	    debug_print($line);
	    if (ereg ('^.*Lines:(.*)--(.*)$', $line, $regs)!=0)
	       {
//	       debug_print("startline ".$regs[1]."endline ".$regs[2]);
               for ($i=$regs[1]-1;($i<=$regs[2]-1);$i++) $flag[$i]['useless']=TRUE;
	   }
	}
	return $flag;
}


function fkt_msv()
{
	debug_print('msv');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$dotfile=file_with('');
	$infile=file_with($source);

	$cmd=MECCE_BIN.mecce_opts()."-msv -dot $dotfile -o $outfile $infile";

	$ret=run_process($cmd);

	$_SESSION['dot_data']=from_tmp($dotfile);
	$xce=from_tmp($outfile);
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);
}

function fkt_raf()
{
	debug_print('raf');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$dotfile=file_with('');
	$infile=file_with($source);

	$cmd=MECCE_BIN.mecce_opts()."-raf ".sgoal()." -dot $dotfile -o $outfile $infile";

	$ret=run_process($cmd);

	$_SESSION['dot_data']=from_tmp($dotfile);
	$xce=from_tmp($outfile);
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);
}


function fkt_far()
{
	debug_print('far');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$dotfile=file_with('');
	$infile=file_with($source);

	$cmd=MECCE_BIN.mecce_opts()."-far  -dot $dotfile -o $outfile $infile";

	$ret=run_process($cmd);

	$_SESSION['dot_data']=from_tmp($dotfile);
	$xce=from_tmp($outfile);
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);
}

function fkt_rul()
{
	debug_print('rul');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$infile=file_with($source);

	$cmd=RUL_BIN." $infile ".sgoal()." -o $outfile";

	$ret=run_process($cmd);

	$xce=from_tmp($outfile);
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);
}
function fkt_rul_bup()
{
	debug_print('rul_bup');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$infile=file_with($source);

	$cmd=RUL_BIN." $infile -b -o $outfile";

	$ret=run_process($cmd);

	$xce=from_tmp($outfile);
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);
}
function fkt_nfta()
{
	debug_print('nfta');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$infile=file_with($source);

	$cmd=NFTA_BIN." $infile -q ".sgoal()." -o $outfile";

	$ret=run_process($cmd);

	set_slice($source,from_tmp($outfile),'nfta_slice');
}
function fkt_nfta_bup()
{
	debug_print('nfta_bup');
	extract($GLOBALS['dout']);
	
	$outfile=file_with('');
	$infile=file_with($source);

	$cmd=NFTA_BIN." $infile -o $outfile";

	$ret=run_process($cmd);

	set_slice($source,from_tmp($outfile),'nfta_slice');
}

function fkt_ciao_optimise()
{
	debug_print('ciao_optimise');
	extract($GLOBALS['dout']);
	
	$infile=file_with(":-module(_,_).\n".$source);
	rename($infile,$infile.".pl");	
	$cmd=CIAOPPCL_BIN." -O $infile.pl";

//	$cmd="echo \"hall\";cat /tmp/zZF5ux;ls /tmp ";
	$ret=run_process($cmd);

	$xce=from_tmp($infile."_pd_codegen_af_co.pl");
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);

}

function fkt_ciao_analyse()
{
	debug_print('ciao_analyse');
	extract($GLOBALS['dout']);
	
	$infile=file_with(":-module(_,_).\n".$source);
	rename($infile,$infile.".pl");		

	$cmd=CIAOPPCL_BIN." -A $infile.pl";
	$ret=run_process($cmd);

	$xce=from_tmp($infile."_eterms_shfr_co.pl");
	$_SESSION['specialized']=$xce;
	$_SESSION['pl_out']=pl2html($xce);

}

?>