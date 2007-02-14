<html lang="en">
<head>
  <link rel="stylesheet" type="text/css" href="mecce.css" />
  <title>&#9733; ECCE, the Partial Deduction system &#9733;</title>
</head>
<script src="mecce.js" type="text/javascript"></script>

<body onLoad="window.status='',ds();">

<div id="main">
<form name="Struct" enctype="multipart/form-data" method="POST"
      onSubmit="return validate();" action="<?=$_SERVER['PHP_SELF']?>">

<input type="hidden" name="form_data" value="present"/>

<div id="top">
    <img src="ecce_bar.jpg" style="float:left;">

    <fieldset id="info"><legend>Information</legend>
    <button onClick="window.open('doc/help.php','winHelp','width=700,height=500');"
        name="help" class="ecce_button">Help</button>
    <br/>
    <button onClick="window.open('doc/about.php','winAbout','width=350,height=300');"
        name="about" class="ecce_button">About us</button>
    <br/>
    <button onClick="window.open('downloads/index.php','newWin','width=500,height=300');"
        name="download" class="ecce_button">Download</button>
    </fieldset>

<br style="clear:both;"/>    
</div> <!-- /top -->



<input type="hidden" name="MAX_FILE_SIZE" value="<?= MAX_UPLOAD_SIZE ?>"/>

<div id="middle">
<div id="source">

<fieldset><legend>Source</legend>
    <input type="file" name="InputFile" onChange="choosing();"/>
    <? show_button('load_file'); ?>
    <? show_button('save_file'); ?>
    <textarea cols="90" rows="20" name="source"
              title="INPUT PROLOG FILE: You can either load it using the above buttons, load a working example from the righthand side menu, or copy and paste your file here" 
              onFocus="editing_pl();">
<?=htmlspecialchars($source)?>
    </textarea><br/>
</fieldset><br/>

<fieldset class="half"><legend>Goal</legend>
<input type="text" size="36" name="goal" 
       id="goal" title="Type in the Prolog atom or goal to be used within any of the following actions" 
       value="<?=htmlspecialchars($goal)?>"/>
</fieldset><fieldset class="half">

<legend>Actions</legend>

    <? show_button('pe'); ?>
    <? show_button('slice'); ?>
    <? show_button('msv'); ?>
    <? show_button('raf'); ?>
    <? show_button('far'); ?>
</fieldset><br/><br/>

<fieldset><legend>Specialised Code</legend>
    <? show_button('save_xce'); ?>
    <? show_button('swap'); ?>
    <? show_button('dot'); ?>
  <iframe name="pl_out" style="display:block;" 
            src="pl_out.php">
<textarea cols="90" rows="20" name="res" readonly="true">
<?=$res; ?>
</textarea>
</iframe>


</fieldset>
</div>
<div id="options">

<fieldset><legend>Working Examples</legend>
    <select id="preload_id" name="preload_example">
<?
    foreach($prolog_examples as $key=>$ex) 
        echo "\t\t<option value=\"$key\">".$ex['des']."</option>\n";
?>
    </select>
    <? show_button('pre_load'); ?>
</fieldset><br/>

<fieldset><legend>Control Setting</legend>
     <? show_cb('config_default');?>
     <? show_cb('fast'); ?>
     <? show_cb('classic'); ?>
     <? show_cb('mixtus'); ?>
     <? show_cb('classic-fast'); ?>
     <? show_cb('minimal'); ?>
     <? show_cb('term'); ?>
</fieldset><br/>

<fieldset><legend>Post-processing</legend>
     <? show_cb('pp_max');?>
     <? show_cb('pp_default'); ?>
     <? show_cb('pp_off'); ?>
</fieldset><br/>

<fieldset><legend>Plug-ins</legend>
    <? show_button('rul'); ?>
    <? show_button('rul_bup'); ?>
    <? show_button('nfta'); ?>
    <? show_button('nfta_bup'); ?>
    <? show_button('ciao_analyse'); ?>
    <? show_button('ciao_optimise'); ?>
   <button onClick="window.open('doc/plug-ins.php','winHelp','width=300,height=200');"
        name="About Plug-ins" class="ecce_button">About Plug-ins</button>
    <br/>

</fieldset><br/>

</div>
</div> <!-- /middle -->

</form>

<br style="clear:both;"/><br/>
</div> <!-- /main -->

<div id="bottom">
    <a href="index.php">ECCE</a> is part of the 
    <a href="http://www.clip.dia.fi.upm.es/Projects/ASAP/">ASAP Project</a>.
    &nbsp;&nbsp;&nbsp; Queries? Please contact the
    <a href="mailto: info1@cs.uni-duesseldorf.de">System Administrator</a>.
</div>

<? if (MECCE_DEBUG) echo $debug; ?>

</body>
