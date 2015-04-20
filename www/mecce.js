function ds(){
    
    document.Struct.load_file.disabled=true;
    document.Struct.save_file.disabled=true;
    var empty_xce=(document.Struct.res.value == '');
    document.Struct.save_xce.disabled=empty_xce;
    document.Struct.swap.disabled=empty_xce;
    document.Struct.dot.disabled=empty_xce;
}
function choosing(){
    document.Struct.load_file.disabled=false;
    document.Struct.save_file.disabled=true;
    document.Struct.save_xce.disabled=true;
    document.Struct.swap.disabled=true;
    document.Struct.dot.disabled=true;

}
function editing_pl(){
    document.Struct.load_file.disabled=true;
    document.Struct.save_file.disabled=false;
    document.Struct.save_xce.disabled=true;
    document.Struct.swap.disabled=true;
    document.Struct.dot.disabled=true;

}
function pl_edited(){
    document.Struct.load_file.disabled=true;
    document.Struct.save_file.disabled=true;
    document.Struct.save_xce.disabled=true;
    document.Struct.swap.disabled=true;
    document.Struct.dot.disabled=true;

}

function new_window() {
   url="http://wyvern.cs.uni-duesseldorf.de/ecce/index.php?form_data=present&new_window=on&";
   url=url.concat(document.pressed,"=",document.value);

   window.open(url,'winChTree','width=100,height=100');
   exit;
}

function set_target()
{
	document.forms.Struct.target="SVG-VIEWER";
}	

function unset_target()
{
	document.forms.Struct.target="_top";
}	



function validate()
{
    unset_target();
    var empty_source=(document.Struct.source.value == '');
    var empty_goal=(document.Struct.goal.value == '');
    var ret=true;
    var msg1="A SOURCE file is needed!";
    var msg2="Both SOURCE file and GOAL are needed!";
 
    if(document.pressed == 'pe') {
        ret=(!empty_source && !empty_goal);
        msg=msg2;
    }
    if(document.pressed == 'slice') {
        ret=(!empty_source && !empty_goal);
        msg=msg2;
    }
    if(document.pressed == 'msv') {
        ret=(!empty_source);
        msg=msg1;
    }
    if(document.pressed == 'raf') {
        ret=(!empty_source && !empty_goal);
        msg=msg2;
    }
    if(document.pressed == 'far') {
        ret=(!empty_source);
        msg=msg1;
    }
    if(document.pressed == 'rul') {
        ret=(!empty_source && !empty_goal);
        msg=msg2;
    }
    if(document.pressed == 'rul_bup') {
        ret=(!empty_source);
        msg=msg1;
    }
    if(document.pressed == 'nfta') {
        ret=(!empty_source && !empty_goal);
        msg=msg2;
    }
    if(document.pressed == 'nfta_bup') {
        ret=(!empty_source);
        msg=msg1;
    }

    if(document.pressed == 'dot') {
	set_target();
    }
    
    if(!ret) alert(msg)
    return ret;
}
