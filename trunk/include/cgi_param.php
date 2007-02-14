<?php

function cgi_param($field,$default,$max_size,$type)
{
        $value = $default;

        if (isset ($_REQUEST [$field]) && $_REQUEST [$field] != '')
                         $value = $_REQUEST [$field];
        
        
        if (strlen($value)>$max_size)
        {
                ntool_log("cgi_param:string-size: .".$field." ".strlen($value));        
                set_meldung("Eingabeende abgeschnitten");
                $value=substr($value,0,$max_size);
        }

        $regexp=array(
                'alnum' =>  '^[-_a-zA-Z0-9 ]+$',
                'text'  =>  '^.*$',                     //any string
                'hex'   =>  '^[0123456789abcdef]+$',
                'al'    => '^[-a-zA-Z ]+$',
                'num'   => '^[0-9]+$',
                'real'   => '(^[0-9]+.[0-9]$)|(^[0-9]+$)',
                'ml'    => '^([-a-zA-Z0-9_\.]+)@([-a-zA-Z0-9_\.]+)$',
                );
        
        if (!empty($value) and ereg($regexp[$type],$value) == 0)
        {
                ntool_log("cgi_param:regexp:".$field." ".$type);        
//                set_meldung("Falsche Eingabe");
                $value=$default;
        }       
        

//        debug_print ("cgi_param field(".$field.") value(".$value.")");
        return $value;
}
