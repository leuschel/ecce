
lisp(0,"
(REQUEST
 :reply-With  testsuite1045749040601
 :laNguaGe  FIPA-SL0
 :ontology  FIPA-Agent-Management
 :protocol  FIPA-Request
 :conversation-id  tester261045749039858@paris.agentcities.net1045749040601
)
").
lisp(1,"
(request
 :sender  ( agent-identifier  :name tester261045749039858@paris.agentcities.net :addresses (sequence http://leap.crm-paris.com:5194/leap ))
 :receiver  (set ( agent-identifier  :name df@ACIAO :addresses (sequence http://clip.dia.fi.upm.es:32323/acc )) )
)
").
lisp(2,"
(request
 :content  ""(( action ( agent-identifier  :name df@ACIAO :addresses (sequence http://clip.dia.fi.upm.es:32323/acc )) ( register (df-agent-description :name ( agent-identifier  :name tester261045749039858@paris.agentcities.net :addresses (sequence http://leap.crm-paris.com:5194/leap )) :protocol (set FIPA-Request FIPA-Query) :ontology (set FIPA-Agent-Management JADE-Agent-Management) :language (set FIPA-SL0 FIPA-XML) :services (set (service-description :name DF-TEST-1 :type DF-TEST-SERVICE-1 :protocol (set FIPA-Contract-Net) :ontology (set Test-Suite-Dummy) :language (set FIPA-KML) :ownership LEAP-TEST-SUITE-1 :properties (set (property :name prop1 :value val1) (property :name prop2 :value val2))))) ) ))"" 
)
").
lisp(3,"
(request
 :sender  ( agent-identifier  :name tester261045749039858@paris.agentcities.net :addresses (sequence http://leap.crm-paris.com:5194/leap ))
 :receiver  (set ( agent-identifier  :name df@ACIAO :addresses (sequence http://clip.dia.fi.upm.es:32323/acc )) )
 :content  ""(( action ( agent-identifier  :name df@ACIAO :addresses (sequence http://clip.dia.fi.upm.es:32323/acc )) ( register (df-agent-description :name ( agent-identifier  :name tester261045749039858@paris.agentcities.net :addresses (sequence http://leap.crm-paris.com:5194/leap )) :protocol (set FIPA-Request FIPA-Query) :ontology (set FIPA-Agent-Management JADE-Agent-Management) :language (set FIPA-SL0 FIPA-XML) :services (set (service-description :name DF-TEST-1 :type DF-TEST-SERVICE-1 :protocol (set FIPA-Contract-Net) :ontology (set Test-Suite-Dummy) :language (set FIPA-KML) :ownership LEAP-TEST-SUITE-1 :properties (set (property :name prop1 :value val1) (property :name prop2 :value val2))))) ) ))"" 
 :reply-with  testsuite1045749040601
 :language  FIPA-SL0
 :ontology  FIPA-Agent-Management
 :protocol  FIPA-Request
 :conversation-id  tester261045749039858@paris.agentcities.net1045749040601
)
").
lisp(4,"
(define (Linguistic-Variable :id Altura)
   (name 'Altura)
   (universe 'Persona)
   (feature 'Talla)
   (range (define (range) (min 100) (max 200)))
   (linguistic-labels Muy_Bajo Bajo Mediano Alto Muy_Alto)
   (sets Alto Bajo)
)
").
lisp(5,"
(request
   :encoding ( 20030102T0505050 
	       ( ""string"" pepe ) paco ( ) 20021212T10101010 pis 
               pun ( 2003 01 02 T 05 05 05 05 ) plas
                     20030102T05050507A )
)
").
lisp(6,"
(request
   :reply-by 20021212T10101010
   :reply-by 20021212T10101010x
   :reply-by 20021212T101010108
)
").



xml(0,"
<?xml version=""1.0""?>
<fipa-message act=""REQUEST""
	conversation-id = ""juanito"">
(REQUEST
 :reply-With  testsuite1045749040601
 :laNguaGe  FIPA-SL0
 :ontology  FIPA-Agent-Management
 :protocol  FIPA-Request
 :conversation-id  tester261045749039858@paris.agentcities.net1045749040601
)
").
xml(1,"
<?xml version=""1.0""?>
<fipa-message act=""REQUEST""
	conversation-id = ""pepito"">
 <sender>
     <agent-identifier>  
         <name>
	 tester261045749039858@paris.agentcities.net 
         </name>
	 <addresses>
	     <url href=""http://leap.crm-paris.com:5194/leap"" />
	 </addresses>
     </agent-identifier>  
 </sender>
 <receiver>
     <agent-identifier>
         <name>
	 df@ACIAO 
         </name>
	 <addresses>
	     <url href = ""http://clip.dia.fi.upm.es:32323/acc"" />
	 </addresses>
     </agent-identifier>  
 </receiver>
</fipa-message>
").
