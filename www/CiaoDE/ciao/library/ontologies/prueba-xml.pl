input(1,'<?xml version="1.0" encoding="iso-8859-1" ?> 
<!DOCTYPE Ontology> 
<!-- 

	====================================================================================
	Automatically generated XML file.
	WebODE 2.0.
	(c) Laboratory of Artificial Intelligence, 2000.  School of Computer Science (FI).
	Technical University of Madrid (UPM).
	====================================================================================



  --> 
<Ontology>
<!-- 
***********************************************
             Ontology Description
***********************************************


  --> 
  <Name>Prueba Sergio</Name> 
  <Description>Es una prueba realizada por Sergio Guadarrama</Description> 
  <Author>sguada</Author> 
  <Creation-Date>2003-02-18</Creation-Date> 
  <Related-Reference>reference</Related-Reference> 
<Conceptualization>
<!-- 
***********************************************
                  References
***********************************************

  --> 
<Reference>
  <Name>reference</Name> 
  </Reference>
<!-- 
***********************************************
                  Concepts
***********************************************

  --> 
<Concept>
  <Name>Base-FSet</Name> 
  <Related-Reference>reference</Related-Reference> 
  </Concept>
<Concept>
  <Name>Fuzzy-Set</Name> 
<Class-Attribute>
  <Name>Linguistic-Variable</Name> 
  <Type>Ontology</Type> 
  <Minimum-Cardinality>1</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  <Related-Reference>reference</Related-Reference> 
  <Inferred>Range</Inferred> 
  </Class-Attribute>
<Class-Attribute>
  <Name>Name</Name> 
  <Type>String</Type> 
  <Minimum-Cardinality>1</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
<Class-Attribute>
  <Name>Range</Name> 
  <Type>Range</Type> 
  <Minimum-Cardinality>1</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
<Class-Attribute>
  <Name>Type</Name> 
  <Type>String</Type> 
  <Minimum-Cardinality>0</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
<Class-Attribute>
  <Name>Values</Name> 
  <Type>Ontology</Type> 
  <Minimum-Cardinality>0</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
<Instance-Attribute>
  <Name>Value</Name> 
  <Type>Integer</Type> 
  <Minimum-Cardinality>1</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  <Minimum-Value>0</Minimum-Value> 
  <Maximum-Value>1</Maximum-Value> 
  </Instance-Attribute>
  </Concept>
<Concept>
  <Name>Linguistic-Variable</Name> 
<Class-Attribute>
  <Name>Name</Name> 
  <Type>String</Type> 
  <Minimum-Cardinality>1</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
<Class-Attribute>
  <Name>Universe</Name> 
  <Type>String</Type> 
  <Minimum-Cardinality>1</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
  <Related-Reference>reference</Related-Reference> 
  </Concept>
<Concept>
  <Name>Mio</Name> 
  </Concept>
<Concept>
  <Name>Origin-FSet</Name> 
<Class-Attribute>
  <Name>Function</Name> 
  <Type>Ontology</Type> 
  <Minimum-Cardinality>0</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
<Class-Attribute>
  <Name>Orden</Name> 
  <Type>String</Type> 
  <Minimum-Cardinality>0</Minimum-Cardinality> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  </Class-Attribute>
  </Concept>
<Concept>
  <Name>suyo</Name> 
  </Concept>
<!-- 
***********************************************
                   Groups
***********************************************

  --> 
<Group>
  <Name>Test</Name> 
  <Related-Concept>Linguistic-Variable</Related-Concept> 
  <Related-Concept>Fuzzy-Set</Related-Concept> 
  </Group>
<!-- 
***********************************************
                  Relationships
***********************************************

  --> 
<Term-Relation>
  <Name>Subclass-of</Name> 
  <Origin>suyo</Origin> 
  <Destination>Mio</Destination> 
  <Maximum-Cardinality>-1</Maximum-Cardinality> 
  </Term-Relation>
<Term-Relation>
  <Name>Subclass-of</Name> 
  <Origin>Origin-FSet</Origin> 
  <Destination>Fuzzy-Set</Destination> 
  <Maximum-Cardinality>-1</Maximum-Cardinality> 
  </Term-Relation>
<Term-Relation>
  <Name>Subclass-of</Name> 
  <Origin>Base-FSet</Origin> 
  <Destination>Origin-FSet</Destination> 
  <Maximum-Cardinality>-1</Maximum-Cardinality> 
  </Term-Relation>
<Term-Relation>
  <Name>Ad</Name> 
  <Origin>Linguistic-Variable</Origin> 
  <Destination>Fuzzy-Set</Destination> 
  <Maximum-Cardinality>1</Maximum-Cardinality> 
  <Related-Property>Reflexive</Related-Property> 
  <Related-Property>Function</Related-Property> 
  </Term-Relation>
<!-- 
***********************************************
                  Properties
***********************************************

  --> 
<Property>
  <Name>Animal</Name> 
  </Property>
<Property>
  <Name>Function</Name> 
  </Property>
  </Conceptualization>
  </Ontology>').
