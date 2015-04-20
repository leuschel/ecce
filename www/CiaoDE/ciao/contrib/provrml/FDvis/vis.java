import java.applet.*;
import java.awt.*;
import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.*;

import iicm.*;
import iicm.vrml.*;
import iicm.utils3d.*;
import iicm.vrml.pw.*;
import iicm.vrml.pw.Node;
import iicm.vrml.vrwave.*;
//import vrml.external.Node;
import vrml.external.field.*;
import vrml.external.exception.*;
import vrml.external.Browser;

/** Main class */
public class vis extends Applet implements Runnable {
  /** Constants */
  int NumberOfScenes =  10;
  int NumberOfFrames =  10;

  /** VRML stuff */
  Scene           scene[]      = new Scene[NumberOfScenes];        
  SceneFrame      sceneFrame[] = new SceneFrame[NumberOfFrames];
  Browser         browser;
  GroupNode       groupNode[]  = new GroupNode[NumberOfScenes*NumberOfFrames];

  /** Indexers and limits */
  int             frameIndex    = 0;
  int             sceneIndex[]  = new int[NumberOfScenes];
  int             actualScene[] = new int[NumberOfScenes];
  int             MaxFrames     = NumberOfFrames;
  int             MaxScenes     = NumberOfScenes;
  int             codeIndex     = 0;
  private  int    index;

  /** Code storage */
  String          codeStore[];

  /** Some AWT junk for layout purposes */
  Panel          tridPanel    = new Panel();
  Panel          commandPanel = new Panel(); 
 
  /** Network stuff */
  Socket          vrmlSocket;    
  DataInputStream networkIStream;
  PrintStream     networkOStream;
  DataInputStream in;            
  Socket          incoming;
  URL             appURL;
  Thread          networkThread;
  

  /** The initialisation of the class */
  public void init() {
    //If we name it Quit it will kill the applet directly.
    Button quitButton = new Button( "Exit" ); 
    commandPanel.add( quitButton );

    for( int i = 0; i < MaxScenes; i++ )
      scene[i] = new Scene( this );

    for( int i = 0; i < MaxScenes * MaxFrames ; i++ )
      groupNode[i] = new GroupNode();
    
    //sceneFrame = new SceneFrame( "", scene, true );
    this.show(false);
    try
      {
	browser = new Browser();
      }
      catch (Throwable e) { }
  
    createControls();
    
    networkThread = new Thread( this, "networkThread" );
    networkThread.start();
  }
  
  /** Create controls and set the layout for the applet */
  public void createControls() {
    removeAll(); 
    // Create the layout for the applet
    GridBagLayout gridbag          = new GridBagLayout();
    GridBagConstraints constraints = new GridBagConstraints();
    tridPanel.resize( 410, 310 );
        
    // Set the layout for the applet 
    setLayout( gridbag );
    constraints.gridx = 0;
    constraints.gridy = 0;
    gridbag.setConstraints( tridPanel, constraints );
    add( tridPanel );
     this.show(false);
    //Restrict the shape
    constraints.gridx     = GridBagConstraints.REMAINDER;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    constraints.gridy     = GridBagConstraints.RELATIVE;
    constraints.fill      = GridBagConstraints.HORIZONTAL;
    gridbag.setConstraints( commandPanel, constraints );

    add( commandPanel );
    repaint();
  }
   
  /** Reading from a file and creating a VRML view of the code read. */
  public void createDocumentFile( String FileName ) {
    try{
      appURL         = new URL(getDocumentBase(), FileName );
      VRMLparser vrp = new VRMLparser( appURL.openStream() );
      GroupNode root = vrp.readStream();
	  	    
      if (root != null && frameIndex < MaxFrames )
	{
	  int index = getNodeIndex( frameIndex, sceneIndex[frameIndex] );
	  groupNode[ index ] =  root;
		 
	  scene[ frameIndex ].replaceScene( root );
	  sceneFrame[frameIndex] = 
	          new SceneFrame(FileName,scene[frameIndex],true);
	  scene[frameIndex].redraw();
	  sceneIndex[frameIndex]++;
	  actualScene[frameIndex] = sceneIndex[frameIndex];
	  frameIndex++;
	  
	  repaint();
	}
    }
    catch (MalformedURLException e) {
      System.out.println ("Unable to load: " + e);
      return;
    }
    catch (IOException e) {
      System.out.println ("Unable to load: " + e);
      return;
    }
  }
  
  /** Creates a new empty window with the name set to the number of the frame
   *
   **/
  public void createNewFrame() {
    if( frameIndex < MaxFrames )
      {
	sceneFrame[ frameIndex] = new SceneFrame( 
				  Integer.toString( frameIndex ), null, true );
	frameIndex++;
      }
  }
  
  /** Create a documnet with the code and display it in a new window. */
  public void createWindowCode( String code )
    { 
      if( frameIndex < MaxFrames )
	{
	  StringBufferInputStream hsb = new StringBufferInputStream( code );
	  VRMLparser vrp = new VRMLparser( hsb );
	  GroupNode root = vrp.readStream();
	  Node  noden = Node.readNode(vrp);
	  //Traverser  traverser = new Traverser();
	  noden.writeNode(System.out,noden.subfields);
	  

	  if (root != null)
	    {
	      
	      int index = getNodeIndex( frameIndex, sceneIndex[frameIndex] );
	      groupNode[ index ] =  root;
	      
	      sceneFrame[ frameIndex ] = 
		new SceneFrame( Integer.toString( frameIndex ), 
				scene[ frameIndex ], true );
	      
	      scene[frameIndex].replaceScene( root );
	      scene[frameIndex].redraw();
	      sceneIndex[frameIndex]++;
	      actualScene[frameIndex] = sceneIndex[frameIndex];
	      frameIndex++;
	     
	    }
	  else
	    {
	      System.out.println ("error on parsing " );
	      if (vrp.getVersion () == 0.0f)
		System.out.println ("bad header");
	    } 
	}
      else
	System.out.println( "There are to many windows loaded already" );
    }

  /** Create a document with the code and display it in an old window. */
  public void createDocumentCode( int num, String code )
    {
      //System.out.println( "The code we want to see "+code);
      StringBufferInputStream hsb = new StringBufferInputStream( code );
      VRMLparser vrp = new VRMLparser( hsb );
      GroupNode root = vrp.readStream();
      //System.out.println( "FrameNumber: "+Integer.toString( num ) );
      
      if ( root != null && num < frameIndex )
	{
	  int index = getNodeIndex( num, sceneIndex[num] );
	  groupNode[ index ] =  root;
	  scene[ num ].replaceScene(root);
	  scene[ num ].redraw();
	  sceneIndex[num]++;
	  actualScene[num] = sceneIndex[num];
	}
      else
	{
	  System.out.println ("error on parsing:" +code);
	  if (vrp.getVersion () == 0.0f)
	    System.out.println ("bad header");
	} 
    }

  /** Rotating to a new scenery that we have since before. */
  public void rotateScene( int frameNum, int sceneNum )
    {
      if ( sceneNum < sceneIndex[ frameNum ] )
	{
	  GroupNode root = groupNode[ getNodeIndex( frameNum, sceneNum ) ];
	  //root.writeNodes( System.out );
	  scene[ frameNum ].replaceScene( root );
	  scene[ frameNum ].redraw();
	  actualScene[frameNum] = sceneNum;
	}
      else
	{
	  System.out.println ("Picture does not exist");
	} 
    }
  
  /** Rotating back to a scenery that we have since before. */
  public void rotateSceneBack( int frameNum  )
    {
      int sceneNum = actualScene[frameNum] - 1;
      
      if ( sceneNum < 0 )
	sceneNum = sceneIndex[frameNum];
      System.out.println("hopp till scene :"+Integer.toString(sceneNum));
      GroupNode root = groupNode[ getNodeIndex( frameNum, sceneNum ) ];
      //root.writeNodes( System.out );
      scene[ frameNum ].replaceScene( root );
      scene[ frameNum ].redraw();
      actualScene[frameNum] = sceneNum;
    }
  
  /** Rotating forward to a scenery that we have since before. */
  public void rotateSceneForth( int frameNum  )
    {
      int sceneNum = actualScene[frameNum] + 1;
      
      if ( sceneNum > sceneIndex[ frameNum ] )
	sceneNum = 0; //Start from the beginning
      System.out.println("hopp till scene :"+Integer.toString(sceneNum));
     
      GroupNode root = groupNode[ getNodeIndex( frameNum, sceneNum ) ];
      //root.writeNodes( System.out );
      scene[ frameNum ].replaceScene( root );
      scene[ frameNum ].redraw();
      actualScene[frameNum] = sceneNum;
	
    }  

  /** Removes a frame with all its contents */
  public void disposeFrame( int Num ) {
    if( Num < frameIndex )
      {
	sceneFrame[ Num ].dispose();
	deleteFrameScene(Num);
      }
    else
      System.out.println( "Accessed frame does not exist" );
  }
  
  /* Deletes the scenes from a frame */
  private void deleteFrameScene( int Num ) {
    for(int i = 0; i < MaxScenes; i++ )
      {
	int place = frameSceneIndex(Num,i);
	scene[place] = null;
      }
  }	 

  /* Returns the actual place for the scene. */
  private int frameSceneIndex(int Frame, int Scene) {
    return Frame * MaxFrames + Scene;
  }
  
  /* Returns the actual place for the scene. */
  private int getNodeIndex(int Frame, int Scene) {
    return Frame * MaxFrames + Scene;
  }
    

  public void run() {

    connect();     
    String numberString = new String();
    String code         = new String();;

    try {
      boolean done = false;
      
      while ( !done ) {
	String str = in.readLine( );
	System.out.println("Stingen som vi kollar in "+str);
	if (str == null) 
	  {
	    done = true;
	  }
	else
	  {  
	    //We exit the system.
	    if(str.trim().equals("exit"))
	      {
		done = true;
		System.out.println("CLOSE THE CONNECTION");
	      }
	    else
	      //We delete the frame that comes as an argument.
	      if(str.trim().startsWith("close"))
		{
		  numberString = in.readLine( );
		  int num      = Integer.parseInt( numberString );
		  disposeFrame(num);
		}
	    else
	      //We creates a new window and display code from a file.
	      if(str.trim().startsWith("file")) {
		
		//System.out.println( "The file is comming" );
		String fileName = new String(str.substring(
							   str.indexOf( "file" ) + 4 ) );
		createDocumentFile( fileName );
	      }
	    else
	      //We read the word 'code'  and opens a new window with the 
	      // code. 
	      if(str.trim().startsWith("code")) {
		System.out.println( "code" );
		code = new String(str.substring(
						str.indexOf( "code" ) + 4 ) );
		
		code = getTheRestOfInput( code );
		createWindowCode( code );
		System.out.println("Creating a new window");
	      } 
	      else
		//We want to  change the code in one window
		if(str.trim().startsWith("change")) {
		  System.out.println( "change" );
		  numberString = in.readLine( );
		  int num      = Integer.parseInt( numberString );
		  System.out.println( "Changing in window nr: "+ numberString );
		  code = new String();;
		  code = getTheRestOfInput( code );
		  createDocumentCode( num, code );
		} 
		else
		  //We want to take a look at  an old picture.
		  if(str.trim().startsWith("rotate_forth")) {  
		    numberString = in.readLine( );
		    System.out.println( 
				       "Rotating forward in window nr: "+ numberString );
		    int num      = Integer.parseInt( numberString );	
		    rotateSceneForth( num );
		  } 
		  else
		    //We want to take a look at  an old picture.
		    if(str.trim().startsWith("rotate_back")) {
		      numberString = in.readLine( );
		      System.out.println("Rotating back in window nr: "+ numberString );
		      int num      = Integer.parseInt( numberString );
		      rotateSceneBack( num );
		    } 
		    else
		      //We want to take a look at  an old picture.
		      if(str.trim().startsWith("rotate")) {
			numberString = in.readLine( );
			System.out.println( "Rotating in window nr: "+ numberString );
			int num      = Integer.parseInt( numberString );
			numberString = in.readLine( );
			System.out.println( "To picture nr: "+ numberString );
			int sceneNum = Integer.parseInt( numberString );
			rotateScene( num, sceneNum );
		      } 
		      else
			//We creates a new empty browser.
			if( str.trim().startsWith("new")) {
			  createNewFrame();
			}
	  }
	
      }
    }
    catch ( IOException e ) {
      System.err.println( "IOException caught: " + e.toString() );
    }
    disconnect();
  }
  
  private String getTheRestOfInput( String s ) {
    try 
      {
	String str  = in.readLine( );
	while( !str.trim().startsWith("@END@")  )
	  {
	    s  = s.concat( str + "\n" );
	    str = in.readLine( );
	  }
      }          
    catch ( IOException e ) {
      System.err.println( "IOException caught: " + e.toString() );
    }
    return s;
  }
  

  public void disconnect() { 
    try {
      networkThread.sleep( 1000 );
      disposeFrame( 0 );
      System.exit( 0 );
      System.exit( -1 );
      this.destroy();
    } 
    catch ( InterruptedException e ) {
      System.out.println( "InterruptedException: " + e.toString() );
    }
    catch( SecurityException e ) {
      System.out.println( "SecurityException: " + e.toString() );
    }
  }

  public void connect() {
    try 
      {  
	//System.out.println("###########################################");
	//System.out.println("############ SOCKET IS CREATED ############");
	ServerSocket serverSocket = new ServerSocket(0); //Any free port
        InetAddress inetAddress = serverSocket.getInetAddress();
        //System.out.println( "Address " + inetAddress.toString()  );
	String   hostAddress  = inetAddress.getHostName();
	
	int port =  serverSocket.getLocalPort();
	//System.out.println("##### CONNECT YOURSELF TO PORT: "+port +" #####");
	System.out.println( "@PORT@"+port );
	incoming = serverSocket.accept( );
	//System.out.println("############# CONNECTED  ##################");
	
	in = new DataInputStream(incoming.getInputStream());
      } 
    catch (Exception e) 
      { 
	System.out.println(e);
      }  
    
  }
  
    /** Event processor */
  public boolean handleEvent( Event evt ) {
    switch ( evt.id ) {
    case Event.ACTION_EVENT: {
      if( evt.arg.toString().equals( "Exit" ) ) {
	System.out.println( "Exit" );
	disconnect();
	return true;
      }
      break;
    }
    }
    return false;
  }
  
}
  /////// END OF CODE 
