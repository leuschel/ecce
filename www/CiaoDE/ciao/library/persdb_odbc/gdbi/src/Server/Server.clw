; CLW file contains information for the MFC ClassWizard

[General Info]
Version=1
LastClass=CServerDoc
LastTemplate=CDialog
NewFileInclude1=#include "stdafx.h"
NewFileInclude2=#include "Server.h"
LastPage=0

ClassCount=7
Class1=CServerApp
Class2=CServerDoc
Class3=CServerView
Class4=CMainFrame

ResourceCount=4
Resource1=IDD_ABOUTBOX
Resource2=IDD_LOGIN_DIALOG
Resource3=IDR_SERVERTYPE
Class5=CAboutDlg
Class6=CChildFrame
Class7=CLoginDlg
Resource4=IDR_MAINFRAME

[CLS:CServerApp]
Type=0
HeaderFile=Server.h
ImplementationFile=Server.cpp
Filter=N
BaseClass=CWinApp
VirtualFilter=AC
LastObject=CServerApp

[CLS:CServerDoc]
Type=0
HeaderFile=ServerDoc.h
ImplementationFile=ServerDoc.cpp
Filter=N
LastObject=CServerDoc
BaseClass=CDocument
VirtualFilter=DC

[CLS:CServerView]
Type=0
HeaderFile=ServerView.h
ImplementationFile=ServerView.cpp
Filter=C

[CLS:CMainFrame]
Type=0
HeaderFile=MainFrm.h
ImplementationFile=MainFrm.cpp
Filter=T


[CLS:CChildFrame]
Type=0
HeaderFile=ChildFrm.h
ImplementationFile=ChildFrm.cpp
Filter=M

[CLS:CAboutDlg]
Type=0
HeaderFile=Server.cpp
ImplementationFile=Server.cpp
Filter=D

[DLG:IDD_ABOUTBOX]
Type=1
Class=CAboutDlg
ControlCount=4
Control1=IDC_STATIC,static,1342177283
Control2=IDC_STATIC,static,1342308480
Control3=IDC_STATIC,static,1342308352
Control4=IDOK,button,1342373889

[MNU:IDR_MAINFRAME]
Type=1
Class=CMainFrame
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_MRU_FILE1
Command4=ID_APP_EXIT
Command5=ID_VIEW_TOOLBAR
Command6=ID_VIEW_STATUS_BAR
Command7=ID_APP_ABOUT
CommandCount=7

[TB:IDR_MAINFRAME]
Type=1
Class=CMainFrame
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_CUT
Command5=ID_EDIT_COPY
Command6=ID_EDIT_PASTE
Command7=ID_FILE_PRINT
Command8=ID_APP_ABOUT
CommandCount=8

[MNU:IDR_SERVERTYPE]
Type=1
Class=CServerView
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_CLOSE
Command4=ID_FILE_SAVE
Command5=ID_FILE_SAVE_AS
Command6=ID_FILE_MRU_FILE1
Command7=ID_APP_EXIT
Command8=ID_EDIT_UNDO
Command9=ID_EDIT_CUT
Command10=ID_EDIT_COPY
Command11=ID_EDIT_PASTE
Command12=ID_VIEW_TOOLBAR
Command13=ID_VIEW_STATUS_BAR
Command14=ID_WINDOW_NEW
Command15=ID_WINDOW_CASCADE
Command16=ID_WINDOW_TILE_HORZ
Command17=ID_WINDOW_ARRANGE
Command18=ID_APP_ABOUT
CommandCount=18

[ACL:IDR_MAINFRAME]
Type=1
Class=CMainFrame
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_UNDO
Command5=ID_EDIT_CUT
Command6=ID_EDIT_COPY
Command7=ID_EDIT_PASTE
Command8=ID_EDIT_UNDO
Command9=ID_EDIT_CUT
Command10=ID_EDIT_COPY
Command11=ID_EDIT_PASTE
Command12=ID_NEXT_PANE
Command13=ID_PREV_PANE
CommandCount=13

[DLG:IDD_LOGIN_DIALOG]
Type=1
Class=CLoginDlg
ControlCount=9
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_STATIC,static,1342308352
Control4=IDC_DSN,edit,1350631552
Control5=IDC_STATIC,static,1342308352
Control6=IDC_LOGIN,edit,1350631552
Control7=IDC_STATIC,static,1342308352
Control8=IDC_PASSWORD,edit,1350631584
Control9=IDC_CHECK_SAVEVALUES,button,1342242819

[CLS:CLoginDlg]
Type=0
HeaderFile=LoginDlg.h
ImplementationFile=LoginDlg.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC
LastObject=CLoginDlg

