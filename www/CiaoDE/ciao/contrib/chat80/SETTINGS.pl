%% ===========================================================================
%% 
%% Installation configuration settings
%% 
%% Set to appropriate values before installing the system.
%% The defaults listed are suggestions and/or the ones used for local 
%% installation in the CLIP group machines.
%% ===========================================================================
:- module(_,_,[functions]).
% :- use_module(library(metaterms)). % terms_vars or terms_check
%% ===========================================================================
%% Directory where the executable should be installed
%% 
bin := '/home/clip/public_html/Software'.
%% ---------------------------------------------------------------------------
%% Name of application
%% 
basemain := webchat.
%% ---------------------------------------------------------------------------
%% Define this to be the permissions for installed execs/dirs and data files
%% perm(User,Group,Others)
%% 
execpermissions(perm(rwx,rwx,rx)).
datapermissions(perm(rw, rw, r )).
%% ---------------------------------------------------------------------------
ciaoc('../../ciaoc/ciaoc').
