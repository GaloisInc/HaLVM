--  Derived from the following stuff 2007-04-27.

--  popen-like library
--
--  Author : Jens-Ulrik Petersen
--  Created: 16 August 2001
--
--  Version:  $Revision: 1.5 $ from $Date: 2001/10/17 07:30:53 $
--
--  Copyright (c) 2001 Jens-Ulrik Holger Petersen
--  (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- Description
--

-- This code is based on runProcess from the hslibs posix
-- library, but internally it uses file descriptors instead
-- of handles and returns the output and error streams
-- lazily as strings as well as the pid of forked process,
-- instead of just IO (). 

module Testing.POpen (popen)
where

import System.Posix
import Directory
import IO (isEOFError, hGetContents, Handle, hPutStr, hClose)
import Maybe (fromJust, isJust)
import Monad (when)

popen :: FilePath			-- Command
      -> [String]			-- Arguments
      -> Maybe [(String, String)]	-- Environment
 	-- (stdin, stdout, stderr, pid)
      -> IO (Handle, Handle, Handle, ProcessID)
popen path args env =
    do
    (inr, inw) <- createPipe
    (outr, outw) <- createPipe
    (errr, errw) <- createPipe
    pid <- forkProcess (doTheBusiness inr outw errw)
    closeFd inr
    closeFd outw
    closeFd errw
    hin <- fdToHandle inw
    hout <- fdToHandle outr
    herr <- fdToHandle errr
    return (hin, hout, herr, pid)
    where
    doTheBusiness :: 
	   Fd	            -- stdin
	-> Fd		    -- stdout
	-> Fd		    -- stderr
        -> IO ()
    doTheBusiness inr outw errw = 
	do
        dupTo inr stdInput
	dupTo outw stdOutput
	dupTo errw stdError
	executeFile path True args env
