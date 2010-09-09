/*! \file view-filter.cxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#include "view-filter.hxx"

ViewFilter::ViewFilter ()
{
    this->_logFileDesc = 0;
}

ViewFilter::~ViewFilter ()
{
    if (this->_logFileDesc != 0)
	delete this->_logFileDesc;
}

bool ViewFilter::Init (const char *logDescFileName)
{
   LogFileDesc *lfd = LoadLogDesc (logDescFileName);

    if (lfd == 0)
	return false;

    this->_logFileDesc = lfd;

    return true;

}
