/* view-filter.hxx
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 */

#ifndef _VIEW_FILTER_HXX_
#define _VIEW_FILTER_HXX_

#include "log-desc.hxx"

class GroupFilter;

class ViewFilter {
  public:

    ViewFilter ();
    ~ViewFilter ();

    bool Init (const char *logDescFileName);

    EventDesc *FindEventById (int id) { return this->_logFileDesc->FindEventById(id); }

  private:
    LogFileDesc		*_logFileDesc;
};

#endif /* !_VIEW_FILTER_HXX_ */
