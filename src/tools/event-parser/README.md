This is a modified version of GHC Events that adds support for  Manticore events
============================

To install:
 
    git clone https://github.com/ml9951/ghc-events.git
    git checkout manticore-events
    cabal install

You should then be able to build ThreadScope to visualize Manticore events

Note that you will have to install this package prior to installing
ThreadScope so that it uses *this* package instead of downloading the
mainstream version on Hackage

**Adding New Events**

To add a new event, you must modify the following files and functions:

* EventLogFormat.h -- add new #define for each new event
* EventTypes.hs  -- add new type for event
* Events.hs -- modify the following functions
  * showEventInfo
  * eventTypeNum
  * putEventSpec
  * standardParsers
