
typedef struct {
	Value_t	proxyObj;	// proxy object in the global heap
	Value_t	localObj;	// local-heap object that the proxy represents.
} ProxyTblEntry_t;

extern void createList (VProc_t *vp);