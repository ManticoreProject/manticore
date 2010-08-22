
typedef struct {
	Value_t	proxyObj;	// proxy object in the global heap
	Value_t	localObj;	// local-heap object that the proxy represents.
} ProxyTblEntry_t;


extern int createProxy (VProc_t *vp, Value_t fls);
extern void isProxy (VProc_t *vp,int zahl);
extern void deleteProxy (VProc_t *vp,int zahl);
extern int promotedProxy (VProc_t *vp,int zahl);
extern void setPromote (VProc_t *vp, int zahl);