#define let(var)                  lang.let_(Loc(__FILE__,__LINE__, 0), #var
#define be                        ,
#define in                        ,

#define lambda(param)             lang.lambda_(Loc(__FILE__,__LINE__, 0), #param,

#define if                        lang.if_(Loc(__FILE__, __LINE__, 0),
#define then                      ,
#define else                      ,

#define end                       )

#define var(v)                    lang.var_(#v)
#define vec(...)                  lang.vec_(Loc(__FILE__,__LINE__, 0), __VA_ARGS__)
#define app(f, ...)               lang.app_(Loc(__FILE__,__LINE__, 0), lang.var_(#f), __VA_ARGS__)
