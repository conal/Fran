// Global variables have (1) initialization, (2) getter, (3) setter
// GSL

#ifndef _GLOBALVAR_H_
#define _GLOBALVAR_H_



#define declare_global(type, name)                     \
                                                      \
  EXT_API(type) get_##name(void);                     \
													  \
  EXT_API(void) set_##name(type);



#define define_global(type, name, init)                 \
                                                      \
  type name = init;                                   \
													  \
  EXT_API(type) get_##name()                          \
  { return name; }                                    \
													  \
  EXT_API(void) set_##name(type newVal)               \
  { name = newVal; }


#endif
