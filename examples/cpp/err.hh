
#ifndef __ERR_HH__
#define __ERR_HH__

#include "slang.h"
#include "compat.hh"
#include "cdecl.hh"
#include "../common.h"

BEGIN_C_DECL
typedef void (*ErrorHook) (char *msg);

inline static void recover(char *error_msg)
{
   cerr << error_msg << endl;
   SLang_restart(0);
   SLang_set_error(SL_USAGE_ERROR);
}
END_C_DECL

class ErrorHandler
{
	ErrorHook prev;

   public:

	ErrorHandler()			{ toggle_hook(); }
	~ErrorHandler()			{ SLang_Error_Hook = prev; }

	void toggle_hook(void)	{

	   if (SLang_Error_Hook == recover)
		SLang_Error_Hook = prev;
	   else {
		prev = SLang_Error_Hook;
		SLang_Error_Hook = recover;
	   }

	}
};

#endif
