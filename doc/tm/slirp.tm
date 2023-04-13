#i docbook_man.tm

#d arg#1 <arg>$1</arg>\__newline__
#d exe \command{slirp}
#d slang S\\[hy]Lang
#d slirp \literal{SLIRP}

\manpage{\exe}{1}{C code generator for the \slang scripting language}

\mansynopsis{\exe}{
   \manarg_rep{OPTIONS}
   \manarg_req{headerfile}
   \manarg_rep{headerfile}
}

\refsect1{DESCRIPTION}
  \p
  \slirp is the (SL)ang (I)nterface (P)ackage, a code generator aimed
  primarily at simplifying the process of creating modules for the \slang
  scripting language.
  \p
  \slirp can dramatically reduce the time and effort required to make
  C, C++, or Fortran code callable directly from the \slang interpreter,
  generate Makefiles to automate the build process, and even parallelize
  wrappers with OpenMP to take advantage of multiprocessors.

  \p
  \slirp may also be used to generate pure C bindings for C++ code, or
  empty (stub) implementations for the interface(s) specified by its input
  header files. The code generated in these cases has no dependencies upon
  \slang whatsoever.
\refsect1-end

#d man_options_entry#2 \varlistentry{\term{$1}}{\p $2 \p-end}\__newline__

\refsect1{OPTIONS}
  \variablelist
	\man_options_entry{\option{\\-c++}}{
	    Mandate that input headers be interpreted as C++, which can
	       be helpful to coerce the interpretation of files
	    that either lack a .hh suffix or do not contain class
	    definitions or other explicit C++ syntax.
	}
	\man_options_entry{\option{\\-cfront}}{
	   Generate standalone C wrappers (.cc and .h files),
           instead of S-Lang bindings, for C++ interface
	}
	\man_options_entry{\option{\\-const_strings}}{
	   Treat 'char*' return values as 'const char*'
	}
	\man_options_entry{\option{\\-d}}{
	   Include slirp_debug_pause() debugging stub within module
	}
	\man_options_entry{\option{\\-fortran_strlen <i>}}{
	   Indicate where hidden string len args are placed in
	   wrappers of Fortran calls accepting CHARACTER args
	   (0 = append length args to end of arg list [default]
	   1 = insert length args directly after CHARACTER arg)
	}
	\man_options_entry{\option{\\-g[=integer]}}{
	   Emit debugging output to terminal, of varying verbosity
	}
	\man_options_entry{\option{\\-fprefix <pref>}}{
	   Emit code only for functions which begin with an
	   exact match to <pref>, instead of default regexp
	}
	\man_options_entry{\option{\\-h \\-help \\-\\-help}}{
	   This message
	}
	\man_options_entry{\option{\\-ignore <option>}}{
	   Tune the emission of ignored symbol messages: use 'notrunc'
	   to append to existing ignored symbol list (default: the
	   ignored symbol list is truncated at startup), or specify an
	   alternate output file name (default: ./ignored.txt)
	}
	\man_options_entry{\option{\\-I  <dir>}}{
	   Add <dir> to search path for headers during code
	   generation; will also be reflected in \\-make output
	}
	\man_options_entry{\option{\\-L  <dir>}}{
	   Add <dir> to search path at link time; for \\-make
	}
	\man_options_entry{\option{\\-l  <lib>}}{
	   Add <lib> to library list at link time; for \\-make
	}
	\man_options_entry{\option{\\-ldflags <flags>}}{
	   Use when \\-L or \\-l are inappropriate; for \\-make
	}
	\man_options_entry{\option{\\-m  <name>}}{
	   Override default module name with <name>
	}
	\man_options_entry{\option{\\-make}}{
	   Generate a best-effort Makefile for compiling module
	   (\\-make is also implied by \\-L, \\-l, and \\-ldflags)
	}
	\man_options_entry{\option{\\-mapnames <R> <S>}}{
	   Synonym for \\-rename (deprecated)
	}
	\man_options_entry{\option{\\-nocom}}{
	   Do not wrap common blocks
	}
	\man_options_entry{\option{\\-noinit}}{
	   Do not generate an initialization fragment
	}
	\man_options_entry{\option{\\-nopop}}{
	   Avoid explicit pop of func arguments, if possible
	   (also turns off generation of Usage: statements)
	}
	\man_options_entry{\option{\\-noautotype}}{
	   Disable mapping of unknown types to opaque ptr types
	}
	\man_options_entry{\option{\\-otree}}{
	   Print hierarchical listing of all opaque types
	   defined by the current invocation, then exit
	}
	\man_options_entry{\option{\\-openmp}}{
	   Emit OpenMP #pragmas to parallelize vectorized code
	   (implies \\-vec, i.e. turns on vectorization)
	}
	\man_options_entry{\option{\\-print}}{
	   Print interface for code which would be generated,
	   but don't actually generate any code
	}
	\man_options_entry{\option{\\-rc  <file>}}{
	   Load customizations from <file>, rather than from
	   first of ./slirprc or $SLIRPRC
	}
	\man_options_entry{\option{\\-refscalars}}{
	   Support passing scalars as array/ref arguments
	   (default: on for Fortran bindings, off for C/C++)
	}
	\man_options_entry{\option{\\-rename  <R> <S>}}{
	   Map C functions with prefixes matching the S-Lang
	   regexp R to S-Lang functions beginning with string S
	   (set S=NULL to map regexp prefix to empty string)
	}
	\man_options_entry{\option{\\-sldb}}{
	   Run in interactive SLDB debugger (\slang 2 only)
	}
	\man_options_entry{\option{\\-stdout}}{
	   Emit code to stdout, instead of files
	}
	\man_options_entry{\option{\\-stubs}}{
	   Generate stub (empty) implementations for input
	   header files, allowing the module interface to be
	   exercised without linking in the underlying library
	   or any of its dependencies
	}
	\man_options_entry{\option{\\-tmapout <file>}}{
	   Save a copy of the active typemap table to <file>
	}
	\man_options_entry{\option{\\-tmapin  <file>}}{
	   evalfile() additional typemaps from <file>
	}
	\man_options_entry{\option{\\-vec}}{
	   Attempt to vectorize every function within input interface
	}
	\man_options_entry{\option{\\-\\-version}}{
	   Output version information
	}
	\man_options_entry{\option{\\-v}}{
	   Show verbose loading messages
	}
	\man_options_entry{\option{\\-vh \\-vhelp, \\-\\-vhelp}}{
	   Verbose help (normal \\-\\-help output, plus rarely used options)
	}
  \variablelist-end
\refsect1-end

\refsect1{AUTHOR}
  \p
  The author of SLIRP is Michael S. Noble <mnoble@space.mit.edu>.
  Rafael Laboissiere <rafael@debian.org> created the SLIRP package
  for Debian.
  \pp
  Permission is granted to copy, distribute and/or modify
  this document under the terms of the GNU General Public License,
  Version 2 any later version published by the Free Software
  Foundation.
  \pp
  On Debian systems, the complete text of the GNU General Public
  License can be found in \filename{/usr/share/common\\-licenses/GPL}
\p-end
\refsect1-end

\refsect1{SEE ALSO}
   \p
   On Debian systems the reference manual for SLIRP can be found at
   /usr/share/doc/slang\\-slirp/manual.txt.gz.   It is also available
   in PDF, HTML, and text forms on the SLIRP website.
\refsect1-end

\manpage-end
