#i docbook_man.tm

#d arg#1 <arg>$1</arg>\__newline__
#d exe \command{slirpsh}
#d slang \literal{S-Lang}
#d slirp \literal{SLIRP}

\manpage{\exe}{1}{S-Lang shell endowed with SLIRP annotation capabilities}

\mansynopsis{\exe}{
   \arg{OPTIONS}
}

\refsect1{DESCRIPTION}
  \p
  This program should not be explicitly invoked by users.  Instead, it
  should be accessed implicitly from within the slirp wrapper script.
  \p
  \exe is \slang shell endowed with a custom file loader that gives it
  the ability to recognize \slirp annotations.  \slirp is the (SL)ang
  (I)nterface (P)ackage, a code generator aimed primarily at simplifying
  the process of creating modules for the \slang scripting language.
  \p
  \slirp can dramatically reduce the time and effort required to make
  C, C++, or Fortran code callable directly from the \slang interpreter,
  and will even generate Makefiles to automate the build process.
\refsect1-end

#d man_options_entry#2 \varlistentry{\term{$1}}{\p $2 \p-end}\__newline__

\refsect1{OPTIONS}
#v+
  As noted above, this program should not be explicitly invoked by users.
#v-
  \variablelist
	\man_options_entry{\option{--help}}{
		Print this help
	}
	\man_options_entry{\option{--version}}{
		Show version information
	}
	\man_options_entry{\option{-g}}{
		Compile with debugging code, tracebacks, etc
	}
	\man_options_entry{\option{-n}}{
		Don't load personal init file
	}
	\man_options_entry{\option{-i}}{
		Use this initialization file instead of ~/.slshrc
	}
	\man_options_entry{\option{-v}}{
		Show verbose loading messages
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
  License can be found in \filename{/usr/share/common-licenses/GPL}
\p-end
\refsect1-end

\refsect1{SEE ALSO}
   \p
   On Debian systems the reference manual for SLIRP can be found at
   /usr/share/doc/slang-slirp/manual.txt.gz.   It is also available
   in PDF, HTML, and text forms on the SLIRP website.
\refsect1-end

\manpage-end
