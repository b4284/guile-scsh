(use-modules (ice-9 format)
	     (ice-9 receive)

	     (srfi srfi-1)
	     (srfi srfi-13)
	     (srfi srfi-14)

	     (scsh alt-syntax)
	     (scsh module-system)
	     (scsh let-opt)
	     (scsh loophole)
	     (scsh signals)
	     (scsh fdports)
	     (scsh syntax-helpers)
	     (scsh bitwise)
	     (scsh utilities)
	     (scsh define-foreign-syntax)
	     (scsh ascii)
	     (scsh features)
	     (scsh primitives)
	     (scsh reading)
	     (scsh jar-defrecord)
	     (scsh cset-obsolete)
	     (scsh char-p)
	     (scsh defrec)
	     (scsh errno)
	     (scsh rw)

	     (scsh rx re-low)
	     (scsh rx cond-package)
	     (scsh rx let-match)
	     (scsh rx re)
	     (scsh rx spencer)
	     (scsh rx simp)
	     (scsh rx posixstr)
	     (scsh rx re-high)
	     (scsh rx oldfuns)
	     (scsh rx re-subst)
	     (scsh rx re-fold)

	     ;; rx macro generates code that requires
	     ;; (scsh rx re)
	     (scsh rx re-syntax)

	     (scsh rx parse)
	     (scsh rx rx-lib)

	     (scsh rdelim)
	     (scsh here)
	     (scsh scsh-version)
	     (scsh weak)
	     (scsh population)

	     ;; also exports signal/alrm, interrupt/alrm etc.
	     (scsh sighandlers)

	     (scsh procobj)

	     (scsh syscalls)

	     (scsh fname)
	     (scsh fluid)
	     (scsh stringcoll)
	     (scsh scsh-condition)
	     
	     ;; define-simple-syntax generates code that requires
	     ;; (scsh alt-syntax).
	     ;; other macros generate code that requires
	     ;; (scsh scsh) and (scsh procobj).
	     (scsh syntax)

	     (scsh fileinfo)
	     (scsh glob)

	     ;; replaces rename-file.
	     (scsh filesys)
	     
	     (scsh time)
	     (scsh newports)

	     (scsh fr)

	     ;; the awk macro generates code that requires:
	     ;; (scsh rx re-syntax)
	     ;; (scsh rx re)
	     ;; (scsh rx re-high)
	     (scsh awk)

	     ;; also exports symbols such as address-family/unspecified.
	     (scsh netconst)

	     (scsh network)
	     (scsh scsh)
)
