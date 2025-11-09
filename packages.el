;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package:
;;
;;   1. Declare them here in a `package!' statement,
;;   2. Run 'doom sync' in the shell,
;;   3. Restart Emacs.
;;
;; Use 'C-h f package\!' to look up documentation for the `package!' macro.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(package! symbol-overlay)

;; (package! verb)

;; (package! aws-mode
;;   :recipe (:host github
;;            :repo "snowiow/aws.el"
;;            :files ("*.el")))

;; (package! ssh-config-mode)

(package! good-scroll)

;; (package! ekg)

(package! ace-window)

(package! aidermacs)

(package! moldable-emacs
  :recipe (:host github
           ;; :repo "ag91/moldable-emacs"
           :repo "Awannaphasch2016/moldable-emacs"
           :branch "pipe"
           )
  ;; :pin "c806cb8"
  )

(package! eaf
  :recipe (:local-repo "~/downloads/emacs-application-framework"))

(package! justl :recipe (:host github :repo "psibi/justl.el"))

;; (package! detached)

;; (package! org-download)

;; (package! consult-gh
;;   :recipe (:host github :repo "armindarvish/consult-gh" :branch "main"))
;; (package! consult-gh-embark
;;   :recipe (:host github :repo "armindarvish/consult-gh" :branch "main"))
;; (package! consult-gh-forge
;;   :recipe (:host github :repo "armindarvish/consult-gh" :branch "main"))

;; (package! code-compass)

;; (package! grid-table
;;   :recipe (:host github :repo "yibie/grid-table" :branch "main"))

(package! popper)

(package! superchat
  :recipe (:host github :repo "yibie/superchat" :branch "main" )
  ;; :pin "257f614"
  )

(package! context-navigator
  :recipe (:host github :repo "11111000000/context-navigator"))

;; (package! atlas
;;   :recipe (:host github :repo "11111000000/atlas"))

(package! efrit
  :recipe (:host github :repo "steveyegge/efrit"))

(package! shell-maker)

(package! acp
  :recipe (:host github :repo "xenodium/acp.el"))

(package! agent-shell
  :recipe (:host github :repo "xenodium/agent-shell"))

(package! command-log-mode)

;; (package! gptel-mcp
;;   :recipe (:host github :repo "lizqwerscott/gptel-mcp.el"))

(package! mcp)

(package! pyvenv)

(package! emacs-piper
  :recipe (:host gitlab :repo "howardabrams/emacs-piper"))



(package! emacs-with-nyxt
  :recipe (:host github :repo "ag91/emacs-with-nyxt"))
