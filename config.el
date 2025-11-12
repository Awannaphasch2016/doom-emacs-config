;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :v "v" #'er/expand-region)

(defun anak/load-doppler-env (&optional project config)
  "Load Doppler environment variables for PROJECT and CONFIG into the current Emacs session.
Defaults to project 'anak' and config 'dev_personal'."
  (interactive
   (list
    (read-string "Doppler project: " "anak")
    (read-string "Doppler config: " "dev_personal")))
  (let* ((project (or project "anak"))
         (config  (or config "dev_personal"))
         (doppler-cmd (format "doppler run --project %s --config %s --command env"
                              project config)))
    (with-temp-buffer
      (if (zerop (call-process-shell-command doppler-cmd nil (current-buffer)))
          (progn
            (goto-char (point-min))
            (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
              (let ((var (match-string 1))
                    (val (match-string 2)))
                (setenv var val)))
            (message "✅ Doppler environment loaded for project: %s, config: %s"
                     project config))
        (message "❌ Failed to load Doppler environment for project: %s, config: %s"
                 project config)))))

(anak/load-doppler-env "rag-chatbot-worktree" "dev_personal")
(anak/load-doppler-env)

(defun anak/doppler--sanitize-secret-name (name)
  "Return NAME converted to Doppler-safe secret name (A-Z, 0-9 and underscore).
Replaces any disallowed character with underscore and upcases the result."
  (let* ((up (upcase (string-trim name)))
         (safe (replace-regexp-in-string "[^A-Z0-9_]" "_" up)))
    safe))

(defun anak/doppler-set-secret (name value project config)
  "Interactively set a Doppler secret.
Prompts for NAME and VALUE (value is hidden). Defaults PROJECT/CONFIG provided.
Sanitizes NAME to Doppler rules (only uppercase letters, digits and underscores)."
  (interactive
   (let ((n (read-string "Secret name: "))
         (v (read-passwd "Secret value (hidden): "))
         (p (read-string "Doppler project: " "anak"))
         (c (read-string "Doppler config: " "dev_personal")))
     (list n v p c)))
  (let* ((safe-name (anak/doppler--sanitize-secret-name name)))
    (when (not (string= name safe-name))
      (if (y-or-n-p (format "Secret name %S is invalid for Doppler. Use sanitized name %S instead? " name safe-name))
          (setq name safe-name)
        (setq name (read-string "Enter a Doppler-safe secret name (A-Z0-9_):")))))
  (let* ((buffer (get-buffer-create "*Doppler Set Secret*"))
         (args (list "secrets" "set" name value "--project" project "--config" config)))
    (with-current-buffer buffer (erase-buffer))
    (let ((exit-code (apply #'call-process "doppler" nil buffer nil args)))
      (if (= exit-code 0)
          (progn
            (message "✅ Doppler secret %s set successfully" name)
            (display-buffer buffer))
        (progn
          (message "❌ Doppler failed (exit %d) — see buffer %s" exit-code (buffer-name buffer))
          (display-buffer buffer))))))

;; Requires your existing anak/doppler--sanitize-secret-name
(defun anak/doppler-get-secret (name project config)
  "Get a Doppler secret NAME for PROJECT/CONFIG."
  (interactive
   (let* ((n (read-string "Secret name: "))
          (p (read-string "Doppler project: " "anak"))
          (c (read-string "Doppler config: " "dev_personal"))
          (use-copy (y-or-n-p "Copy secret value to kill-ring (will remove buffer)? ")))
     (list n p c use-copy)))
  (let* ((name (anak/doppler--sanitize-secret-name name))
         (buf (get-buffer-create "*Doppler Get Secret*"))
         ;; prefer --raw to get the unprocessed value; --copy would let doppler copy to clipboard
         (args (list "secrets" "get" name "--copy" "--raw" "--project" project "--config" config)))
    (let ((exit (apply #'call-process "doppler" nil nil nil args))
          (env (current-kill 0)))
      (if (= exit 0)
          (progn
            (message "✅ Secret %s retrieved (visible in %s)" name (buffer-name buf))
            env)
        (message "❌ Doppler get failed (exit %d) — see buffer %s" exit (buffer-name buf))))))


(defun anak/doppler-delete-secret (name project config &optional assume-yes)
  "Delete a Doppler secret NAME in PROJECT/CONFIG.
By default prompts for confirmation. If ASSUME-YES is non-nil (or user answers yes
to the first prompt), the function will pass --yes to Doppler to proceed non-interactively."
  (interactive
   (let* ((n (read-string "Secret name to delete: "))
          (p (read-string "Doppler project: " "anak"))
          (c (read-string "Doppler config: " "dev_personal"))
          (y (y-or-n-p (format "Are you sure you want to delete %s from %s:%s? " n p c))))
     (list n p c y)))
  (let* ((name (anak/doppler--sanitize-secret-name name))
         (buf (get-buffer-create "*Doppler Delete Secret*"))
         (args (append (list "secrets" "delete" name "--project" project "--config" config)
                       (when assume-yes (list "--yes")))))
    (with-current-buffer buf (erase-buffer))
    (let ((exit (apply #'call-process "doppler" nil buf nil args)))
      (if (= exit 0)
          (progn
            (message "✅ Secret %s deleted from %s:%s" name project config)
            (display-buffer buf))
        (progn
          (display-buffer buf)
          (message "❌ Doppler delete failed (exit %d) — see buffer %s" exit (buffer-name buf)))))))

;; (defvar anak/gptel-current-envvar nil
;;   "Env var name holding the API key for the currently active provider.")

;; (defun anak/gptel-api-key ()
;;   "Return the API key string for the active provider, or nil."
;;   (when anak/gptel-current-envvar
;;     (getenv anak/gptel-current-envvar)))

;; ;;; --- Provider registry: OpenAI + Anthropic ---------------------------------

;; (defvar anak/gptel-providers
;;   `((openai
;;      :env "OPENAI_API_KEY"
;;      :backend (lambda ()
;;                 (gptel-make-openai
;;                  "openai"
;;                  :protocol "https"
;;                  :host "api.openai.com"
;;                  :endpoint "/v1/chat/completions"
;;                  :models '(gpt-4o gpt-4o-mini)
;;                  :key #'anak/gptel-api-key
;;                  :stream t
;;                  :curl-args '("--retry" "2" "--max-time" "60")
;;                  :request-params '(:temperature 0.7 :top_p 1.0))))
;;     (anthropic
;;      :env "ANTHROPIC_API_KEY"
;;      :backend (lambda ()
;;                 (gptel-make-anthropic
;;                  "anthropic"
;;                  :key #'anak/gptel-api-key
;;                  :models '(claude-3-5-sonnet claude-3-5-haiku)
;;                  :stream t
;;                  :curl-args '("--retry" "2" "--max-time" "60")
;;                  :request-params '(:temperature 0.7 :top_p 1.0)))))
;;   "Registry of GPTel providers (OpenAI and Anthropic only).
;; Each entry maps PROVIDER -> plist with :env and :backend.
;; :backend must return a fully constructed `gptel-backend' struct.")

;; (defun anak/gptel--provider-names ()
;;   (mapcar #'symbol-name (mapcar #'car anak/gptel-providers)))

;; ;;; --- Provider switcher ------------------------------------------------------

;; (defun anak/gptel-set-provider (provider)
;;   "Activate GPTel PROVIDER (symbol). Uses env var per `anak/gptel-providers'."
;;   (interactive
;;    (list (intern (completing-read "Provider: " (anak/gptel--provider-names) nil t))))
;;   (let* ((spec (alist-get provider anak/gptel-providers)))
;;     (unless spec (user-error "Unknown provider: %s" provider))
;;     (setq anak/gptel-current-envvar (plist-get spec :env))
;;     (setq gptel-backend (funcall (plist-get spec :backend)))
;;     (setq gptel-api-key #'anak/gptel-api-key)
;;     (message "gptel -> provider: %s (env: %s)"
;;              provider (or anak/gptel-current-envvar "none")))
;;   gptel-backend)

;; (anak/gptel-set-provider 'openai)
;; ;; (anak/gptel-set-provider 'open-router)
;; ;; (anak/gptel-set-provider 'anthropic)
;; (setq gptel-log-level 'debug)
;; (setq gptel-default-mode 'org-mode)


;; ;; (defun anak/gptel-patch-key! (&optional backend key-fn)
;; ;;   "Destructively set :key slot on BACKEND (default `gptel-backend').
;; ;; Returns the new key function."
;; ;;   (let* ((be (or backend gptel-backend)))
;; ;;     (unless (and be (gptel-backend-p be))
;; ;;       (user-error "Not a gptel-backend: %S" be))
;; ;;     (setf (gptel-backend-key be) (or key-fn #'anak/gptel-api-key))
;; ;;     (message "gptel: patched key slot -> %S" (gptel-backend-key be))
;; ;;     (gptel-backend-key be)))

;; ;; (anak/gptel-patch-key!)


(after! gptel
  (setq gptel-log-level 'debug)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
        (gptel-make-openai "OpenRouter"               ;Any name you want
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (auth-source-pick-first-password :host "openrouter.ai")
          :models '(mistralai/mixtral-8x7b-instruct
                    openai/gpt-3.5-turbo
                    meta-llama/codellama-34b-instruct
                    codellama/codellama-70b-instruct
                    google/palm-2-codechat-bison-32k
                    google/gemini-pro
                    qwen/qwen3-30b-a3b))
        gptel-model   'openai/gpt-3.5-turbo))

(setq org-agenda-files
      '("~/org/todo/work.org"
        "~/org/todo/personal.org"))

(use-package! symbol-overlay
  :config
  (setq symbol-overlay-idle-time 0.2)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))

;; (defun anak/set-project-root-as-default-directory ()
;;   "Ensure current buffer's default-directory is the project root for Python tools."
;;   (when (and (derived-mode-p 'python-mode)
;;              (fboundp 'projectile-project-root))
;;     (setq default-directory (projectile-project-root))))

;; (add-hook 'python-mode-hook #'anak/set-project-root-as-default-directory)

;; (use-package tree-sitter
;;   :config
;;   ;; Python configuration
;;   (setq treesit-language-source-alist
;;         '((python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))))
;; (after! tree-sitter
;;   (unless (treesit-ready-p 'python)
;;     (treesit-install-language-grammar 'python)))

;; (use-package! python
;;   :config
;;   (setq python-indent-guess-indent-offset-verbose nil)

;;   ;; Use tree-sitter mode if available
;;   (when (treesit-ready-p 'python)
;;     (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))))

;; (defun anak/neotree-toggle-and-find ()
;;   "Toggle NeoTree and reveal the current file."
;;   (interactive)
;;   (if (neo-global--window-exists-p)
;;       (neotree-hide)
;;     (let ((project-dir (projectile-project-root))
;;           (file-name (buffer-file-name)))
;;       (if project-dir
;;           (neotree-dir project-dir)
;;         (neotree-show))
;;       (when file-name
;;         (neotree-find file-name)))))

;; (use-package! neotree
;;   :commands (neotree-toggle neotree-show neotree-hide neotree-dir neotree-find)
;;   :init
;;   ;; General behavior settings
;;   (setq neo-smart-open t
;;         neo-window-fixed-size t)

;;   :config
;;   ;; Key remapping (runs after neotree is loaded)
;;   (map! :leader
;;         :desc "NeoTree open" "o n" #'anak/neotree-toggle-and-find
;;         :desc "NeoTree find this file" "o N" nil))

(use-package! good-scroll
  :hook (doom-first-input . good-scroll-mode)
  :config
  ;; Set scroll speed and behavior
  (setq good-scroll-step 4
        good-scroll-duration 0.15)

  ;; Optional: make PageUp/PageDown smooth
  (map! :n "C-u" #'good-scroll-down-full-screen
        :n "C-d" #'good-scroll-up-full-screen))

;; (use-package! ekg
;;   :init
;;   (require 'ekg-embedding)
;;   (ekg-embedding-generate-on-save)
;;   (require 'ekg-llm)
;;   (require 'llm-openai)  ;; The specific provider you are using must be loaded.
;;   (let ((my-provider (make-llm-openai :key "my-openai-api-key")))
;;     (setq ekg-llm-provider my-provider
;;           ekg-embedding-provider my-provider
;;           ekg-db-file "~/database/ekg.sqlite")))


(defun anak/store-last-window (&rest _args)
  "Store current window before ace-window switch."
  (setq anak/last-window (selected-window)))

(defun anak/jump-to-last-window ()
  "Jump to the last selected window."
  (interactive)
  (if (and anak/last-window (window-live-p anak/last-window))
      (select-window anak/last-window)
    (message "No previous window stored.")))

(advice-add 'ace-window :before #'anak/store-last-window)

(advice-add 'ace-window :before #'anak/store-last-window)
(map! :leader
      :desc "Jump to last window"
      "w r" #'anak/jump-to-last-window)

(after! embark
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map (kbd "o") (my/embark-ace-action find-file)))

(defun my/embark-vsplit-file (file)
  "Split window horizontally, open FILE in new window, and move cursor there."
  (interactive "fFile: ")
  (split-window-horizontally)
  (other-window 1)
  (find-file file))

(after! embark
  (define-key embark-file-map (kbd "v") #'my/embark-vsplit-file))

(use-package! aidermacs
  :config
  (map! :n "C-c a" #'aidermacs-transient-menu)
  (setq aidermacs-program "aider"))

;; (after! aidermacs
;;   (setq aidermacs-program "aider"))

;; can't get it to work correctly
;; (setq! emacs-everywhere-app-info-function  #'emacs-everywhere--app-info-linux-x11)

(use-package! moldable-emacs
  :load-path  "~/.config/emacs/.local/straight/repos/moldable-emacs/"
  ;; (add-to-list 'load-path "~/downloads/moldable-emacs/")
  ;; (add-to-list 'load-path "~/.config/emacs/.local/straight/repos/moldable-emacs/")
  ;; (add-to-list 'me-files-with-molds "~/downloads/moldable-emacs/molds/core.el")
  ;; (add-to-list 'me-files-with-molds "~/downloads/moldable-emacs/molds/contrib.el")
  :config
  (require 'moldable-emacs)
  (me-setup-molds))

(use-package! eaf
  :load-path "~/downloads/emacs-application-framework"
  :init
  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-evil)
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  ;; (setq! eaf-dired-advisor-enable nil)
  ;; (setq! eaf-find-file-advisor-enable nil)
  (defalias 'browse-web #'eaf-open-browser)
  (define-key key-translation-map (kbd "SPC")
              (lambda (prompt)
                (if (derived-mode-p 'eaf-mode)
                    (pcase eaf--buffer-app-name
                      ("browser" (if  eaf-buffer-input-focus
                                     (kbd "SPC")
                                   (kbd eaf-evil-leader-key)))
                      ("pdf-viewer" (kbd eaf-evil-leader-key))

                      ("image-viewer" (kbd eaf-evil-leader-key))
                      (_  (kbd "SPC")))
                  (kbd "SPC"))))
  )

(use-package! justl
  :config
  ;; Normal buffer (recipes)
  (evil-define-key 'normal justl-mode-map
    (kbd "g r") #'justl--refresh-buffer
    (kbd "e")   #'justl-exec-recipe
    (kbd "E")   #'justl-exec-shell
    (kbd "?")   #'justl-help-popup
    (kbd "h")   #'justl-help-popup
    (kbd "w")   #'justl--exec-recipe-with-args
    (kbd "W")   #'justl-no-exec-shell
    (kbd "m")   #'justl--show-modules
    (kbd "RET") #'justl-go-to-recipe)

  ;; Module buffer
  (evil-define-key 'normal justl-module-mode-map
    (kbd "g r") #'justl--module-refresh-buffer
    (kbd "e")   #'justl-exec-module
    (kbd "?")   #'justl-module-help-popup
    (kbd "h")   #'justl-module-help-popup
    (kbd "o")   #'justl--module-open-justl
    (kbd "RET") #'justl--go-to-module)

  (map! :map justl-mode
        :leader "p j" #'justl))


;; (use-package! detached
;;   :init
;;   (detached-init)
;;   :bind (;; Replace `async-shell-command' with `detached-shell-command'
;;          ([remap async-shell-command] . detached-shell-command)
;;          ;; Replace `compile' with `detached-compile'
;;          ([remap compile] . detached-compile)
;;          ([remap recompile] . detached-compile-recompile)
;;          ;; Replace built in completion of sessions with `consult'
;;          ([remap detached-open-session] . detached-consult-session))
;;   :custom ((detached-show-output-on-attach t)
;;            (detached-terminal-data-command system-type)))

;; (after! detached
;;   (evil-define-key 'normal detached-list-mode-map
;;     "a" #'detached-edit-session-annotation
;;     "d" #'detached-list-delete-session
;;     "e" #'detached-edit-and-run-session
;;     "f" #'detached-list-select-filter
;;     "g" #'detached-list-revert
;;     "i" #'detached-list-initialize-session-directory
;;     "j" nil
;;     "k" nil
;;     "J" #'imenu
;;     "K" #'detached-list-kill-session
;;     "m" #'detached-list-mark-session
;;     ;; Narrow
;;     "n a" #'detached-list-narrow-annotation
;;     "n c" #'detached-list-narrow-command
;;     "n d" #'detached-list-narrow-session-directory
;;     ;; Host
;;     "n h h" #'detached-list-narrow-host
;;     "n h c" #'detached-list-narrow-currenthost
;;     "n h l" #'detached-list-narrow-localhost
;;     "n h r" #'detached-list-narrow-remotehost
;;     "n o" #'detached-list-narrow-output
;;     "n O" #'detached-list-narrow-origin
;;     ;; State
;;     "n s a" #'detached-list-narrow-active


;;     "n s i" #'detached-list-narrow-inactive
;;     "n s s" #'detached-list-narrow-success
;;     "n u" #'detached-list-narrow-unique
;;     "n w" #'detached-list-narrow-working-directory
;;     "n +" #'detached-list-narrow-after-time
;;     "n -" #'detached-list-narrow-before-time
;;     "q" #'detached-list-quit
;;     "r" #'detached-rerun-session
;;     "t" #'detached-list-toggle-mark-session
;;     "T" #'detached-list-toggle-sessions
;;     "u" #'detached-list-unmark-session
;;     "U" #'detached-list-unmark-sessions
;;     "v" #'detached-list-view-session
;;     "w" #'detached-copy-session-command
;;     "W" #'detached-copy-session-output
;;     "x" #'detached-list-detach-from-session
;;     "%" #'detached-list-mark-regexp
;;     "=" #'detached-list-diff-marked-sessions
;;     "-" #'detached-list-widen
;;     "!" #'detached-shell-command
;;     ;; Describe
;;     ". s" #'detached-describe-session
;;     ". d" #'detached-describe-duration
;;     (kbd "<backspace>") #'detached-list-remove-narrow-criterion
;;     (kbd "<return>") #'detached-list-open-session)

;;   (evil-define-key 'normal vterm-mode-map  (kbd "C-<return>") #'detached-vterm-attach))

;; (use-package! org-download
;;   :after org
;;   :config
;;   (setq org-download-image-dir "~/images"
;;         org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"))

;; (map! :map org-mode-map
;;       :leader
;;       :desc "Paste image from clipboard"
;;       "i p" #'org-download-clipboard)


;; (defun anak/paste-image-anywhere ()
;;   "Paste image from clipboard into current buffer, saving it to ~/Pictures/emacs_paste/."
;;   (interactive)
;;   (let* ((default-directory (expand-file-name "~/Pictures/emacs_paste/"))
;;          (_ (unless (file-directory-p default-directory)
;;               (make-directory default-directory t)))
;;          (filename (format-time-string "image_%Y%m%d_%H%M%S.png"))
;;          (filepath (expand-file-name filename default-directory))
;;          (cmd (cond
;;                ;; Linux / Wayland / X11
;;                ((executable-find "xclip")
;;                 (format "xclip -selection clipboard -t image/png -o > %s" (shell-quote-argument filepath)))
;;                ;; macOS
;;                ((executable-find "pngpaste")
;;                 (format "pngpaste %s" (shell-quote-argument filepath)))
;;                ;; Windows PowerShell
;;                (t
;;                 (format "powershell -command \"Add-Type -AssemblyName System.Windows.Forms; "
;;                         "[Windows.Forms.Clipboard]::GetImage().Save('%s','Png')\"" filepath)))))
;;     (shell-command cmd)
;;     (if (file-exists-p filepath)
;;         (progn
;;           (insert (format "![%s](%s)" filename filepath))
;;           (when (image-type-from-file-name filepath)
;;             (let ((img (create-image filepath)))
;;               (when img
;;                 (let ((ov (make-overlay (point) (point))))
;;                   (overlay-put ov 'display img)
;;                   (overlay-put ov 'face 'default))))))
;;       (message "No image data found in clipboard!"))))

;; (global-set-key (kbd "C-S-v") #'anak/paste-image-anywhere)

(defun my-list-apt-installed-packages ()
  "Open a buffer listing all apt-installed packages."
  (interactive)
  (let ((buf (get-buffer-create "*apt-installed-pkgs*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (shell-command-to-string "apt list --installed"))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;; (use-package! consult-gh
;;   :after consult
;;   :custom
;;   (consult-gh-default-clone-directory "~/Downloads")
;;   (consult-gh-show-preview t)
;;   (consult-gh-preview-key "C-o")
;;   (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
;;   (consult-gh-large-file-warning-threshold 2500000)
;;   (consult-gh-confirm-name-before-fork nil)
;;   (consult-gh-confirm-before-clone t)
;;   (consult-gh-notifications-show-unread-only nil)
;;   (consult-gh-default-interactive-command #'consult-gh-transient)
;;   (consult-gh-prioritize-local-folder nil)
;;   (consult-gh-group-dashboard-by :reason)
;;   ;;;; Optional
;;   (consult-gh-repo-preview-major-mode nil) ; show readmes in their original format
;;   (consult-gh-preview-major-mode 'org-mode) ; use 'org-mode for editing comments, commit messages, ...

;;   :config

;;   (require 'consult-gh-transient)
;;   (require 'consult-gh-embark)
;;   (require 'consult-gh-forge)
;;   (consult-gh-embark-mode +1)
;;   (consult-gh-forge-mode +1)
;;   ;; Remember visited orgs and repos across sessions
;;   (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
;;   (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
;;   ;; Enable default keybindings (e.g. for commenting on issues, prs, ...)
;;   (consult-gh-enable-default-keybindings))

;; (use-package! consult-gh-embark
;;   :after consult-gh
;;   :config
;;   (consult-gh-embark-mode +1))

;; (use-package! consult-gh-forge
;;   :after consult-gh
;;   :config
;;   (consult-gh-forge-mode +1)
;;   (setq consult-gh-forge-timeout-seconds 20))

;; (use-package! consult-gh-embark
;;   :after consult-gh
;;   :config
;;   (consult-gh-embark-mode +1))

;; (use-package! consult-gh-forge
;;   :after consult-gh
;;   :config
;;   (consult-gh-forge-mode +1)
;;   (setq consult-gh-forge-timeout-seconds 20))

;; (use-package! consult-omni
;;   :after consult)

;; (use-package! code-compass
;;   :config
;;   (code-compass-install))

;; (use-package! grid-table
;;   :config
;;   (require 'grid-table)
;;   (require 'grid-table-plugins)
;;   (setq grid-table-default-save-directory "~/Documents/grid-table/")
;;   (setq grid-table-image-target-char-height 8)
;;   (setq grid-table-image-max-width-ratio 0.9))

(use-package! popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  (setq! popper-group-function #'popper-group-by-projectile))

(use-package! superchat
  :after gptel mcp
  :config
  (setq superchat-response-timeout 10))

(use-package! context-navigator
  :custom
  (context-navigator-autoload t)
  (context-navigator-autosave t)
  :config
  (context-navigator-mode 1)
  (map! :leader
        :desc "context-navigator menu"
        "o l n" #'context-navigator-view-open-menu))

(use-package! avy
  :config
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

                                        ;Replace your package manager or preferred dict package
  (defun dictionary-search-dwim (&optional arg)
    "Search for definition of word at point. If region is active,
    search for contents of region instead. If called with a prefix
    argument, query for word to search."
    (interactive "P")
    (if arg
        (dictionary-search nil)
      (if (use-region-p)
          (dictionary-search (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
        (if (thing-at-point 'word)
            (dictionary-lookup-definition)
          (dictionary-search-dwim '(4))))))

  (defun avy-action-define (pt)
    (save-excursion
      (goto-char pt)
      (dictionary-search-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?Z  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?= avy-dispatch-alist) 'dictionary-search-dwim
        (alist-get ?H avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)
  )

;; (use-package atlas
;;   :commands (atlas-index atlas-entity-tree atlas-progress-mode)
;;   :init
;;   (require 'atlas-autoloads))

(use-package! efrit
  :config
  (setq efrit-data-directory "~/elfrit-data")
  (setq! efrit-model "claude-sonnet-4-5-20250929"))

(use-package! shell-maker
  :config
  (require 'shell-maker))

(use-package! agent-shell
  :after auth-source
  :config
  (require 'agent-shell)
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))
  ;; (setq agent-shell-openai-authentication
  ;;       (agent-shell-openai-make-authentication :api-key (auth-source-pick-first-password :host "openai.com")))
  )


(defun anak/duplicate-workspace ()
  "Duplicate current Doom workspace (tab) layout and buffers, prompting for name."
  (interactive)
  (require 'persp-mode)
  (require 'subr-x) ;; for string-empty-p, string-trim
  (let* ((current (persp-name (get-current-persp)))
         (entered (string-trim
                   (read-string (format "Name for duplicate of \"%s\": " current))))
         ;; If user leaves it blank or chooses an existing name, auto-unique it.
         (final-name (let ((n (if (string-empty-p entered) nil entered)))
                       (if (member n (persp-names))
                           (generate-new-buffer-name n)
                         n))))
    (persp-add-new final-name)
    (persp-copy current final-name)
    (persp-switch final-name)
    (message "Duplicated workspace: %s -> %s" current final-name)))
(map! :leader
      :desc "Duplicate current workspace"
      "TAB R" #'anak/duplicate-workspace)


(defcustom context-navigator-global-key "C-c n"
  "Global key sequence for opening the Context Navigator transient.
Default is \"C-c n\"."
  :type '(choice (const :tag "None" nil) (string :tag "Key sequence"))
  :group 'context-navigator
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'context-navigator--update-global-keybinding)
           (ignore-errors (context-navigator--update-global-keybinding)))))

(map! :leader
      "o l n" #'context-navigator-view-open-menu)

;; (map! :mode context-navigator-view-mode
;;       :n
;;       "H"  #'context-navigator-groups-split-toggle
;;       "q" #'context-navigator-view-quit
;;       "D" #'context-navigator-view-delete-dispatch
;;       "M" #'context-navigator-view-toggle-dispatch
;;       "L" #'context-navigator-view-activate)

;; (map! :mode context-navigator-groups-split-mode
;;       :n
;;       "l" #'context-navigator-groups-split-select
;;       "SPC" #'nil)

(add-hook 'context-navigator-view-mode-hook
          (lambda ()

            (evil-local-set-key 'normal (kbd "n") #'context-navigator-toggle)
            (evil-local-set-key 'normal (kbd "m") #'context-navigator-view-toggle-dispatch)
            (evil-local-set-key 'normal (kbd "h") #'context-navigator-groups-split-toggle)
            (evil-local-set-key 'normal (kbd "q") #'context-navigator-view-quit)
            (evil-local-set-key 'normal (kbd "d") #'context-navigator-view-delete-dispatch)
            (evil-local-set-key 'normal (kbd "?") #'context-navigator-view-open-menu)
            (evil-local-set-key 'normal (kbd "p") #'context-navigator-view-push-now)
            (evil-local-set-key 'normal (kbd "u") #'context-navigator-undo)
            (evil-local-set-key 'normal (kbd "C-r") #'context-navigator-redo)
            (evil-local-set-key 'normal (kbd "s") #'context-navigator-view-filter-by-names)
            (evil-local-set-key 'normal (kbd "f") #'context-navigator-view-filter-by-content )
            (evil-local-set-key 'normal (kbd "F") #'context-navigator-view-filter-clear)

            (evil-local-set-key 'normal (kbd "l") #'context-navigator-groups-split-select )
            (evil-local-set-key 'normal (kbd "m") #'context-navigator-view-group-toggle-select )
            (evil-local-set-key 'normal (kbd "a") #'context-navigator-view-group-create )
            (evil-local-set-key 'normal (kbd "R") #'context-navigator-view-group-rename )
            (evil-local-set-key 'normal (kbd "E") #'context-navigator-view-group-edit-description )
            (evil-local-set-key 'normal (kbd "C") #'context-navigator-view-group-duplicate )
            (evil-local-set-key 'normal (kbd "D") #'context-navigator-view-delete-dispatch )
            ))


(add-hook 'context-navigator-groups-split-mode-hook
          (lambda ()
            (define-key context-navigator-groups-split-mode-map (kbd "SPC") nil)))

(defun anak/gh-create-repo-and-push ()
  "Create a private GitHub repo using the current directory name.
Automatically detects if commits exist and conditionally adds --push flag.
If not a git repo, initializes one first."
  (interactive)
  (let* ((default-directory (if (magit-toplevel)
                                (magit-toplevel)
                              default-directory))
         (dir-name (file-name-nondirectory (directory-file-name default-directory)))
         (is-git-repo (file-exists-p (expand-file-name ".git" default-directory)))
         (has-commits nil)
         (push-flag ""))

    ;; Initialize git repo if needed
    (unless is-git-repo
      (message "Not a git repository. Running git init...")
      (shell-command-to-string "git init"))

    ;; Check if commits exist
    (let ((commit-count-output (shell-command-to-string "git rev-list --count HEAD 2>/dev/null")))
      (setq has-commits (and (string-match "^[0-9]+$" (string-trim commit-count-output))
                             (> (string-to-number (string-trim commit-count-output)) 0))))

    ;; Set push flag if commits exist
    (setq push-flag (if has-commits "--push" ""))

    ;; Build and run gh command
    (let* ((cmd (format "gh repo create %s --private --source=. --remote=origin %s"
                        (shell-quote-argument dir-name)
                        push-flag))
           (output (progn
                     (message "Creating GitHub repo: %s%s"
                              dir-name
                              (if has-commits " (with push)" ""))
                     (shell-command-to-string cmd))))

      ;; Check result
      (if (string-match-p "error\\|failed" output)
          (message "❌ Failed to create repo: %s" output)
        (progn
          (message "✅ Successfully created repo: %s%s"
                   dir-name
                   (if has-commits " and pushed commits" ""))
          (when (fboundp 'magit-refresh)
            (magit-refresh)))))))

;; Magit Ediff keybindings under SPC g >
(map! :leader
      (:prefix ("g" . "git")
       :desc "Create GitHub repo & push" "^" #'anak/gh-create-repo-and-push
       (:prefix (">" . "ediff")
        :desc "Ediff dwim"           ">"  #'magit-ediff-dwim
        :desc "Ediff compare"        "c"  #'magit-ediff-compare
        :desc "Ediff commit"         "m"  #'magit-ediff-show-commit
        :desc "Ediff staged"         "s"  #'magit-ediff-show-staged
        :desc "Ediff unstaged"       "u"  #'magit-ediff-show-unstaged
        :desc "Ediff working tree"   "w"  #'magit-ediff-show-working-tree
        :desc "Ediff stage file"     "S"  #'magit-ediff-stage
        :desc "Ediff stash"          "h"  #'magit-ediff-show-stash
        :desc "Ediff resolve all"    "r"  #'magit-ediff-resolve-all
        :desc "Ediff resolve rest"   "R"  #'magit-ediff-resolve-rest)))

;; (use-package! gptel-mcp
;;   :config (require 'gptel-mcp))

(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx"
                              :args ("-y" "@modelcontextprotocol/server-filesystem")
                              :roots ("/home/anak/tmp/")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
             ;; ("qdrant" . (:url "http://localhost:8000/sse"))
             ("graphlit" . (
                            :command "npx"
                            :args ("-y" "graphlit-mcp-server")
                            :env (
                                  :GRAPHLIT_ORGANIZATION_ID "your-organization-id"
                                  :GRAPHLIT_ENVIRONMENT_ID "your-environment-id"
                                  :GRAPHLIT_JWT_SECRET "your-jwt-secret")))
             ))
  :config
  (require 'mcp-hub)

  (push (gptel-make-tool
         :name "get_weather"
         :description "Get current weather for a city"
         :args '((:name "city"
                  :type string
                  :description "City name"
                  :required t))
         :function (lambda (city)
                     (format "The weather in %s is sunny, 72°F" city)))
        gptel-tools)
  :hook (after-init . mcp-hub-start-all-server))


;; (set-fontset-font t 'thai (font-spec :family "Noto Sans Thai") nil 'prepend)
;; (set-fontset-font t 'thai (font-spec :family "TlwgTypist") nil 'prepend)
(set-fontset-font t 'thai (font-spec :family "Loma") nil 'prepend)
;; (setq use-default-font-for-symbols t)

;; (after! eglot
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;              '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
;;   (setq-default eglot-workspace-configuration
;;                 '(:basedpyright (:venvPath "."
;;                                  ;; :venv ".venv"
;;                                  :analysis (:typeCheckingMode "strict"
;;                                             :diagnosticMode "workspace"
;;                                             :autoImportCompletions t
;;                                             :useLibraryCodeForTypes t))))
;;   )

(use-package! piper
  :load-path "/home/anak/.config/emacs/.local/straight/repos/emacs-piper/"
  :config
  (map! :n "C-c C-|" #'piper))

(use-package! emacs-with-nyxt
  :config
  (setq browse-url-browser-function #'browse-url-nyxt))

;; (use-package! agent-menu
;;   :load-path "/home/anak/dev/my-packages/claude-transcient"
;;   :config
;;   (map! :desc "AI Agents" :n "C-c a" #'agent-menu-smart-launch))

(load! "agent-menu.el" "/home/anak/dev/claude-transcient")
(load! "cursor-transient.el" "/home/anak/dev/claude-transcient")
(load! "claude-transient.el" "/home/anak/dev/claude-transcient")
(map! :desc "AI Agents" :n "C-c a" #'agent-menu-smart-launch)
