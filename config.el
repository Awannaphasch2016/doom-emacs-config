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
Defaults to project 'rag-chatbot-worktree' and config 'dev_personal'."
  (interactive
   (list
    (read-string "Doppler project: " "rag-chatbot-worktree")
    (read-string "Doppler config: " "dev_personal")))
  (let* ((project (or project "rag-chatbot-worktree"))
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
         (p (read-string "Doppler project: " "rag-chatbot-worktree"))
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

(defun anak/doppler-get-secret (name project config &optional copy)
  "Get a Doppler secret NAME for PROJECT/CONFIG.
If COPY is non-nil (or user answers yes), copy the raw secret value to the kill-ring
and remove the buffer to avoid leaving the secret visible."
  (interactive
   (let* ((n (read-string "Secret name: "))
          (p (read-string "Doppler project: " "rag-chatbot-worktree"))
          (c (read-string "Doppler config: " "dev_personal"))
          (use-copy (y-or-n-p "Copy secret value to kill-ring (will remove buffer)? ")))
     (list n p c use-copy)))
  (let* ((name (anak/doppler--sanitize-secret-name name))
         (buf (get-buffer-create "*Doppler Get Secret*"))
         ;; prefer --raw to get the unprocessed value; --copy would let doppler copy to clipboard
         (args (list "secrets" "get" name "--copy" "--raw" "--project" project "--config" config)))
    (with-current-buffer buf (erase-buffer))
    (let ((exit (apply #'call-process "doppler" nil buf nil args)))
      (if (= exit 0)
          (let ((val (string-trim (with-current-buffer buf (buffer-string)))))
            (if copy
                (progn
                  ;; erase or kill buffer so secret isn't left on screen
                  (when (buffer-live-p buf) (kill-buffer buf))
                  (message "✅ Secret %s copied to kill-ring (buffer removed)" name))
              (progn
                (display-buffer buf)
                (message "✅ Secret %s retrieved (visible in %s)" name (buffer-name buf)))))
        (progn
          (display-buffer buf)
          (message "❌ Doppler get failed (exit %d) — see buffer %s" exit (buffer-name buf)))))))


(defun anak/doppler-delete-secret (name project config &optional assume-yes)
  "Delete a Doppler secret NAME in PROJECT/CONFIG.
By default prompts for confirmation. If ASSUME-YES is non-nil (or user answers yes
to the first prompt), the function will pass --yes to Doppler to proceed non-interactively."
  (interactive
   (let* ((n (read-string "Secret name to delete: "))
          (p (read-string "Doppler project: " "rag-chatbot-worktree"))
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

(defvar anak/gptel-current-envvar nil
  "Env var name holding the API key for the currently active provider.")

(defun anak/gptel-api-key ()
  "Return the API key string for the active provider, or nil."
  (when anak/gptel-current-envvar
    (getenv anak/gptel-current-envvar)))

;;; --- Provider registry: OpenAI + Anthropic ---------------------------------

(defvar anak/gptel-providers
  `((openai
     :env "OPENAI_API_KEY"
     :backend (lambda ()
                (gptel-make-openai
                 "openai"
                 :protocol "https"
                 :host "api.openai.com"
                 :endpoint "/v1/chat/completions"
                 :models '(gpt-4o gpt-4o-mini)
                 :key #'anak/gptel-api-key
                 :stream t
                 :curl-args '("--retry" "2" "--max-time" "60")
                 :request-params '(:temperature 0.7 :top_p 1.0))))
    (anthropic
     :env "ANTHROPIC_API_KEY"
     :backend (lambda ()
                (gptel-make-anthropic
                 "anthropic"
                 :key #'anak/gptel-api-key
                 :models '(claude-3-5-sonnet claude-3-5-haiku)
                 :stream t
                 :curl-args '("--retry" "2" "--max-time" "60")
                 :request-params '(:temperature 0.7 :top_p 1.0)))))
  "Registry of GPTel providers (OpenAI and Anthropic only).
Each entry maps PROVIDER -> plist with :env and :backend.
:backend must return a fully constructed `gptel-backend' struct.")

(defun anak/gptel--provider-names ()
  (mapcar #'symbol-name (mapcar #'car anak/gptel-providers)))

;;; --- Provider switcher ------------------------------------------------------

(defun anak/gptel-set-provider (provider)
  "Activate GPTel PROVIDER (symbol). Uses env var per `anak/gptel-providers'."
  (interactive
   (list (intern (completing-read "Provider: " (anak/gptel--provider-names) nil t))))
  (let* ((spec (alist-get provider anak/gptel-providers)))
    (unless spec (user-error "Unknown provider: %s" provider))
    (setq anak/gptel-current-envvar (plist-get spec :env))
    (setq gptel-backend (funcall (plist-get spec :backend)))
    (setq gptel-api-key #'anak/gptel-api-key)
    (message "gptel -> provider: %s (env: %s)"
             provider (or anak/gptel-current-envvar "none")))
  gptel-backend)

(anak/gptel-set-provider 'openai)


;; (defun anak/gptel-patch-key! (&optional backend key-fn)
;;   "Destructively set :key slot on BACKEND (default `gptel-backend').
;; Returns the new key function."
;;   (let* ((be (or backend gptel-backend)))
;;     (unless (and be (gptel-backend-p be))
;;       (user-error "Not a gptel-backend: %S" be))
;;     (setf (gptel-backend-key be) (or key-fn #'anak/gptel-api-key))
;;     (message "gptel: patched key slot -> %S" (gptel-backend-key be))
;;     (gptel-backend-key be)))

;; (anak/gptel-patch-key!)

(setq org-agenda-files
      '("~/org/todo/work.org"
        "~/org/todo/personal.org"))

(use-package! symbol-overlay
  :config
  (setq symbol-overlay-idle-time 0.2)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))

(defun anak/set-project-root-as-default-directory ()
  "Ensure current buffer's default-directory is the project root for Python tools."
  (when (and (derived-mode-p 'python-mode)
             (fboundp 'projectile-project-root))
    (setq default-directory (projectile-project-root))))

(add-hook 'python-mode-hook #'anak/set-project-root-as-default-directory)
(add-hook 'python-mode-hook #'lsp-deferred)  ;; or #'lsp-deferred

(defun anak/neotree-toggle-and-find ()
  "Toggle NeoTree and reveal the current file."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (neotree-dir project-dir)
        (neotree-show))
      (when file-name
        (neotree-find file-name)))))

(use-package! neotree
  :commands (neotree-toggle neotree-show neotree-hide neotree-dir neotree-find)
  :init
  ;; General behavior settings
  (setq neo-smart-open t
        neo-window-fixed-size t)

  :config
  ;; Key remapping (runs after neotree is loaded)
  (map! :leader
        :desc "NeoTree open" "o p" #'anak/neotree-toggle-and-find
        :desc "NeoTree find this file" "o P" nil))

(use-package! good-scroll
  :hook (doom-first-input . good-scroll-mode)
  :config
  ;; Set scroll speed and behavior
  (setq good-scroll-step 4
        good-scroll-duration 0.15)

  ;; Optional: make PageUp/PageDown smooth
  (map! :n "C-u" #'good-scroll-down-full-screen
        :n "C-d" #'good-scroll-up-full-screen))

(use-package ekg
  :init
  (require 'ekg-embedding)
  (ekg-embedding-generate-on-save)
  (require 'ekg-llm)
  (require 'llm-openai)  ;; The specific provider you are using must be loaded.
  (let ((my-provider (make-llm-openai :key "my-openai-api-key")))
    (setq ekg-llm-provider my-provider
          ekg-embedding-provider my-provider
          ekg-db-file "~/database/ekg.sqlite")))

(use-package! ace-window)

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
