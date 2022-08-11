;; doom emacs
(setq gc-cons-threshold (* 50 1000 1000)
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 2 1000 1000)
          gc-cons-percentage 0.1)))

;; measure startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name "eln-cache/" user-emacs-directory)))
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-deny-list '("evil*\\.el$")
        comp-speed 2
        comp-async-report-warnings-errors nil
        native-comp-async-report-warnings-errors nil
  ))

;; Initialize package sources
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t) ;; for debugging startup time

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(defvar dot-font-size 140)
(defvar dot-mono-font"JetBrainsMono Nerd Font")
(defvar dot-variable-font "Avenir Next")
(set-face-attribute 'default nil :font dot-mono-font :height dot-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font dot-mono-font :height dot-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font dot-variable-font :height (+ dot-font-size 30) :weight 'regular)

(setq inhibit-startup-message t)
(setq use-dialog-box nil)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar
(set-fringe-mode 5)        ; Give some breathing room

(global-auto-revert-mode t)    ;; auto load file when changed
(setq global-auto-revert-non-file-buffers t) ;; great for direc
(setq auto-revert-avoid-polling t)

(global-set-key (kbd "<escape>") 'keyboard-scape-quit)   ;; Make ESC quit prompts
(global-set-key (kbd "<mouse-3>") 'yank)

(setq default-directory "~/projects")
(setq max-lisp-eval-depth 10000)  ;; for lsp-mode
(setq max-specpdl-size 5000)  ;; for lsp-mode

;; y/n rather than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; hide title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; mac title bar
(when (equal system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))
;; Tab
;; http://ergoemacs.org/emacs/emacs_tabs_space_indentation_setup.html
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)
;; make indent commands use space only (never tab character)
(setq-default indent-tabs-mode nil)

;; no littering
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; replaced by no-littering
;; store all backup and autosave files in the tmp dir
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; Line Number
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; hightlight current line
(global-hl-line-mode t)

;; keep history
(savehist-mode 1)
(setq history-length 50)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; clipboard history, shorter for cleaner history
(setq kill-ring-max 20)

;; enable recentf
(recentf-mode 1)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)

;; auto remove trailing whitespace
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'markdown-mode)
              (delete-trailing-whitespace))))

;; case sensitive for query-replace
(setq case-fold-search  nil)

;; use interactive shell
(setq async-shell-command-buffer 'rename-buffer
      ;; bash -ic, zsh -ics
      shell-command-switch "-ics")

;; forcing split right
(setq split-height-threshold nil
      split-width-threshold 80)

(defun dot/dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    ;; replace this with xdg-open for linux
    (call-process "open" nil 0 nil file)
    (message "Opening %s done" file)))

(use-package dired
  :ensure nil
  :straight nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
       ("RET" . dired-find-alternate-file)
       ("-" . (lambda () (interactive) (find-alternate-file "..")))
       ("\C-c\C-y" . dired-copy-paste-do-copy)
       ("\C-c\C-p" . dired-copy-paste-do-paste)
       ("\C-c\C-d" . dired-copy-paste-do-cut)
       ("\C-c\C-o" . dot/dired-open-file)
       )
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-Agho --group-directories-first")
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil) ; disables warning
  ;; not use macos ls
  (when (equal system-type 'darwin)
    (setq insert-directory-program
     "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"
     ; "/usr/local/opt/coreutils/libexec/gnubin/ls"
)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "gh" 'dired-hide-dotfiles-mode))

(defvar dired-copy-paste-func nil)
(defvar dired-copy-paste-stored-file-list nil)

(defun dired-copy-paste-parse-files-for-directories (fns)
    "Add slashes to the end of files that are directories."
    (mapcar (lambda (fn)
	      (if (file-directory-p fn)
		  (concat fn "/")
		fn))
	    fns))


(defun dired-copy-paste-do-cut ()
  "In dired-mode, cut a file/dir on current line or all marked file/dir(s)."
  (interactive)
  (setq dired-copy-paste-stored-file-list (dired-copy-paste-parse-files-for-directories
					   (dired-get-marked-files))
        dired-copy-paste-func 'rename-file)
  (message
   (format "%S is/are cut."dired-copy-paste-stored-file-list)))


(defun dired-copy-paste-do-copy ()
  "In dired-mode, copy a file/dir on current line or all marked file/dir(s)."
  (interactive)
  (setq dired-copy-paste-stored-file-list (dired-get-marked-files)
        dired-copy-paste-func 'dired-copy-file)
  (message
   (format "%S is/are copied."dired-copy-paste-stored-file-list)))


(defun dired-copy-paste-do-paste ()
  "In dired-mode, paste cut/copied file/dir(s) into current directory."
  (interactive)
  (let ((stored-file-list nil))
    (dolist (stored-file dired-copy-paste-stored-file-list)
      (condition-case nil
          (progn
            (funcall dired-copy-paste-func stored-file (dired-current-directory) 1)
            (push stored-file stored-file-list))
        ;; (error nil)
	))
    (if (eq dired-copy-paste-func 'rename-file)
        (setq dired-copy-paste-stored-file-list nil
              dired-copy-paste-func nil))
    (revert-buffer)
    (message
     (format "%d file/dir(s) pasted into current directory." (length stored-file-list)))))

(use-package orderless
  :demand t
  :custom (completion-styles '(orderless)))

(use-package vertico
  :straight '(vertico :host github
                      :repo "minad/vertico"
                      :branch "main")
  :bind (:map minibuffer-local-map
         ("C-<return>" . vertico-exit-input))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

;; orignal project-recent-file excludes one exists in buffer list
(defvar consult-source-project-recent-file-incl-buffer
  `(:name     "Project File"
    :narrow   (?p . "Project")
    :hidden   t
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :new
    ,(lambda (file)
       (consult--file-action
        (expand-file-name file (consult--project-root))))
    :enabled
    ,(lambda ()
       (and consult-project-function
            recentf-mode))
    :items
    ,(lambda ()
      (when-let (root (consult--project-root))
        (let ((len (length root))
              (ht (consult--buffer-file-hash)))
          (mapcar (lambda (file)
                    (let ((part (substring file len)))
                      (when (equal part "") (setq part "./"))
                      (put-text-property 0 (length part)
                                         'multi-category `(file . ,file) part)
                      part))
                  (seq-filter (lambda (x) (string-prefix-p root x))
                              recentf-list))))))
  "Project file candidate source for `consult-buffer'.")

(use-package consult
  :demand t
  :bind
  (:map minibuffer-local-map
  (("C-r" . consult-history)))
  :config
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  (setq consult-preview-key (kbd "M-p"))
  (setq consult-project-buffer-sources
      (list
       `(:hidden nil :narrow ?f ,@consult-source-project-recent-file-incl-buffer)))
  :custom
  (consult-find-command "fd --color=never ARG OPTS")
  ;; filtering out system buffer with leading *, temp buffer with leading space and magit buffer
  (consult-buffer-filter '("^\*" "\\` " "magit*"))
  ;; (consult-buffer-sources '(consult--source-buffer consult--source-project-buffer  consult--source-project-file))
)

(use-package consult-lsp)

(use-package affe
  :straight (affe :type git :host github :repo "minad/affe")
  :after orderless
  :custom
  (affe-find-command "fd . --type f --color=never")
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches))

(defun embark-act-noquit ()
  "Run action but don't quit the minibuffer afterwards."
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

(defun dot/message-var (var)
(message "Var: %s" var)
)

(use-package embark
  :after vertico
  :bind
  (:map minibuffer-local-map
  (("C-e" . embark-act)
   ;; ("C-M-e" . embark-act-noquit) ;; crash emacs with emacs@28 native comp
    :map embark-general-map
    ("C-v" . consult-buffer-other-window)
    ("O" . consult-file-externally)
))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-action-indicator
    (lambda (map _target)
      (which-key--show-keymap "Embark" map nil nil 'no-paging)
      #'which-key--hide-popup-ignore-command)
    embark-become-indicator embark-action-indicator)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  ;; :hook (embark-collect-mode . consult-preview-at-point-mode)
)

(use-package marginalia
  :config
  (marginalia-mode))

;; Theme
(use-package doom-themes
  :custom
  (doom-gruvbox-dark-variant "hard")
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (line-number-mode -1)
  (column-number-mode -1)
  (size-indication-mode -1)
  :custom
  ((doom-modeline-height 10)
  (doom-modeline-buffer-encoding nil)
  ))

; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package yascroll
  :init (global-yascroll-bar-mode 1)
  :config
  (set-face-attribute 'yascroll:thumb-text-area nil :background "steel blue")
  (set-face-attribute 'yascroll:thumb-fringe nil :background "steel blue" :foreground "steel blue")
  :custom (yascroll:delay-to-hide 0.8)
)

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package good-scroll
:init (good-scroll-mode 1)
:disabled
:config
  (define-key evil-normal-state-map "\C-u" '(lambda () (interactive) (good-scroll-move (- 1000))))
)

(defun dot/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (set-variable 'org-hide-emphasis-markers t)
  (visual-line-mode 1))

(defun dot/org-font-setup ()
  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
 ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font dot-variable-font :weight 'regular :height (cdr face)))

  (custom-theme-set-faces 'user
                        `(org-level-3 ((t (:foreground "sky blue")))))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  )

(use-package org
  :demand t
  :commands (org-capture org-agenda)
  :hook (org-mode . dot/org-mode-setup)
  :config
  (setq org-startup-folded t)
  (setq org-ellipsis " ▾")
  (dot/org-font-setup)
  (setq org-agenda-files (quote ("~/Dropbox/org")))
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  ;; tags
  (setq org-tag-alist
    '((:startgroup)
      ; Put mutually exclusive tags here
      (:endgroup)
      ("@errand" . ?E)
      ("@home" . ?H)
      ("@work" . ?W)))
  ;; refiling
  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)))
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; org capture
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/projects/org/tasks" "Inbox")
          "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))
  )

(use-package org-superstar
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :custom
  (org-superstar-item-bullet-alist
  '((?- . ?•)
    (?+ . ?➤))))

(straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(defun dot/org-mode-visual-fill ()
  (setq visual-fill-column-width 140
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :after org
  :hook (org-mode . dot/org-mode-visual-fill))



(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      ))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation t)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
)

(defun dot/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-all)
  (org-display-inline-images))

(defun dot/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.75) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " "
        org-image-actual-width nil)
  (org-display-inline-images)
  (dot/org-present-prepare-slide)
  (setq-local org-appear-mode nil))

(defun dot/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun dot/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dot/org-present-prepare-slide))

(defun dot/org-present-next ()
  (interactive)
  (org-present-next)
  (dot/org-present-prepare-slide))

(use-package org-present
  :commands org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-l" . dot/org-present-next)
         ("C-c C-h" . dot/org-present-prev))
  :hook ((org-present-mode . dot/org-present-hook)
         (org-present-mode-quit . dot/org-present-quit-hook)))

;; Automatically tangle our Emacs.org config file when we save it
(defun dot/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/projects/emacs-config/dotemacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dot/org-babel-tangle-config)))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)  ;; for evil-collection
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; to black hole
  (define-key evil-normal-state-map "x" 'delete-forward-char)
  (define-key evil-normal-state-map "X" 'delete-backward-char)
  (evil-define-operator evil-delete-blackhole (beg end type register yank-handler)
    (interactive "<R><y>")
    (evil-delete beg end type ?_ yank-handler))
  (evil-define-operator evil-change-blackhole (beg end type register yank-handler)
    (interactive "<R><y>")
    (evil-change beg end type ?_ yank-handler))
  (evil-define-operator evil-change-line-blackhole (beg end type register yank-handler)
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-line))
  ;; (define-key evil-normal-state-map (kbd "d") 'evil-delete-blackhole)
  (define-key evil-normal-state-map (kbd "c") 'evil-change-blackhole)
  (define-key evil-normal-state-map (kbd "C") 'evil-change-line-blackhole)
  ;; (define-key evil-normal-state-map "Q" (kbd "@q"))
  (define-key evil-normal-state-map "Q" (lambda () (interactive) (evil-execute-macro 1 last-kbd-macro)))
)

;; (define-key evil-normal-state-map (kbd "SPC S") (lambda () (evil-ex "%s/")))
;; define an ex kestroke to a func
;; (eval-after-load 'evil-ex
;;   '(evil-ex-define-cmd "bl" 'gud-break))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; (use-package evil-snipe
;;   :after evil
;;   :init
;;   (setq evil-snipe-scope 'visible)
;;   (setq evil-snipe-repeat-scope 'whole-visible)
;;   :config
;;   (evil-snipe-mode)
;;   (evil-snipe-override-mode)
;;   (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package undo-fu
  :after evil
  :config
  (setq undo-limit 400000
      undo-strong-limit 3000000
      undo-outer-limit 3000000)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(defun dot/insert-curly ()
(interactive)
(insert "{\n}")
(evil-normal-state)
(evil-open-above 1)
)

(defun dot/insert-quote()
(interactive)
(insert "\"\"")
(left-char)
)

(use-package key-chord
;; :hook
;; (go-mode . (lambda () (key-chord-define go-mode-map "{{" 'dot/insert-curly)))
;; (rust-mode . (lambda () (key-chord-define rust-mode-map "{{" 'dot/insert-curly)))
;; (typescript-mode . (lambda () (key-chord-define rust-mode-map "{{" 'dot/insert-curly)))
;; (web-mode . (lambda () (key-chord-define rust-mode-map "{{" 'dot/insert-curly)))
;; (js-mode . (lambda () (key-chord-define rust-mode-map "{{" 'dot/insert-curly)))
:config
(key-chord-define-global "{{" 'dot/insert-curly)
(key-chord-define-global "\"\"" 'dot/insert-quote)
(key-chord-mode 1))

(setq tramp-default-method "ssh")

(use-package lsp-mode
  :demand t
  :commands (lsp lsp-deferred)
  :bind-keymap ("C-c l" . lsp-command-map)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-signature-function 'lsp-signature-posframe)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-signature-auto-activate nil) ;; you could manually request them via `lsp-signature-activate`
  ;; (setq lsp-signature-render-documentation nil)
  ;; ignore files for file watcher
  (setq lsp-file-watch-ignored-directories
        (append '("[/\\\\]\\.venv\\'") lsp-file-watch-ignored-directories))
)

(use-package flycheck
  :hook (lsp-mode . global-flycheck-mode))

(use-package lsp-ui
:after lsp-mode
:init
(setq lsp-ui-sideline-show-diagnostics t
      lsp-ui-sideline-show-hover nil
      lsp-ui-sideline-show-code-actions nil
      lsp-ui-doc-enable nil
      lsp-ui-doc-max-height 50
      eldoc-idle-delay 0 ;; bottom left defintion
))

(use-package lsp-treemacs
  :disabled
  :after lsp-mode)

(defun dot/company-complete ()
  "Insert the selected candidate or the first if none are selected."
  (interactive)
  (if company-selection
      (company-complete-selection)
    (company-complete-number 1)))

;; enable globally and default backend is dabbrev-code only (doesn't seem to work in org)
(use-package company
  :after lsp-mode
  ;; :hook
  ;; (lsp-mode . dot/init-company-lsp)
  :bind (:map company-active-map
         ("<tab>" . dot/company-complete)
        )
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-backends '(company-capf))
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0)
  :config
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  :disabled
  :commands dap-debug
  :config
  (require 'dap-hydra)
  ;; (dap-ui-mode 1)
  (add-hook 'dap-stopped-hook
        (lambda (arg) (call-interactively #'dap-hydra)))
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix "C-c"
    "d" '(dap-hydra t :wk "debugger")))

;; example https://www.reddit.com/r/emacs/comments/azddce/what_workflows_do_you_have_with_projectile_and/
(use-package projectile
  :disabled
  :demand t
  :diminish projectile-mode
  :config
  (projectile-mode)
  (define-key projectile-command-map (kbd "ESC") nil);; default ESC is bad toggle buffer
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
)

(use-package magit
  ;; enter opens file in the other window
  :bind (:map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window))
  :custom
  (magit-diff-refine-hunk (quote all)) ;; hightlight the exact diff
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :after magit)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

(use-package git-gutter
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2))

(defun dot/wgrep-commit ()
  (interactive)
  (wgrep-finish-edit)
  (wgrep-save-all-buffers)
)

(use-package wgrep
  :bind
  (:map wgrep-mode-map
    ("C-c C-c" . dot/wgrep-commit))
  (:map grep-mode-map
    ("C-c C-w" . wgrep-change-to-wgrep-mode))
)

;; disable evil in vterm, relies on zsh vi mode
(evil-set-initial-state 'vterm-mode 'emacs)

(use-package vterm
:commands vterm
:config (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
:commands vterm
:custom (vterm-toggle-scope 'project)
:config
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                ;; (display-buffer-reuse-window display-buffer-at-bottom)
                (display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                (direction . bottom)
                (dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.35)))
)

(use-package yasnippet
:config
(setq yas-snippet-dirs '("~/projects/emacs-config/snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
)

(use-package avy
:config
(setq avy-all-windows nil)
)

(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))
  (setf (alist-get 'gofmt apheleia-formatters)
        '("goimports"))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-l" "119" "-"))
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :config
  ;; https://github.com/emacs-mirror/emacs/blob/2181495af8f47057a7a61e01c192416b9ca70988/lisp/progmodes/hideshow.el#L258
  (add-to-list 'hs-special-modes-alist '(typescript-mode "{" "}" "/[*/]" nil))
  (add-to-list 'hs-special-modes-alist '(rust-mode "{" "}" "/[*/]" nil))
)

;; (defun dot/hs-toggle-fold ()
;;   (interactive)
;;   (if (hs-already-hidden-p)
;;     (save-excursion
;;       (end-of-line)
;;       (hs-show-block))
;;     (hs-hide-block)
;;   ))

(defun dot/hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('dot/hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun dot/hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('dot/hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'dot/hs-global-show))
      (_ (hs-hide-all))))

;; Make sure emacs use the proper ENV VAR
(use-package exec-path-from-shell
;; :after vterm
:demand t
:config
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; for daemon only
(when (daemonp)
  (exec-path-from-shell-initialize))
)

;; rainbow delimiter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Built-in Python utilities
(use-package python
  :hook (python-mode . lsp-deferred)
  ;; :custom
  ;; (dap-python-debugger 'debugpy)
  ;; (dap-python-executable "python3")
  :init
  (setq exec-path (append exec-path '("/usr/local/bin")))
  :config
  ;; (require 'dap-python)
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "ipython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3")))
  ;; change docstring color to be the same of comment
  (set-face-attribute 'font-lock-doc-face nil :foreground "#928374")
)

;; auto switching python venv to <project>/.venv
;; https://github.com/jorgenschaefer/pyvenv/issues/51
(defun dot/pyvenv-autoload ()
          (interactive)
          "auto activate venv directory if exists"
          (f-traverse-upwards (lambda (path)
              (let ((venv-path (f-expand ".venv" path)))
              (when (f-exists? venv-path)
              (pyvenv-activate venv-path))))))

(use-package pyvenv
  :after python
  :hook (python-mode . dot/pyvenv-autoload)
  :config
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "ipython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3")))
  (pyvenv-tracking-mode 1))

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :after python
  :ensure nil
  :straight nil
  :hook (inferior-python-mode . hide-mode-line-mode)
  :config (setq python-shell-prompt-detect-failure-warning nil))

;; pyright, it detects venv/.venv automatically
(use-package lsp-pyright
  :after python
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  (when (executable-find "python3"
        (setq lsp-pyright-python-executable-cmd "python3")))
  ;; :custom
  ;; (lsp-pyright-typechecking-mode "off")
)

  ;; (use-package blacken
  ;;   :after python
  ;;   :custom (blacken-line-length 99))

  ;; or use (when (eq major-mode 'python-mode) 'blacken-buffer)
  ;; (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook 'blacken-buffer)))

(use-package ein :disabled :commands ein:run)

;; (defun dot/lsp-go-before-save-hooks ()
;;   ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (setq gofmt-command "goimports")
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'dot/lsp-go-before-save-hooks)

(use-package go-mode
:init
(setq exec-path (append exec-path '("~/go/bin")))
:hook (go-mode . lsp-deferred)
;; :config
;; (require 'dap-go)
)

(use-package rust-mode
:init
(setq exec-path (append exec-path '("~/.cargo/bin")))
:hook (rust-mode . lsp-deferred)
:config
(setq rust-format-on-save t)
:custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"])
)

(use-package terraform-mode :mode "\\.tf\\'")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package typescript-mode
  :mode "\\.ts\\'"
  :init
  (setq exec-path (append exec-path '("/usr/local/bin")))
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq exec-path (append exec-path '("/usr/local/bin")))
  :hook (js2-mode . lsp-deferred)
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil))


(use-package prettier-js
  :config
  (setq prettier-js-show-errors nil))

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :hook (web-mode . lsp-deferred)
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package solidity-mode
:config
(setq solidity-comment-style 'slash)
)

;;; require cl otherwise got remove-if-not void function
;;; then it thinks it is a clang, which doesn't really work with forge

;; (require 'cl)
;; (use-package solidity-flycheck
;; :init
;; (setq solidity-solc-path "/opt/homebrew/bin/solc")
;; (setq solidity-flycheck-solc-checker-active t)
;; )

(use-package company-solidity
:hook (solidity-mode . (lambda ()
	(set (make-local-variable 'company-backends)
		(append '((company-solidity company-capf company-dabbrev-code))
			company-backends)))
))

;; (use-package elfeed
;; :config
;; (setf url-queue-timeout 15)
;; (setq elfeed-feeds
;;   '(
;;   "https://hnrss.org/frontpage"
;;   )
;; ))

;; toggle evil
(defun toggle-evilmode ()
  (interactive)
  (if (eq evil-state 'normal)
    (progn
      ; go emacs
      (message "Emacs Mode")
      (evil-emacs-state)
      (set-variable 'cursor-type 'bar)
    )
    (progn
      ; go evil
      (message "Normal Mode")
      (evil-normal-state)
      (set-variable 'cursor-type 'box)
    )
  )
)

(defun dot/go-to-dotemacs ()
    "Go To Emacs Config File"
    (interactive)
    (find-file'dot/go-to-dotemacs "~/projects/emacs-config/dotemacs.org"))

(defun dot/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (delete-backward-char arg)))

(defun dot/toggle-frame ()
    "
    Toggle between make-frame (if visible frame == 1) and delete-frame (else).
    Mimic toggling maximized buffer behaviour together with the starting frame maximized setting
    "
    (interactive)
    (if (eq (length (visible-frame-list)) 1)
        (make-frame)
        (delete-frame)))

(defun dot/toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun dot/split-dired-jump ()
    "Split left dired jump"
    (interactive)
    (split-window-right)
    (evil-window-right 1)
    (balance-windows)
    (dired-jump))

(defun dot/kill-other-prog-buffers ()
  "Kill all other buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (set-buffer buffer)
        (when (not (equal major-mode 'fundamental-mode))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i prog buffer(s)." count)))
)

;; (defun dot/new-named-tab (name)
;;     "Create a new tab with name inputs, prefixed by its index"
;;     (interactive "MNew Tab Name: ")
;;     (tab-bar-new-tab)
;;     (tab-bar-rename-tab (concat (number-to-string (+ 1 (tab-bar--current-tab-index))) "-" name)))

(defun dot/straight-freeze-then-backup ()
  (interactive)
  (straight-freeze-versions)
  (delete-file "~/projects/emacs-config/default.el")
  (copy-file "~/.config/emacs/straight/versions/default.el" "~/projects/emacs-config/default.el")
)

(defun dot/straight-thaw-from-backup ()
  (interactive)
  (delete-file "~/.config/emacs/straight/versions/default.el")
  (copy-file "~/projects/emacs-config/default.el" "~/.config/emacs/straight/versions/default.el" )
  (straight-thaw-versions)
)

(defun dot/find-in-projects (&optional initial)
  (interactive "P")
    (affe-find "~/projects" initial))

(defun dot/shell-project-root ()
  ; only works with git repo
  (interactive)
  (let ((default-directory (car (project-roots (project-current)))))
    (call-interactively #'async-shell-command))
  (other-window 1)
  (evil-normal-state)
)

(defun dot/switch-project ()
  (interactive)
  (let ((project-dir (completing-read "Switch to project: " (split-string (shell-command-to-string "ls ~/projects/")))))
  (affe-find (concat "~/projects/" project-dir))))

(use-package hydra
 :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale font size"
  ("k" text-scale-increase "increase")
  ("j" text-scale-decrease "decrease")
  ("q" nil "quit" :exit t))

(use-package general
  :config
  ;; leader key overrides for all modes (e.g. dired) in normal state
  (general-override-mode)
  (general-define-key
    :states '(normal emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    "t" '(vterm-toggle :which-key "toggle vterm")
    "r" '(consult-ripgrep :which-key "ripgrep")
    "s" 'query-replace
    "p" '(dot/switch-project :which-key "switch project")
    "b" '(consult-buffer :which-key "switch buffer")
    "j" '(next-buffer :which-key "next buffer")
    "k" '(previous-buffer :which-key "previous buffer")
    "q" '(kill-current-buffer :which-key "kill current buffer")
    "Q" '(dot/kill-other-prog-buffers :which-key "kill buffers except current")
    ;; magit
    "SPC" '(magit-status :which-key "magit status")
    "g"   '(:ignore g :which-key "magit commands")
    "gc"  '(magit-branch-checkout :which-key "checkout a branch")
    "gd"  '(magit-diff-unstaged :which-key "diff unstaged")
    "gl"  '(magit-log-buffer-file :which-key "git log current buffer")
    "gm"  '(vc-refresh-state :which-key "update modeline vc state")
    ;; find file ops
    "f" '(:ignore f :which-key "file commands")
    "fp" '(dot/find-in-projects :which-key "fd files ~/projects")
    "fe" '((lambda () (interactive) (find-file "~/projects/emacs-config/dotemacs.org")) :which-key "go to emacs config file")
    "fr" '(consult-recent-file :which-key "find recent files")
    "ff" '(consult-project-buffer :which-key "find project buffers and recent files")
    "fd" '(affe-find :which-key "find project files")
    "fo" '((lambda () (interactive) (affe-find "~/Dropbox/org")) :which-key "find org file")
    ;; bookmarks
    "m" '(:ignore m :which-key "bookmark commands")
    "mm" '(consult-bookmark :which-key "bookmark consult")
    "ms" '(bookmark-set :which-key "bookmark set")
    "md" '(bookmark-delete :which-key "bookmark delete")
    ;; lsp, linting etc.
    "l" '(:ignore l :which-key "lsp commands")
    "lr" '(lsp-workspace-restart :which-key "lsp-restart-workspace")
    "ld" '(lsp-ui-doc-glance :which-key "lsp-ui-doc-glance")
    "lf" '(lsp-ui-doc-focus-frame :which-key "lsp ui focus frame")
    "ll" '(flycheck-next-error :which-key "list errors")
    ;; "le" '(flycheck-list-errors :which-key "list errors")
    ;; org
    "o" '(:ignore o :which-key "org commands")
    "oa"  '(org-agenda :which-key "agenda")
    "oc"  '(org-capture t :which-key "capture")
    ;; hydra
    "h" '(:ignore h :which-key "hydra commands")
    "hf" '(hydra-text-scale/body :which-key "scale font size")
    )
  ;; non leader key overrides
  (general-define-key
    :states '(normal visual emacs)
    :keymaps 'override
    "C-k" 'evil-window-up
    "C-j" 'evil-window-down
    "C-h" 'evil-window-left
    "C-l" 'evil-window-right
    "C-<tab>" 'dot/hs-cycle
    "C-S-<tab>" 'dot/hs-global-cycle
    "ZZ" (lambda () (interactive) (delete-window) (balance-windows))
    "8" 'back-to-indentation
    "9" 'end-of-line
  )
  ;; non-override global mapping for normal + insert state
  (general-define-key
    :states '(normal insert visual emacs)
    "<f12>"   'dot/toggle-maximize-buffer
    "M-z" 'toggle-evilmode
    "C-/"   'consult-line
    "C-M-/" 'consult-outline
    "C-M-p" 'consult-yank-replace
    "C-M-s"   'dot/shell-project-root
    ;; TODO consult-register
  )
  ;; evil normal/visual mapping
  ;; (general-evil-setup)
  (general-define-key
    :states '(normal visual)
    "s" 'avy-goto-char-2-below
    "S" 'avy-goto-char-2-above
    "gl" 'avy-goto-line
    "gw" 'avy-goto-word-1
    "\\" '(lambda () (interactive) (evil-window-vsplit) (evil-window-right 1))
    "-" 'dired-jump
    "_" 'dot/split-dired-jump)
)

  ;; org-mod
  (general-define-key
    :states 'normal
    :keymaps 'org-mode-map
    "K" 'org-up-element
  )
  ;; vterm-mod
  (general-define-key
    :states  'insert
    :keymaps 'vterm-mode-map
    "C-c"    'vterm-send-C-c
  )
  ;; yasnippet
  ;; http://joaotavora.github.io/yasnippet/snippet-expansion.general
  (general-define-key
    :states '(insert)
    :keymaps 'yas-minor-mode-map
    "M-TAB" #'yas-expand
    "SPC" yas-maybe-expand
  )
