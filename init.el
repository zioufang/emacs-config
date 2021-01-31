;; doom emacs
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 100000000 ; 16mb
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

(defvar dot-font-size 150)
(defvar dot-mono-font"JetBrains Mono")
(defvar dot-variable-font "Cantarell")
(set-face-attribute 'default nil :font dot-mono-font :height dot-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font dot-mono-font :height dot-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font dot-variable-font :height (+ dot-font-size 30) :weight 'regular)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar
(set-fringe-mode 5)        ; Give some breathing room

(auto-revert-mode t)    ;; auto load file when changed

(global-set-key (kbd "<escape>") 'keyboard-scape-quit)   ;; Make ESC quit prompts

(setq default-directory "~/projects")
(setq max-lisp-eval-depth 10000)  ;; for lsp-mode
(setq max-specpdl-size 5000)  ;; for lsp-mode

;; y/n rather than yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; clipboard history, shorter for cleaner counsel-yank-pop
(setq kill-ring-max 10)

;; enable recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; auto remove trailing whitespace
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'markdown-mode)
              (delete-trailing-whitespace))))

(setq tab-bar-new-tab-to `rightmost
      tab-bar-show t
      tab-bar-new-tab-choice "~/projects"
)

;; Get the current tab name for use in some other display when tab-bar-show = nil
(defun dot/current-tab-name ()
  (alist-get 'name (tab-bar--current-tab)))

(use-package dired
  :ensure nil
  :straight nil
  :hook (dired-mode . dired-hide-details-mode)
  :commands (dired dired-jump)
  :bind (("C-x C-d" . dired-jump))
  :custom
  (dired-listing-switches "-Agho --group-directories-first")
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil) ; disables warning
  ;; not use macos ls
  (when (equal system-type 'darwin)
    (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "gh" 'dired-hide-dotfiles-mode))

(defun dot/find-file-right (filename)
  (interactive)
  (split-window-right)
  (other-window 1)
  (balance-windows)
  (find-file filename))
(defun dot/find-file-below (filename)
  (interactive)
  (split-window-below)
  (other-window 1)
  (balance-windows)
  (find-file filename))
(defun dot/set-ivy-action-split-find-file (ivy-func)
  (ivy-add-actions
    ivy-func
    '(("v" dot/find-file-right "open right")
    ("s" dot/find-file-below "open below")))
)
(use-package ivy
  :diminish
  :bind (
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-r" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-initial-inputs-alist nil)    ;; remove ^
  (setq ivy-extra-directories nil) ;; remove ./.. from dir
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done) ;; single tab completion (was double)
  (dolist (ivy-func
  '(ivy-switch-buffer))
  (dot/set-ivy-action-split-find-file ivy-func))
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (dolist (ivy-func
  '(counsel-find-file
    counsel-recentf))
  (dot/set-ivy-action-split-find-file ivy-func)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; better M-x, provide frequent items at the top
(use-package amx
  :after ivy
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/.config/emacs/amx-hist")
  (amx-history-length 100)
  (amx-show-key-bindings nil)
  :config
  (amx-mode 1))

;; unmaintained, still looking for maintainer
;; (use-package ivy-prescient
;;   :after counsel
;;   :config
;;   (ivy-prescient-mode 1)
;;   (prescient-persist-mode 1)
;;   (setq prescient-sort-length-enable nil))

;; better help for counsel
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package bufler
  :disabled
  :bind (("C-M-j" . bufler-switch-buffer)
         ("C-M-k" . bufler-workspace-frame-set))
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET")   'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "d"           'bufler-list-buffer-kill)

  (setf bufler-groups
        (bufler-defgroups
          ;; Subgroup collecting all named workspaces.
          (group (auto-workspace))
          ;; Subgroup collecting buffers in a projectile project.
          (group (auto-projectile))
          ;; Grouping browser windows
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "Help/Info"
                     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                     ;; (mode-match "*Helpful*" (rx bos "helpful-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' & `dired' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (name-match "**Special**"
                                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*"))))
          ;; Group remaining buffers by major mode.
          (auto-mode))))

;; Theme
(use-package doom-themes
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

(use-package command-log-mode)

(defun dot/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (set-variable 'org-hide-emphasis-markers t)
  (visual-line-mode 1))

(defun dot/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
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

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

(use-package org
  :hook (org-mode . dot/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (dot/org-font-setup)
  ;; remove C-j/k for org-forward/backward-heading-same-level
  ;; (define-key org-mode-map (kbd "<normal-state> C-j") nil)
  ;; (define-key org-mode-map (kbd "<normal-state> C-k") nil)
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun dot/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :after org
  :hook (org-mode . dot/org-mode-visual-fill))

(require 'ob-go)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (go . t)
    (ein . t)
    ))
(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("np" . "src ein-python :session localhost
"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(defun dot/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dot/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-verbatim (:height 1.75) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (dot/org-present-prepare-slide))

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
  (global-evil-surround-mode))

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

(use-package key-chord
:hook (go-mode . (lambda () (key-chord-define go-mode-map "{{" 'dot/insert-curly)))
:config
(key-chord-mode 1))

(setq tramp-default-method "ssh")

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  :bind-keymap ("C-c l" . lsp-command-map)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; ignore files for file watcher
  (setq lsp-file-watch-ignored-directories
        (append '("[/\\\\]\\.venv\\'") lsp-file-watch-ignored-directories))
)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-ui
:after lsp-mode
:init
(setq lsp-ui-sideline-show-diagnostics t
      lsp-ui-sideline-show-hover nil
      lsp-ui-sideline-show-code-actions nil
      lsp-ui-doc-enable nil
))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package lsp-ivy)

;; enable globally and default backend is dabbrev-code only (doesn't seem to work in org)
(use-package company
  :after lsp-mode
  ;; :hook
  ;; (lsp-mode . dot/init-company-lsp)
  :init
  (setq company-backends '(company-capf))
  :bind (:map company-active-map
         ("<tab>" . company-complete-common-or-cycle))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  ;; (company-backends '(company-capf :with company-yasnippet :with company-files))
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))
  :config
  (global-company-mode)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :config
  (company-prescient-mode 1))

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
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
  :diminish projectile-mode
  :config
  (projectile-mode)
  (define-key projectile-command-map (kbd "ESC") nil);; default ESC is bad toggle buffer
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired)
)
;; better ivy/counsel integration with M-o
(use-package counsel-projectile
  :after ivy
  :config
    (dolist (ivy-func
    '(counsel-projectile-find-file
      counsel-projectile-switch-to-buffer))
    (dot/set-ivy-action-split-find-file ivy-func))

  (counsel-projectile-mode))
;; term emulator, needs CMAKE to compile

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
  :defer t)

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

(use-package wgrep)

(use-package vterm
:commands vterm
:config (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
:config
(setq vterm-toggle-fullscreen-p nil)
;; open vterm in dedicated bottom window
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                ;; (display-buffer-reuse-window display-buffer-at-bottom)
                (display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                (direction . bottom)
                (dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))
)

(use-package yasnippet
:config
(setq yas-snippet-dirs '("~/projects/emacs-config/snippets"))
(yas-global-mode 1))

(use-package avy)

;; Make sure emacs use the proper ENV VAR
(use-package exec-path-from-shell)
;; disable auto load as it is slow
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; for daemon only
(when (daemonp)
  (exec-path-from-shell-initialize))

;; rainbow delimiter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Built-in Python utilities
(use-package python
  :custom
  (dap-python-debugger 'debugpy)
  (dap-python-executable "python3")
  :config
  (require 'dap-python)
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
  :ensure nil
  :straight nil
  :hook (inferior-python-mode . hide-mode-line-mode))

;; pyright, it detects venv/.venv automatically
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :init
  (when (executable-find "python3"
        (setq lsp-pyright-python-executable-cmd "python3")))
  :custom
  (lsp-pyright-typechecking-mode "off")
  (lsp-pyright-auto-import-completions nil)
)

(use-package blacken
  :after python
  :custom (blacken-line-length 99))

;; or use (when (eq major-mode 'python-mode) 'blacken-buffer)
(add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook 'blacken-buffer)))

(use-package ein)

(defun dot/lsp-go-before-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'dot/lsp-go-before-save-hooks)

(use-package go-mode
:hook (go-mode . lsp-deferred)
:config
(require 'dap-go)
)

(use-package terraform-mode)

(use-package dockerfile-mode)

;; (use-package elfeed
;; :config
;; (setf url-queue-timeout 15)
;; (setq elfeed-feeds
;;   '(
;;   "https://hnrss.org/frontpage"
;;   )
;; ))

(defun dot/go-to-dotemacs ()
    "Go To Emacs Config File"
    (interactive)
    (find-file'dot/go-to-dotemacs "~/projects/emacs-config/dotemacs.org"))

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
    (dired-jump))

(defun dot/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun dot/new-named-tab (name)
    "Create a new tab with name inputs, prefixed by its index"
    (interactive "MNew Tab Name: ")
    (tab-bar-new-tab)
    (tab-bar-rename-tab (concat (number-to-string (+ 1 (tab-bar--current-tab-index))) "-" name)))

(use-package hydra)

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
      :non-normal-prefix "M-SPC"
      "t" '(vterm-toggle :which-key "toggle vterm")
      "p" '(counsel-projectile-switch-project :which-key "switch project")
      "b" '(counsel-projectile-switch-to-buffer :which-key "project switch buffer")
      "B" '(ivy-switch-buffer :which-key "switch buffer")
      "r"  '(ivy-resume :which-key "ivy resume")
      ;; magit
      "SPC" '(magit-status :which-key "magit status")
      "g"   '(:ignore g :which-key "magit ops")
      "gc"  '(magit-branch-or-checkout :which-key "checkout a branch")
      "gd"  '(magit-diff-unstaged :which-key "diff unstaged")
      "gl"  '(magit-log-buffer-file :which-key "git log current buffer")
      ;; find file ops
      "f" '(:ignore f :which-key "file ops")
      "ff" '(counsel-projectile-find-file :which-key "project find file")
      "fF" '(counsel-find-file :which-key "find file")
      "fr" '(counsel-recentf :which-key "find recent file")
      "fo" '((lambda () (interactive) (counsel-find-file "~/projects/org")) :which-key "find org file")
      "fp" '((lambda () (interactive) (counsel-find-file "~/projects/")) :which-key "find file in projects")
      "fe" '((lambda () (interactive) (find-file "~/projects/emacs-config/dotemacs.org")) :which-key "go to emacs config file")
      ;; hydra
      "h" '(:ignore h :which-key "hydra commands")
      "hf" '(hydra-text-scale/body :which-key "scale font size")
      )
    ;; non leader key overrides
    (general-define-key
      :states '(normal emacs)
      :keymaps 'override
      "C-k" 'evil-window-up
      "C-j" 'evil-window-down
      "C-h" 'evil-window-left
      "C-l" 'evil-window-right
      "<f12>"   'dot/toggle-maximize-buffer
      "ZZ" '(delete-window :which-key "close window")
    )
    ;; non-override global mapping for normal + insert state
    (general-define-key
      :states '(normal insert visual emacs)
      "C-s"   'swiper
      "C-M-r" 'counsel-projectile-rg
      "C-M-p" 'counsel-yank-pop
      ;; tab bar
      "C-M-t" 'dot/new-named-tab
      "s-1" (lambda () (interactive) (tab-bar-select-tab 1))
      "s-2" (lambda () (interactive) (tab-bar-select-tab 2))
      "s-3" (lambda () (interactive) (tab-bar-select-tab 3))
      "s-4" (lambda () (interactive) (tab-bar-select-tab 4))
    )
    ;; evil normal mapping
    (general-evil-setup)
    (general-nmap
      "s" 'avy-goto-char-2
      "gl" 'avy-goto-line
      "gw" 'avy-goto-word-1
      "-" 'dired-jump
      "_" 'dot/split-dired-jump)
    ;; org-mod
    (general-define-key
      :states 'normal
      :keymaps 'org-mode-map
      "K" 'org-up-element
      "C-c e" 'org-toggle-emphasis
    )
    ;; dired-mod
    (general-define-key
      :states  'normal
      :keymaps 'dired-mode-map
      ;; reuse dired buffer
      "RET"    'dired-find-alternate-file
      "-"      (lambda () (interactive) (find-alternate-file ".."))
      ;; in buffer rename with C-c C-c to confirm
      "i"      (lambda () (interactive) (evil-insert 1))
      "I"      (lambda () (interactive) (evil-insert-line 1))
      "a"      (lambda () (interactive) (evil-append 1))
      "A"      (lambda () (interactive) (evil-append-line 1))
    )
    ;; yasnippet
    ;; http://joaotavora.github.io/yasnippet/snippet-expansion.general
    (general-define-key
      :states '(insert)
      :keymaps 'yas-minor-mode-map
      "M-TAB" #'yas-expand
      "SPC" yas-maybe-expand
    )
)
