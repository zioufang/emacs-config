;; doom emacs
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 100000000 ; 16mb
          gc-cons-percentage 0.1)))

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
;; Tab
;; http://ergoemacs.org/emacs/emacs_tabs_space_indentation_setup.html
(setq-default tab-width 2)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)
(progn
  ;; make indent commands use space only (never tab character)
  (setq-default indent-tabs-mode nil))

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

;; enable recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-d" . dired-jump))
  :custom
  (dired-listing-switches "-Agho --group-directories-first")
  :config
  ;; not use macos ls
  (when (equal system-type 'darwin)
    (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")))

(use-package dired-single)
(defun dot/dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (dot/dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'dot/dired-init))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "gh" 'dired-hide-dotfiles-mode))

(use-package ivy
  :diminish
  :bind (
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-initial-inputs-alist nil)    ;; remove ^
  (setq ivy-extra-directories nil) ;; remove ./.. from dir
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1)
  (setq prescient-sort-length-enable nil))

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

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

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

;; TODO make them one func with folder path
(defun dot/find-org ()
    "Open Org Dir"
    (interactive)
    (counsel-find-file "~/projects/org"))

(defun dot/find-proj ()
    "Open Org Dir"
    (interactive)
    (counsel-find-file "~/projects"))

(defun dot/go-to-dotemacs ()
    "Go To Emacs Config File"
    (interactive)
    (find-file "~/projects/emacs-config/dotemacs.org"))

(defun dot/toggle-frame ()
    "
    Toggle between make-frame (if visible frame == 1) and delete-frame (else).
    Mimic toggling maximized buffer behaviour in full screen mode
    "
    (interactive)
    (if (eq (length (visible-frame-list)) 1)
        (make-frame)
        (delete-frame)))

(defun dot/split-dired-jump ()
    "Split left dired jump"
    (interactive)
    (split-window-right)
    (evil-window-right 1)
    (dired-jump))

(use-package general
  :config
  (general-create-definer leaderkey
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
  )
  ;; evil mapping
  (general-evil-setup)
  (general-nmap
    "C-k" 'evil-window-up
    "C-j" 'evil-window-down
    "C-h" 'evil-window-left
    "C-l" 'evil-window-right
    "-" 'dired-jump
    "_" 'dot/split-dired-jump)
  ;; global mapping
  (general-define-key
    "<f12>"   'dot/toggle-frame
    "C-s"   'swiper
    "C-M-r" 'counsel-recentf
    "C-M-p" 'dot/find-proj
    "C-M-o" 'dot/find-org
    "C-M-e" 'dot/go-to-dotemacs
  )
  (leaderkey
    "h" '(:ignore h :which-key "hydra commands")
    "t" '(vterm-toggle :which-key "toggle vterm")
    "p" '(counsel-projectile-switch-project :which-key "switch project")
    "b" '(counsel-projectile-switch-to-buffer :which-key "project switch buffer")
    "B" '(ivy-switch-buffer :which-key "switch buffer")
    "f" '(counsel-projectile-find-file :which-key "project find file")
    "F" '(counsel-find-file :which-key "find file")
    "r" '(counsel-projectile-rg :which-key "project ripgrep")
    "SPC" '(magit-status :which-key "magit status")
    )
  ;; dired-mode workarounds
  ;; (general-define-key
  ;;   :states 'normal
  ;;   :keymaps 'dired-mode-map
  ;; )
)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale font size"
  ("k" text-scale-increase "increase")
  ("j" text-scale-decrease "decrease")
  ("q" nil "quit" :exit t))

(leaderkey
  "hf" '(hydra-text-scale/body :which-key "scale font size"))

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
  ;; keybindings
  ;; remove C-j/k for org-forward/backward-heading-same-level
  (define-key org-mode-map (kbd "<normal-state> C-j") nil)
  (define-key org-mode-map (kbd "<normal-state> C-k") nil)
  ;; moving up one element
  (define-key org-mode-map (kbd "<normal-state> K") 'org-up-element)
  ;; toggle emphasis
  (define-key org-mode-map (kbd "C-c e") 'org-toggle-emphasis)
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
  :hook (org-mode . dot/org-mode-visual-fill))

(require 'ob-go)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (go . t)
    ))
(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))

(use-package hide-mode-line)

(defun dot/presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

  ;; This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
  ;;                                    (org-verbatim (:height 1.75) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block))))

(defun dot/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  ;; (text-scale-mode 0))

  ;; If you use face-remapping-alist, this clears the scaling:
  (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . dot/presentation-setup)
         (org-tree-slide-stop . dot/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil)
  :config
  (define-key org-tree-slide-mode-map (kbd "C-<left>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "C-<right>") 'org-tree-slide-move-next-tree))

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

(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'whole-visible)
  :config
  (evil-snipe-mode)
  (evil-snipe-override-mode)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

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

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook 
  (python-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; ignore files for file watcher
  (setq lsp-file-watch-ignored-directories 
        (append '("[/\\\\]\\.venv\\'") lsp-file-watch-ignored-directories))
)

;; in-buffer completion interface
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-common-or-cycle))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))

;; icon + others pretty stuff
(use-package company-box
  :hook (company-mode . company-box-mode))

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

;; Built-in Python utilities
(use-package python
  :config
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
  :hook (inferior-python-mode . hide-mode-line-mode))

;; pyright, it detects venv/.venv automatically 
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :init 
  (when (executable-find "python3"
        (setq lsp-pyright-python-executable-cmd "python3")))
  :custom (lsp-pyright-typechecking-mode "off")
)

(use-package blacken
  :after python
  :custom (blacken-line-length 119))

(add-hook 'before-save-hook 'blacken-buffer)

(defun dot/lsp-go-before-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'dot/lsp-go-before-save-hooks)

(use-package go-mode
:hook (go-mode . lsp-deferred)
)

(use-package terraform-mode)

(use-package dockerfile-mode)

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
  :config (counsel-projectile-mode))
;; term emulator, needs CMAKE to compile

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

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
