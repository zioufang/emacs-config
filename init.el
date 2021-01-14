(defvar default-font-size 150)
(set-face-attribute 'default nil :font "JetBrains Mono" :height default-font-size)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar

(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar
(set-fringe-mode 5)        ; Give some breathing room

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-scape-quit)

;; Tab
;; http://ergoemacs.org/emacs/emacs_tabs_space_indentation_setup.html
(setq-default tab-width 4)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)
(progn
  ;; make indent commands use space only (never tab character)
  (setq-default indent-tabs-mode nil))

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

;; line number
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

(defun find-org ()
    "Open where I often go."
    (interactive)
    (counsel-find-file "~/projects/org"))

;; Custom Keybindings
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
    "C-l" 'evil-window-right)
  ;; global mapping
  (general-define-key
    "C-M-b" 'ivy-switch-buffer
    "C-M-o" 'find-org
  )
  (leaderkey
    "h" '(:ignore h :which-key "hydra commands")
    "p" '(projectile-command-map :which-key "projectile commands")
    )
  )

;; Org
(use-package org)

;; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale font size"
  ("k" text-scale-increase "increase")
  ("j" text-scale-decrease "decrease")
  ("q" nil "quit" :exit t))

(leaderkey
  "hf" '(hydra-text-scale/body :which-key "scale font size"))

;; Evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)  ;; for evil-collection
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package undo-fu
  :after evil
  :config
  (setq undo-limit 400000
      undo-strong-limit 3000000
      undo-outer-limit 3000000)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

;; Ivy
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

;; better help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Projectile
;; example https://www.reddit.com/r/emacs/comments/azddce/what_workflows_do_you_have_with_projectile_and/
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))
  (define-key projectile-command-map (kbd "ESC") nil);; default ESC is bad toggle buffer

;; better ivy/counsel integration with M-o
(use-package counsel-projectile
  :config (counsel-projectile-mode))
;; term emulator, needs CMAKE to compile
(use-package vterm)

;; Magit
(use-package magit)
(use-package forge)

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" default))
 '(package-selected-packages
   '(forge evil-magit magit projectile evil-commentary undo-fu hydra evil-collection general helpful counsel ivy-rich which-key vterm ivy use-package evil doom-themes doom-modeline))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
