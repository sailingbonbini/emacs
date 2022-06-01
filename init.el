(scroll-bar-mode -1)
;; Minimal UI
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(show-paren-mode)


;; use fn key as control key
(setq mac-function-modifier 'control)
;; use left option key as super key
(setq mac-option-modifier 'super)

(setq td-local-emacs-dir (file-name-directory (or load-file-name (buffer-file-name))))

(message td-local-emacs-dir)

(add-to-list 'custom-theme-load-path (concat td-local-emacs-dir "/themes/"))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
   that used by the user's shell.

   This is particularly useful under Mac OS X and macOS, where GUI
   apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defun my/reload-config()
  (interactive)
  (load-file (concat td-local-emacs-dir "init.el")))

(defun my/set-olivetti-org-faces()
  (interactive)
  (setq olivetti-body-width 100)
  (custom-set-faces '(org-level-1 ((t (:inherit outline-1 :height 1.2)))))
  (custom-set-faces '(org-level-2 ((t (:inherit outline-2 :height 1.1)))))
  (custom-set-faces '(org-level-3 ((t (:inherit outline-3 :height 1.0)))))
  (custom-set-faces '(org-level-4 ((t (:inherit outline-4 :height 1.0)))))
  (custom-set-faces '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq line-spacing 0.8)
  (text-scale-set 2)
  (centered-cursor-mode))

(defun my/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))


;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t)
;; (zenburn-with-color-variables
;;   (custom-theme-set-faces
;;    'zenburn
;;    `(org-level-1 ((t (:foreground ,zenburn-fg))))
;;    `(org-level-2 ((t (:foreground ,zenburn-fg))))
;;    `(org-level-3 ((t (:foreground ,zenburn-fg))))
;;    `(org-level-4 ((t (:foreground ,zenburn-fg))))
;;    `(org-level-5 ((t (:foreground ,zenburn-fg))))
;;    `(org-level-6 ((t (:foreground ,zenburn-fg))))
;;    `(org-level-7 ((t (:foreground ,zenburn-fg))))
;;    `(org-level-8 ((t (:foreground ,zenburn-fg))))))
;;   )

;; doom theme
;;(use-package material-themes
;;  :ensure t
;;  :config
;;  (load-theme 'material t))


;; ivy completion/switcher etc
(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t))
 
(use-package counsel :ensure t)
(use-package swiper :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)                            ;; enable elpy when python mode comes on
  :config
  (setq python-indent-offset 4)                                                   ;; offset 2
  (setq elpy-rpc-virtualenv-path 'current)                             
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))                    ;; use flycheck instead of flymake
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package nlinum
  :ensure t
  :defer t)

(use-package general
  :ensure t)

;; enable flyspell for all modes that derive from text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq ispell-program-name "/usr/local/bin/ispell")

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; distraction free writing mode
(use-package olivetti
  :ensure t
  :config
  (add-hook 'olivetti-mode-hook 'my/set-olivetti-org-faces))

;;typewriter kind of scrolling
(use-package centered-cursor-mode
  :ensure t)

;;org bullets, make orgs headers look nicer
;;(use-package org-superstar
;;  :config
;;  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; allows to display word counts on subtrees etc
(use-package org-wc
  :ensure t)

(use-package cider
  :ensure t)

;; org mode setup
(setq org-agenda-files '("~/org/inbox.org"))

;; tags
(setq org-tag-alist '(("sharon")
		      ("markus") ("asim") ("tsufit") ("venkat")
		      ("sharon") ("eyal") ("eli") ("doron")
		      ("mariusz") ("samira") ("shimon") ("graeme") ("limor")
		      ))


;; capture templates
(setq org-capture-templates '(( "t" "Task" entry
				(file+headline "~/org/inbox.org" "Inbox")
				"* TODO %?")
			      ( "n" "Note" entry
				(file+headline "~/org/inbox.org" "Inbox")
				"* %?")
			      ( "m" "Meeting" entry
				(file+headline "~/org/inbox.org" "Inbox")
				"* %U %?")))
;; capture targets
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
(setq org-startup-indented 1)

;; hide org's emphasis markers
(setq org-hide-emphasis-markers t)



(defun td/capture-task ()
  (interactive)
  (org-capture nil "t"))

(general-define-key
 "C-c c" 'counsel-org-capture
 "C-x C-d" 'my/duplicate-line
 "C-c r" 'councel-recentf
 "C-c e" 'eval-buffer
 )

(general-define-key
 :keymaps 'org-mode-map
 "C-c a" 'org-agenda
 "C-c C-a" 'org-archive-subtree
 "C-c c" 'counsel-org-capture
 "C-c d" 'org-todo
 "C-c C-r" 'org-refile
 "C-c C-t" 'counsel-org-tag
 "C-c t" 'org-todo
 "C-x C-r" 'counsel-recentf
 "<f1>" 'td/capture-task) 

(setq org-todo-keywords
      '((sequence "TODO(t)" "FOLLOW-UP(f@/!)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-log-into-drawer t)

(defun td/split-window-2-and-switch ()
  (interactive)
  (if (winum-get-window-by-number 2)
      (winum-select-window-2)
    (progn (split-window-horizontally)
     (winum-select-window-2))))

(defun td/split-window-3-and-switch ()
  (interactive)
  (if (winum-get-window-by-number 3)
      (winum-select-window-3)
    (progn (split-window-vertically)
     (winum-select-window-3))))


(general-define-key
 "s-1" 'winum-select-window-1
 "s-2" 'td/split-window-2-and-switch
 "s-3" 'td/split-window-3-and-switch
 "s-4" 'winum-select-window-4
 "s-5" 'winum-select-window-5)

(defun td/load-profile ()
  (interactive)
  (find-file "~/.bash_profile"))

(use-package flycheck
  :ensure t
  :defer t)

(use-package major-mode-hydra
  :ensure t
  :bind
  ("M-SPC" . major-mode-hydra))

(major-mode-hydra-define python-mode nil
			 ("Eval"
			  (("b" elpy-shell-send-region-or-buffer "buffer")
			   ("s" elpy-shell-send-statement-and-step "statement"))
			  "Env"
			  (("a" pyenv-workon "activte")
			   ("d" pyenv-deactivate "deactivate"))))

(major-mode-hydra-define org-mode nil
  ("Insert"
   (("l" org-insert-link "link")
    ("t" counsel-org-tag "tag"))
   "Tools"
   (("w" count-words "count words"))))

;; web mode
(use-package web-mode
  :ensure t
  :mode
  (
   ".html$"
   ".js$"
   ".css$"
   )
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-indentation t
   web-mode-enable-current-column-highlight t
   web-mode-enable-current-element-highlight t
   )  
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))

;; company mode
(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))
