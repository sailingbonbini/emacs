(scroll-bar-mode -1)
;; Minimal UI
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(show-paren-mode)
(global-display-line-numbers-mode t)
(setq package-enable-at-startup nil)

;; stick custom varibales in their own files
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; automatically revert buffers on file change
(global-auto-revert-mode 1)

;; auto update dired buffers as well
(setq global-auto-revert-non-file-buffers t)

;; push .emacs.d/local to load path
(push (expand-file-name "~/.emacs.d/local") load-path)

;; use left option key as super key
(setq mac-option-modifier 'super)

;; store local emacs directory
(setq td-local-emacs-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; make sure emacs PATH matches shell PATH
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



;; use straight.el package manager
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
  (load bootstrap-file nil))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defun td/reload-config()
  (interactive)
  (load-file (concat td-local-emacs-dir "init.el")))

(defun td/set-olivetti-org-faces()
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

(defun td/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(use-package modus-themes
  :straight
  '(modus-themes :type git :host github :repo "protesilaos/modus-themes")
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only)
        modus-themes-paren-match '(bold intense)
	modus-themes-mode-line '(accented borderless padded))
  (load-theme 'modus-vivendi t)
  :config
  (enable-theme 'modus-vivendi)
  :bind ("<f5>" . modus-themes-toggle))


;; ivy completion/switcher etc
(use-package ivy
  :straight
  '(ivy :type git :host github :repo "abo-abo/swiper")
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t))
 
(use-package counsel
  :straight
  '(counsel :type git :host github :repo "abo-abo/swiper")
  )

(use-package swiper
  :straight
  '(swiper :type git :host github :repo "abo-abo/swiper")
  )

;; Which Key
(use-package which-key
  :straight
  '(which-key :type git :host github :repo "justbur/emacs-which-key")
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package winum
  :straight
  '(winum :type git :host github :repo "deb0ch/emacs-winum")
  :config
  (winum-mode))

(use-package general
  :straight
  '(general :type git :host github :repo "noctuid/general.el")
  :config
  ;; global key config
  (general-define-key
   "s-," 'beginning-of-buffer
   "s-." 'end-of-buffer
   "C-c c" 'counsel-org-capture
   "C-x C-d" 'td/duplicate-line
   "C-c r" 'councel-recentf
   "C-c e" 'eval-buffer
   "C-x C-r" 'counsel-recentf
   )

  ;; ORG mode key config
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

  ;; window switch
  (general-define-key
   "M-1" 'winum-select-window-1
   "M-2" 'td/split-window-2-and-switch
   "M-3" 'td/split-window-3-and-switch
   "M-4" 'winum-select-window-4
   "M-5" 'winum-select-window-5)
  )

(use-package term-toggle
  :straight
  '(term-toggle :type git :host github :repo "amno1/emacs-term-toggle")
  :config
  (define-key global-map [f2] #'term-toggle-eshell))

;; enable flyspell for all modes that derive from text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq ispell-program-name "/opt/homebrew/bin/ispell")
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; distraction free writing mode
(use-package olivetti
  :straight
  '(olivetti :type git :host github :repo "rnkn/olivetti")
  :config
  (add-hook 'olivetti-mode-hook 'td/set-olivetti-org-faces))

;;typewriter kind of scrolling
(use-package centered-cursor-mode
  :straight
  '(centered-cursor-mode :type git :host github :repo "andre-r/centered-cursor-mode.el"))

;; allows to display word counts on subtrees etc
(use-package org-wc
  :straight
  '(org-wc :type git :host github :repo "tesujimath/org-wc"))



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

;; a function to move a buffer to a window by number
(defun td/move-buffer-to-window (window-number)
  "Move the current buffer to the specified window."
  (interactive "cMove buffer to window: ")
  (let ((target-window (nth (- window-number (string-to-char "0")) (window-list))))
    (if target-window
        (set-window-buffer target-window (current-buffer))
      (error "Invalid window number"))))



(defun td/load-profile ()
  (interactive)
  (find-file "~/.bash_profile"))

(straight-use-package 'flycheck
  :straight)

(setq ns-command-modifier 'meta)
(setq ns-function-modifier 'hyper)
;;   ns-option-modifier 'meta
;;   ns-control-modifier 'super
;;   ns-function-modifier 'control)


;; web mode
(use-package web-mode
  :straight
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
  :straight
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))
