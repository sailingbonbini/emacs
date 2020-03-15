;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(blink-cursor-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defun my/reload-config()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

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

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; doom theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

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
  (setq python-indent-offset 2)                                                   ;; offset 2
  (setq elpy-rpc-python-command "/usr/local/opt/python/libexec/bin/python")       ;; use python 3.8
  (setq python-shell-interpreter "/usr/local/opt/python/libexec/bin/python")      
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
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "/" '(swiper :which-key "Search for string")
  "TAB" '(switch-to-other-buffer :which-key "other buffer")
  "SPC" '(counsel-M-x :which-key "M-x")
  ">" '(comment-line :which-key "comment line")
  
  ;; application settings
  "a"  '(:ignore t :which-key "applications")
  "at" '(ansi-term :which-key "terminal")
  "ad" '(dired :which-key "dired")
  
  ;; buffer bindings
  "b"  '(:ignore t :which-key "buffer")
  "bb" '(ivy-switch-buffer :which-key "switch buffer")
  "be" '(eval-buffer :which-key "eval buffer")
  "bd" '(kill-buffer :which-key "delete buf")

  ;; emacs tools
  "e" '(:ignore t :which-key "emacs")
  "er"   '(my/reload-config :which-key "reload emacs config")
  
  ;;file bindings
  "f"  '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "recent file")
  "fp" '(td-load-profile :which-key "find bash profile")

  ;;help bindings
  "h"  '(:ignore t :which-key "help")
  "hf" '(counsel-describe-function :which-key "describe function")
  "hv" '(counsel-describe-variable :which-key "describe variable")
  "hi" '(counsel-info-lookup-symbol :which-key "lookup info for symbol")
  "hu" '(counsel-unicode-char :which-key "unicode character")

  ;;ui bindings
  "u"  '(:ignore t :which-key "UI settings")
  "ut" '(counsel-load-theme :which-key "Load theme")
  "ul" '(global-linum-mode :which-key "Line numbers")

  ;; Window
  "w"  '(:ignore t :which-key "window")
  "wr"  '(windmove-right :which-key "move right")
  "ws"  '(ivy-push-view :which-key "safe view")
  "wd"  '(ivy-pop-view :which-key "delete view")
  "wl"  '(windmove-left :which-key "move left")
  "wu"  '(windmove-up :which-key "move up")
  "wb"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wd"  '(delete-window :which-key "delete window")
  "wm"  '(delete-other-windows :which-key "maximize window")
))

(general-define-key
 "s-1" 'winum-select-window-1
 "s-2" 'winum-select-window-2
 "s-3" 'winum-select-window-3
 "s-4" 'winum-select-window-4
 "s-5" 'winum-select-window-5)

(defun td-load-profile ()
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-window-display-mode t)
 '(package-selected-packages
   (quote
    (winum flycheck which-key use-package nlinum major-mode-hydra general evil elpy doom-themes counsel ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
