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
  :init (advice-add 'python-mode :before 'elpy-enable))

(use-package ace-window
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
  "1" '(ame-window 1 :which-key "select window 1")
  "2" '(ace-window 2 :which-key "select window 2")
  
  
  ;; application settings
  "a"  '(:ignore t :which-key "applications")
  "at" '(ansi-term :which-key "terminal")
  "ad" '(dired :which-key "dired")
  
  ;; buffer bindings
  "b"  '(:ignore t :which-key "buffer")
  "bb" '(ivy-switch-buffer :which-key "switch buffer")
  "be" '(eval-buffer :which-key "eval buffer")

  ;; emacs tools
  "e" '(:ignore t :which-key "emacs")
  "er"   '(my/reload-config :which-key "reload emacs config")
  
  ;;file bindings
  "f"  '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "recent file")

  ;;help bindings
  "h"  '(:ignore t :which-key "help")
  "hf" '(counsel-describe-function :which-key "describe function")
  "hv" '(counsel-describe-variable :which-key "describe variable")
  "hi" '(counsel-info-lookup-symbol :which-key "lookup info for symbol")
  "hu" '(counsel-unicode-char :which-key "unicode character")

  ;;ui bindings
  "u"  '(:ignore t :which-key "UI settings")
  "ut" '(counsel-load-theme :which-key "Load theme")

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
))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elpy zenburn-theme which-key use-package helm general evil doom-themes counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
