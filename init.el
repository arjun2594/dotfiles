
;; File path: ~/.emacs.d/init.el

(add-to-list 'load-path "~/.emacs.d/")

;; No splash-screen at the beginning, no menubar, scrollbar, toolbar, fringe

(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(fringe-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'package)
(require 'google-this)

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("original"    . "http://tromey.com/elpa/")
        ("org"         . "http://orgmode.org/elpa/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;Initialize packages
(package-initialize)

;;Install zen-and-art theme
(load-theme 'zen-and-art t)

;;Set ispell backends
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")

;;Install Window-number mode - Use M-1,M-2 to jump between windows
(require 'window-number)
(window-number-meta-mode)

;;Install auto-complete and autopair
(require 'auto-complete)
(require 'autopair)
(require 'auto-complete-config)
(ac-config-default)
(autopair-global-mode) ;; enable autopair in all buffers

;;Python Development Environment
;; Install 3 Python dependencies: sudo pip install jedi elpy rope
;; Install Emacs ELPA packages: elpy flymake-cursor
(add-hook 'python-mode-hook 'elpy-mode)
(highlight-indentation-mode -1)
(add-hook 'python-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))
(setenv "PYTHONPATH" "/usr/bin/python")

;;C/C++ Development Enviroment
;;Install ELPA packages: autocomplete and configure it (already done)
;;Install ELPA packages: yasnippet iedit (Will done while installing elpy)
;;Install ELPA packages: flymake-google-c++ && pip install cpplint
;;Install ELPA package: google-c-style
;;Install auto-complete-c-headers
;;Install ELPA package yasnippet for snippet expansion
(require 'yasnippet)
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
 )
(defun my:flymake-google-init () 
  (require 'flymake-google-cpplint)
  (flymake-google-cpplint-load)
)

(add-hook 'c-mode-hook 'my:ac-c-header-init)
(add-hook 'c++-mode-hook 'my:ac-c-header-init)

(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Turn on semantic mode
(semantic-mode 1)
(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic)
)

(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; ORG-MODE
;; Spellcheck my org mode files.
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;;Change the snippet expansion and iedit keybindings
(define-key yas-minor-mode-map (kbd "C-c ;") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)


;; Additional key-bindings
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "\C-x m") 'execute-extended-command)
(global-set-key (kbd "\C-c m") 'execute-extended-command) 
(global-set-key [f11] 'fullscreen)
(global-set-key (kbd "C-x p") 'package-list-packages-no-fetch)

;;Tramp for editing protected files in existing Emacs session.(C-x C-f /sudo)
(require 'tramp)
(setq tramp-default-method "ssh")

;;Fullscreen mode - Press M-x fullscreen for switching to Fullscreen mode.
(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;; Sometimes the above function may not work. (Gnome Shell 3.8)
;;initialize fullscreen at startup
(fullscreen)

(require 'powerline)
(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)
(powerline-default-theme)


(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;;Maximize Screen
(defun maximize (&optional f)
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(custom-set-variables


















 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4eaad15465961fd26ef9eef3bee2f630a71d8a4b5b0a588dc851135302f69b16" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
