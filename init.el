;; File path: ~/.emacs.d/init.el

(add-to-list 'load-path "~/.emacs.d/scripts/")
(load "~/.emacs.d/scripts/amz-search.el")
  
;; No splash-screen at the beginning, no menubar, scrollbar, toolbar, fringe
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(fringe-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;Display time in modeline
(display-time)

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'package)

;;Set package locations
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("original"    . "http://tromey.com/elpa/")
        ("org"         . "http://orgmode.org/elpa/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;Initialize packages
(package-initialize)

;;Initialize smex
(require 'smex)
(global-set-key "\M-x" 'smex)
(smex-initialize)

;;Load tango-dark theme
(load-theme 'tango-dark t)

;;Change the colour of the mode line
(set-face-foreground 'mode-line "Green")
(set-face-background 'mode-line "Ash")

;;Set ispell backends
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")

;;emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)


;;Install Window-number mode - Use M-1,M-2 to jump between windows
(require 'window-number)
(window-number-meta-mode)

;;Install auto-complete and autopair
(require 'auto-complete)
(require 'autopair)
(require 'auto-complete-config)
(ac-config-default)
(autopair-global-mode) ;; enable autopair in all buffers

;;Setup the backup directory location
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;;Google This
(require 'google-this)
(google-this-mode t)

;;i-edit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

;;Spellcheck my org mode files automatically
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;;Closing Timestamp for org-todo
(setq org-log-done 'time)

;; Additional key-bindings
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Sometimes the above function may not work. (Gnome Shell 3.8)	    ;;
;; ;;initialize fullscreen at startup					    ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	    ;;
;; ;; (defun switch-full-screen ()				       ;;   ;;
;; ;;   (interactive)					       ;;	    ;;
;; ;;   (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")) ;;	    ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(global-set-key [f11] 'fullscreen)
(fullscreen)

;;Maximize Screen
(defun maximize (&optional f)
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;From the default configuration
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


;;Firefox Browse URL 
(defun browse-url-firefox (url &optional new-window)
    "Ask the Firefox WWW browser to load URL.
  Default to the URL around or before point.  The strings in
  variable `browse-url-firefox-arguments' are also passed to
  Firefox.
  
  When called interactively, if variable
  `browse-url-new-window-flag' is non-nil, load the document in a
  new Firefox window, otherwise use a random existing one.  A
  non-nil interactive prefix argument reverses the effect of
  `browse-url-new-window-flag'.
  
  If `browse-url-firefox-new-window-is-tab' is non-nil, then
  whenever a document would otherwise be loaded in a new window, it
  is loaded in a new tab in an existing window instead.
  
  When called non-interactively, optional second argument
  NEW-WINDOW is used instead of `browse-url-new-window-flag'."
    (interactive (browse-url-interactive-arg "URL: "))
    (setq url (browse-url-encode-url url))
    (let* ((process-environment (browse-url-process-environment))
           (window-args (if (browse-url-maybe-new-window new-window)
                            (if browse-url-firefox-new-window-is-tab
                                '("-new-tab")
                              '("-new-window"))))
           (ff-args (append browse-url-firefox-arguments window-args (list url)))
           (process-name (concat "firefox " url))
           (process (apply 'start-process process-name nil
                           browse-url-firefox-program ff-args) )) ))
