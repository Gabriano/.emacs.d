;; Load paths
(defvar *emacs-dir* (file-name-as-directory "~/.emacs.d"))

(add-to-list 'load-path *emacs-dir*)
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(let ((paths '("site-lisp")))
  (dolist (path paths)
    (add-to-list 'load-path (concat *emacs-dir* path))))

(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "/usr/local/Gambit-C/bin/gsi -:t")

;; gambit scheme
(require 'gambit)

;; Py
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))
(require 'prolog)

;; General tab width
(setq-default tab-width 2)

;; markdown
(autoload 'markdown-mode "markdown-mode/markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; Emacs server
(server-start)

;; Load ELPA
(require 'package)
(package-initialize)

;; Enable mouse wheel
(mouse-wheel-mode 1)

;; Buffer switch mode
;; http://www.emacswiki.org/emacs/IswitchBuffers
(iswitchb-mode 1)

;; UTF-8
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Syntax highlighting
(global-font-lock-mode 1)

;; Cursor color
(set-cursor-color "Black")

;; General settings
;; http://homepages.inf.ed.ac.uk/s0243221/emacs/
(setq user-full-name "Eric Thivierge"
      user-mail-address "thiveri@gmail.com"
      inhibit-startup-message t
      initial-scratch-message nil
      default-major-mode 'fundamental-mode
      truncate-partial-width-windows t
      next-line-add-newlines nil
      scroll-step 1
      scroll-conservatively 1
      font-lock-maximum-decoration t
      require-final-newline t
      truncate-partial-width-windows nil
      shift-select-mode nil
      echo-keystrokes 0.1
      x-select-enable-clipboard t
      custom-unlispify-tag-names nil
      ring-bell-function '(lambda ())
      mouse-wheel-mode t
      make-backup-files t
      version-control t
      backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

(setenv "EDITOR" "emacsclient")

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
                 '(progn
                   (color-theme-initialize)
                   (color-theme-hober)))

;; Indentation is 4 spaces.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Display line, column and time (24h format)
(line-number-mode t)
(column-number-mode t)
(display-time)
(setq display-time-24hr-format t)

;; Show paren matches
(show-paren-mode t)

;; Scheme
(setq scheme-program-name "gsi")

;; Magit
(require 'magit)

;; Buffer switching
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

;; My macros

;; Insert java "System.out.println("");" and move in the quotes
(fset 'sop
   [?S ?y ?s ?t ?e ?m ?. ?o ?u ?t ?. ?p ?r ?i ?n ?t ?l ?n ?\( ?\" ?\" ?\) ?\; left left left])

; My custom keybindings

(global-set-key (kbd "M-<f1>") 'switch-to-previous-buffer)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<del>") 'delete-char)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;;(global-set-key (kbd "M-#") 'dabbrev-expand)
(global-set-key (kbd "M-#") 'hippie-expand)
(global-set-key (kbd "C-x C-x") 'other-window)
(global-set-key (kbd "C-M-)") 'delete-window)
(global-set-key (kbd "C-M-K") 'kill-buffer)
(global-set-key (kbd "C-x C-o") 'exchange-point-and-mark)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x m") '(lambda ()
                                 (interactive)
                                 (if (get-buffer "*ansi-term*")
                                     (switch-to-buffer-other-window "*ansi-term*")
                                   (ansi-term (getenv "SHELL")))))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key (kbd "C-o")             ; Works like O in vi
                '(lambda ()
                   (interactive)
                   (beginning-of-line)
                   (open-line 1)
                   (indent-according-to-mode)))

(global-set-key (kbd "C-x C-j")
                '(lambda ()
                   (interactive)
                   (join-line 1)))


(global-set-key (kbd "C-M-S-i")
                '(lambda ()
                   "Open ~/.emacs.d/init.el"
                   (interactive)
                   (find-file "~/.emacs.d/init.el")))


(global-set-key (kbd "C-x t")
                '(lambda ()
                   (interactive)
                   (find-file *todo-file*)))


(provide 'keybindings)


(setq-default fill-column 78)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(require 'desktop)

(defvar my-desktop-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "*Directory to save desktop sessions in")

(defvar my-desktop-session-name-hist nil
  "Desktop session name history")

(defun my-desktop-save (&optional name)
  "Save desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session" t)))
  (when name
    (make-directory (concat my-desktop-session-dir name) t)
    (desktop-save (concat my-desktop-session-dir name) t)))

(defun my-desktop-save-and-clear ()
  "Save and clear desktop."
  (interactive)
  (call-interactively 'my-desktop-save)
  (desktop-clear)
  (setq desktop-dirname nil))

(defun my-desktop-read (&optional name)
  "Read desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session")))
  (when name
    (desktop-clear)
    (desktop-read (concat my-desktop-session-dir name))))

(defun my-desktop-change (&optional name)
  "Change desktops by name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (when name
      (my-desktop-save name))
    (call-interactively 'my-desktop-read)))

(defun my-desktop-name ()
  "Return the current desktop name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (if name
        (message (concat "Desktop name: " name))
      (message "No named desktop loaded"))))

(defun my-desktop-get-current-name ()
  "Get the current desktop name."
  (when desktop-dirname
    (let ((dirname (substring desktop-dirname 0 -1)))
      (when (string= (file-name-directory dirname) my-desktop-session-dir)
        (file-name-nondirectory dirname)))))

(defun my-desktop-get-session-name (prompt &optional use-default)
  "Get a session name."
  (let* ((default (and use-default (my-desktop-get-current-name)))
         (full-prompt (concat prompt (if default
                                         (concat " (default " default "): ")
                                       ": "))))
    (completing-read full-prompt (and (file-exists-p my-desktop-session-dir)
                                      (directory-files my-desktop-session-dir))
                     nil nil nil my-desktop-session-name-hist default)))

(defun my-desktop-kill-emacs-hook ()
  "Save desktop before killing emacs."
  (when (file-exists-p (concat my-desktop-session-dir "last-session"))
    (setq desktop-file-modtime
          (nth 5 (file-attributes (desktop-full-file-name (concat my-desktop-session-dir "last-session"))))))
  (my-desktop-save "last-session"))

(add-hook 'kill-emacs-hook 'my-desktop-kill-emacs-hook)
