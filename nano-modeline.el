;;; nano-modeline.el --- N Λ N O Modeline -*- lexical-binding: t -*-
;; -------------------------------------------------------------------
;; GNU Emacs / N Λ N O Modeline
;; Copyright (C) 2020-2021 - N Λ N O developers 
;;
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/rougier/nano-modeline
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;;
;;; Commentary:
;; -------------------------------------------------------------------
;;
;; Modeline is rendered as:
;; [ status | name (primary)                               secondary ]
;;
;; -------------------------------------------------------------------
;;
;;; Code:

(require 's)
(require 'cl-lib)

(declare-function transient--show "ext:transient")
(declare-function pdf-view-current-page "ext:pdf-view")
(declare-function pdf-cache-number-of-pages "ext:pdf-view")

(defgroup nano-modeline nil
  "Modeline settings"
  :group 'nano)

(defgroup nano-modeline-active nil
  "Active modeline faces.

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'nano-modeline)

(defgroup nano-modeline-inactive nil
  "Inactive modeline faces

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'nano-modeline)

(defvar nano-modeline--active-window nil
  "Currently selected window")

(defface nano-modeline-active nil
  "Modeline face for active modeline"
  :group 'nano-modeline-active)

(defface nano-modeline-active-name nil
  "Modeline face for active name element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-primary nil
  "Modeline face for active primary element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-secondary nil
  "Modeline face for active secondary element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-RO nil
  "Modeline face for active READ-ONLY element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-RW nil
  "Modeline face for active READ-WRITE element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-** nil
  "Modeline face for active MODIFIED element"
  :group 'nano-modeline-active)

(defface nano-modeline-inactive nil
  "Modeline face for inactive window"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-name nil
  "Modeline face for inactive name element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-primary nil
  "Modeline face for inactive primary element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-secondary nil
  "Modeline face for inactive primary element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-RO nil
  "Modeline face for inactive READ-ONLY element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-RW nil
  "Modeline face for inactive READ-WRITE element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-** nil
  "Modeline face for inactive MODIFIED element"
  :group 'nano-modeline-inactive)

;; Core
(defsubst nano-modeline-format (key)
  "Return a mode-line configuration associated with KEY (a symbol).
Throws an error if it doesn't exist."
  (let ((fn (intern-soft (format "nano-modeline-%s-mode" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun nano-modeline-set-modeline! (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let ((modeline (nano-modeline-format key)))
    (setf (if default
              (default-value 'header-line-format)
            (buffer-local-value 'header-line-format (current-buffer)))
          (list modeline))))

(defun nano-modeline-set-hook! (hooks name)
  "Set the modeline to NAME on HOOKS. "
  (let ((fn (intern (format "nano-modeline-set-%s-format-h" name))))
    (fset fn
          (lambda (&rest _) (nano-modeline-set-modeline! name)))
    (if (listp hooks)
        (dolist (hook hooks)
          (add-hook hook fn))
      (add-hook hooks fn))))

(defsubst vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                             (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defsubst nano-mode-name ()
  (format-mode-line mode-name))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun nano-modeline-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

(defsubst nano-modeline-active ()
  "Return non-nil if the selected window has an active modeline."
  (eq (selected-window) nano-modeline--active-window))

(defun nano-modeline-compose (name primary secondary &optional status)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (active        (nano-modeline-active))
         (dedicated     (window-dedicated-p))
         (space-up       +0.20)
         (space-down     -0.25)
         (prefix
          (cond (status
                 (propertize status
                             'face
                             (if active 'nano-modeline-active-status-**
                               'nano-modeline-inactive-status-**)))
                ((and buffer-file-name (buffer-modified-p))
                 (propertize (if dedicated" -- " " ** ")
                             'face (if active 'nano-modeline-active-status-**
                                     'nano-modeline-inactive-status-**)))
                (buffer-read-only
                 (propertize (if dedicated" -- " " RO ")
                             'face (if active 'nano-modeline-active-status-RO
                                     'nano-modeline-inactive-status-RO)))
                (t
                 (propertize (if dedicated" -- " " RW ")
                             'face (if active 'nano-modeline-active-status-RW
                                     'nano-modeline-inactive-status-RW)))))
         (left (concat
                (propertize " "  'face (if active 'nano-modeline-active
                                         'nano-modeline-inactive)
                            'display `(raise ,space-up))
                (propertize name 'face (if active 'nano-modeline-active-name
                                         'nano-modeline-inactive-name))
                (propertize " "  'face (if active 'nano-modeline-active
                                         'nano-modeline-inactive)
                            'display `(raise ,space-down))
                (propertize primary 'face (if active 'nano-modeline-active-primary
                                            'nano-modeline-inactive-primary))))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
                             (length prefix) (length left) (length right)
                             (/ (window-right-divider-width) char-width)))
         (available-width (max 1 available-width)))
    (concat prefix
            left
            (propertize (make-string available-width ?\ )
                        'face (if active 'nano-modeline-active
                                'nano-modeline-inactive))
            (propertize right 'face (if active 'nano-modeline-active-secondary
                                      'nano-modeline-inactive-secondary)))))

;; ---------------------------------------------------------------------
;; (defun nano-modeline-mu4e-dashboard-mode-p ()
;;   (bound-and-true-p mu4e-dashboard-mode))

;; (defun nano-modeline-mu4e-dashboard-mode ()
;;   (nano-modeline-compose (nano-modeline-status)
;;                          "Mail"
;;                          (nano-modeline-mu4e-context)
;;                          (format "%d messages" (plist-get mu4e~server-props :doccount))))

;; ---------------------------------------------------------------------
;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the nano-modeline function to set
;; the header format in a notebook buffer.  Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.
;; (with-eval-after-load 'ein
;;   (defun nano-modeline-ein-notebook-mode ()
;;     (let ((buffer-name (format-mode-line "%b")))
;;       (nano-modeline-compose (if (ein:notebook-modified-p) "**" "RW")
;;                              buffer-name
;;                              ""
;;                              (ein:header-line))))
;;   (setq ein:header-line-format '((:eval (nano-modeline-ein-notebook-mode)))))

;; ---------------------------------------------------------------------
;; (defun nano-modeline-elfeed-search-mode-p ()
;;   (derived-mode-p 'elfeed-search-mode))

;; (defun nano-modeline-elfeed-search-mode ()
;;   (nano-modeline-compose (nano-modeline-status)
;;                          "Elfeed"
;;                          (concat "(" (elfeed-search--header)  ")")
;;                          ""))

;; ;; Elfeed (regular header)
;; (with-eval-after-load 'elfeed
;;   (defun elfeed-setup-header ()
;;     (setq header-line-format (default-value 'header-line-format)))
;;   (setq elfeed-search-header-function #'elfeed-setup-header))

;; ---------------------------------------------------------------------
;; (defun nano-modeline-elfeed-show-mode-p ()
;;   (derived-mode-p 'elfeed-show-mode))

;; (defun nano-modeline-elfeed-show-mode ()
;;   (let* ((title        (elfeed-entry-title elfeed-show-entry))
;;          (tags         (elfeed-entry-tags elfeed-show-entry))
;;          (tags-str     (mapconcat #'symbol-name tags ", "))
;;          (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
;;          (feed         (elfeed-entry-feed elfeed-show-entry))
;;          (feed-title   (plist-get (elfeed-feed-meta feed) :title))
;;          (entry-author (elfeed-meta elfeed-show-entry :author)))
;;     (nano-modeline-compose (nano-modeline-status)
;;                            (s-truncate 40 title "…")
;;                            (concat "(" tags-str ")")
;;                            feed-title)))

;; ---------------------------------------------------------------------
;; (defun nano-modeline-calendar-mode-p ()
;;   (derived-mode-p 'calendar-mode))

;; (defun nano-modeline-calendar-mode () "")

;; ;; Calendar (no header, only overline)
;; (with-eval-after-load 'calendar
;;   (defun calendar-setup-header ()
;;     (setq header-line-format "")
;;     (face-remap-add-relative
;;      'header-line `(:overline ,(face-foreground 'default)
;;                     :height 0.5
;;                     :background ,(face-background 'default))))
;;   (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

;;   ;; From https://emacs.stackexchange.com/questions/45650
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx string-start "*Calendar*" string-end)
;;                  (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode ()
  (nano-modeline-compose "Capture"
                         "(org)"
                         ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(defun nano-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
        (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
        line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                                 crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
             (if (not (equal node "Top")) node
               (format "%s"
                       (if (stringp Info-current-file)
                           (file-name-sans-extension
                            (file-name-nondirectory Info-current-file))
                         Info-current-file)))))
        (setq line (concat line (if (null line) "" " > ")
                           (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun nano-modeline-info-mode ()
  (nano-modeline-compose "Info"
                         (concat "("
                                 (nano-modeline-info-breadcrumbs)
                                 ")")
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-agenda-mode ()
  (nano-modeline-compose "Agenda"
                         ""
                         (format-time-string "%A %-e %B %Y")))

;; ---------------------------------------------------------------------
(defun nano-modeline-term-mode ()
  (nano-modeline-compose "Terminal"
                         (concat "(" shell-file-name ")")
                         (nano-modeline-shorten-directory default-directory 32)
                         " >_ "))

;; ---------------------------------------------------------------------
;; (defun nano-modeline-mu4e-main-mode-p ()
;;   (derived-mode-p 'mu4e-main-mode))

;; (defun nano-modeline-mu4e-main-mode ()
;;   (nano-modeline-compose (nano-modeline-status)
;;                          "Mail"
;;                          (nano-modeline-mu4e-context)
;;                          (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
;; (defun nano-modeline-mu4e-headers-mode-p ()
;;   (derived-mode-p 'mu4e-headers-mode))

;; (defun nano-modeline-mu4e-headers-mode ()
;;   (nano-modeline-compose (nano-modeline-status)
;;                          (mu4e~quote-for-modeline mu4e~headers-last-query)
;;                          ""
;;                          ""))

;; (with-eval-after-load 'mu4e
;;   (defun mu4e~header-line-format () (nano-modeline)))

;; ---------------------------------------------------------------------
;; (setq mu4e-modeline-max-width 72)

;; (defun nano-modeline-mu4e-view-mode-p ()
;;   (derived-mode-p 'mu4e-view-mode))

;; (defun nano-modeline-mu4e-view-mode ()
;;   (let* ((msg     (mu4e-message-at-point))
;;          (subject (mu4e-message-field msg :subject))
;;          (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
;;          (date     (mu4e-message-field msg :date)))
;;     (nano-modeline-compose (nano-modeline-status)
;;                            (s-truncate 40 subject "…")
;;                            ""
;;                            from)))

;; (defun nano-modeline-mu4e-view-hook ()
;;   (setq header-line-format "%-")
;;   (face-remap-add-relative 'header-line
;;                            '(:background "#ffffff"
;;                                          :underline nil
;;                                          :box nil
;;                                          :height 1.0)))
;; (add-hook 'mu4e-view-mode-hook #'nano-modeline-mu4e-view-hook)
;; ---------------------------------------------------------------------
(defun nano-modeline-message-mode ()
  (nano-modeline-compose "Message" "(draft)" ""))


;; ---------------------------------------------------------------------
;; (setq org-mode-line-string nil)
;; (with-eval-after-load 'org-clock
;;   (add-hook 'org-clock-out-hook
;;             '(lambda () (setq org-mode-line-string nil)
;;                (force-mode-line-update))))

;; (defun nano-modeline-org-clock-mode-p ()
;;   org-mode-line-string)

;; (defun nano-modeline-org-clock-mode ()
;;   (let ((buffer-name (format-mode-line "%b"))
;;         (mode-name   (nano-mode-name))
;;         (branch      (vc-branch))
;;         (position    (format-mode-line "%l:%c")))
;;     (nano-modeline-compose buffer-name
;;                            (concat "(" mode-name
;;                                    (if branch (concat ", "
;;                                                       (propertize branch 'face 'italic)))
;;                                    ")" )
;;                            org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun nano-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (nano-mode-name))
        (branch      (vc-branch))
        (page-number (concat
                      (number-to-string (pdf-view-current-page)) "/"
                      (or (ignore-errors
                            (number-to-string (pdf-cache-number-of-pages)))
                          "???"))))
    (nano-modeline-compose
     buffer-name
     (concat "(" mode-name
             (if branch (concat ", "
                                (propertize branch 'face 'italic)))
             ")" )
     page-number
     "RW")))

;; ---------------------------------------------------------------------
(defun nano-modeline-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (nano-mode-name))
        (position    (format-mode-line "%l:%c")))
    (nano-modeline-compose buffer-name "" position)))
(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'nano-subtle))))
;; ---------------------------------------------------------------------
;; (defun nano-modeline-completion-list-mode-p ()
;;   (derived-mode-p 'completion-list-mode))

(defun nano-modeline-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (nano-mode-name))
        (position    (format-mode-line "%l:%c")))

    (nano-modeline-compose buffer-name "" position)))
;; ---------------------------------------------------------------------
;; (with-eval-after-load 'deft
;;   (defun deft-print-header ()
;;     (force-mode-line-update)
;;     (widget-insert "\n")))

;; (defun nano-modeline-deft-mode-p ()
;;   (derived-mode-p 'deft-mode))

;; (defun nano-modeline-deft-mode ()
;;   (let ((prefix " RO ")
;;         (primary "Notes")
;;         (filter  (if deft-filter-regexp
;;                      (deft-whole-filter-regexp) "<filter>"))
;;         (matches (if deft-filter-regexp
;;                      (format "%d matches" (length deft-current-files))
;;                    (format "%d notes" (length deft-all-files)))))
;;     (nano-modeline-compose prefix primary filter matches)))


;; ---------------------------------------------------------------------
(defun nano-modeline-default-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (nano-mode-name))
        (branch      (vc-branch))
        (position    (format-mode-line "%l:%c")))
    (nano-modeline-compose buffer-name
                           (concat "(" mode-name
                                   (if branch (concat ", "
                                                      (propertize branch 'face 'italic)))
                                   ")" )
                           position)))

;; ---------------------------------------------------------------------
;; (defun nano-modeline-mu4e-context ()
;;   "Return the current mu4e context as a non propertized string."

;;   (if (> (length (mu4e-context-label)) 0)
;;       (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
;;     "(none)"))


;; ---------------------------------------------------------------------
(defun nano-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)

  ;; This hooks is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'pre-redisplay-functions #'nano-modeline-set-selected-window-h)

  (setq eshell-status-in-modeline nil)
  (setq-default mode-line-format (list ""))
  (defun nano-modeline-transient-respect-header-line
      (orig-fn &rest args)
    (let ((erase-buffer
          (symbol-function
           (function erase-buffer))))
      (cl-letf
          (((symbol-function
             (function erase-buffer))
            (lambda nil
              (funcall erase-buffer)
              (setq header-line-format nil))))
        (ignore erase-buffer)
        (apply orig-fn args))))

  (advice-add #'transient--show
              :around
              #'nano-modeline-transient-respect-header-line)

  (nano-modeline-set-modeline! 'default t)
  (nano-modeline-set-hook! 'message-mode-hook 'message)
  (nano-modeline-set-hook! 'Info-mode-hook 'info)
  (setq Info-use-header-line nil)
  (nano-modeline-set-hook! 'org-capture-mode-hook 'org-capture)
  (nano-modeline-set-hook! 'org-agenda-mode-hook 'org-agenda)
  (nano-modeline-set-hook! '(vterm-mode-hook term-mode-hook) 'term)
  (nano-modeline-set-hook! 'pdf-view-mode-hook 'pdf-view)
  (nano-modeline-set-hook! 'completion-list-mode-hook 'completion-list)
  (add-hook 'Buffer-menu-mode-hook
            #'buffer-menu-mode-header-line)
  (setq Buffer-menu-use-header-line nil)
  (nano-modeline-set-hook! 'buffer-menu-mode-hook 'buffer-menu))



(defun nano-modeline-set-selected-window-h (&rest _)
  "Track the active modeline's window in `nano-modeline--active-window'."
  (let ((win (selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq nano-modeline--active-window (frame-selected-window)))))

(provide 'nano-modeline)
;;; nano-modeline.el ends here
