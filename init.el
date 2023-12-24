;;; Copyright (c) 2020-2023  Debarghya Datta <info@devildev.me>

;; Author: Debarghya Datta <info@devildev.me>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;  -----------------------------------------
;;  Do not edit the generated file, as it has
;;  been generated, as a tangled file, by the
;;  stupendous org-mode.
;;
;;  Make the changes in the corresponding
;;  dotemacs.org file, instead.
;;  -----------------------------------------

;;; Code:

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; Path to custom modules (mandatory)
(dolist (path '("lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(package-refresh-contents t) ; refresh async
(package-initialize) ; initialize

(require 'use-package-ensure)
(setq use-package-always-ensure t) ; always ensure installed

;; General: Making Keybindings easy
(use-package general
  :config
  (general-evil-setup t) ; I us eVIl
  )

;; Faces
(set-face-attribute 'default nil :family "Fira Code" :height 130)
(set-face-attribute 'italic nil :family "Hack")
(set-face-attribute 'bold nil :weight 'semibold)
;; Glyphs
(defface fallback '((t :family "Fira Code"
                       :inherit 'nano-face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                         (make-glyph-code ?↩ 'fallback))

;; Theme
(defun my-ef-themes-custom-faces ()
  "My customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
  (ef-themes-with-colors
   (custom-set-faces

    ;; mode-line
    `(mode-line ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-main :underline ,bg-mode-line)))
    `(mode-line-inactive ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-main :underline ,bg-alt)))

    ;; nano-headline
    `(nano-modeline-active ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-mode-line :foreground ,fg-mode-line)))
    `(nano-modeline-primary ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-mode-line :foreground ,modeline-info)))
    `(nano-faded-inactive ((,c :inherit nano-modeline-active :background ,bg-cyan-subtle :forground ,fg-dim)))
    `(nano-modeline-status ((,c :inherit default :background ,bg-cyan-subtle :foreground ,fg-mode-line))))))

(add-hook 'ef-themes-post-load-hook #'my-ef-themes-custom-faces)

(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-to-toggle '(ef-spring ef-night))
  :config
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-spring))

;; Layout
(require 'disp-table)
(setq default-frame-alist
      (append (list
	       '(min-height . 1)
	       '(height     . 45)
	       '(min-width  . 1)
	       '(width      . 81)
	       '(vertical-scroll-bars . nil)
	       '(internal-border-width . 24)
	       '(left-fringe    . 1)
	       '(right-fringe   . 1)
	       '(tool-bar-lines . 0)
	       '(menu-bar-lines . 0))))
(require 'nano-modeline)		; modeline + headerline
;; Writer mode
(require 'writer)			; writer mode for ORG

(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))

;; Dashboard
(use-package dashboard
  :custom
  (dashboard-banner-logo-title " Emacs made easy" "set the title")
  (dashboard-startup-banner "~/.config/emacs/lisp/banner.txt" "Banner setup")
  (dashboard-projects-backend 'project-el "set the project backend")
  (dashboard-items 
   '((projects . 2)
     (recents  . 3)
     (agenda . 3)) "Item numbers")
  (dashboard-week-agenda nil "show weekly agenda")
  (add-to-list 'dashboard-items '(agenda) t)
  (dashboard-set-footer nil "set footer")
  (dashboard-set-init-info nil "set init time")
  (dashboard-show-shortcuts nil "shortcut `jump' indicators")
  (dashboard-center-content t "center dashboard")
  (dashboard-agenda-prefix-format "%?-10b" "agenda prefix")
  (dashboard-agenda-tags-format nil "Tag (no need)")
  :custom-face
  (dashboard-text-banner ((t (:inherit fg-alt :weight bold))))
  (dashboard-banner-logo-title ((t (:inherit fg-dim :weight light :foreground "bg-main"))))
  ;; (dashboard-heading ((t (:inherit 'bold))))
  ;; (dashboard-items-face ((t (:inherit 'nano-face-default))))
  ;; (dashboard-no-items-face ((t (:inherit 'nano-face-faded))))
  ;; (dashboard-navigatir ((t (:inherit 'nano-face-faded))))
  :config
  (dashboard-setup-startup-hook))

(add-hook 'dashboard-mode-hook		; Make the dashboard clean
	  (lambda () (setq mode-line-format nil 
			   header-line-format nil)))

(add-hook 'server-after-make-frame-hook (lambda() ; Always start with *dashboard*
					  (switch-to-buffer dashboard-buffer-name)
					  (dashboard-mode)
					  (dashboard-refresh-buffer)))

;; Evil - Vim Mode
(use-package evil
  :custom 
  (evil-want-integration t "integrate it")
  (evil-want-keybinding nil "no keybinding")
  (evil-vsplit-window-right t "vsplit to right")
  (evil-split-window-below t "vsplit to below")

  (evil-want-C-u-delete nil)
  (evil-want-C-w-delete nil)
  (evil-want-C-h-delete nil)
  (evil-want-C-w-in-emacs-state nil)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-disable-insert-state-bindings t)

  (evil-want-change-word-to-end t)
  (evil-want-C-i-jump t)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-C-g-bindings nil)

  (evil-want-Y-yank-to-eol t) ; consistent with D
  (evil-want-empty-ex-last-command t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-minibuffer nil)

  :hook
  ('prog-mode . #'hs-minor-mode)	; make code folding work

  :config
  (evil-set-undo-system 'undo-redo) ; undo system
  (evil-mode))

(use-package evil-collection		; collection of key-bindings
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after evil-collection
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)) ; org-agenda keybindings

;; vertico
(use-package vertico
  :custom
  (vertico-count 8 "Options to show")
  (vertico-resize t "resize acc to the number of options")
  (vertico-count-format nil "no count indicator")
  (vertico-cycle nil "cyclic options")
  :config
  (vertico-mode 1)
  (vertico-reverse-mode 1)
  (advice-add #'vertico--format-candidate :around
	      (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
		     (propertize "» " 'face 'vertico-current)
		   "  ")
		 cand)))
  )

;; orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic) "Basic Styles")
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Marginilia
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia--ellipsis "…" "Nicer ellipsis")
  (marginalia-align 'right "right alignment")
  (marginalia-align-offset -1 "one space on the right")
  :init
  (marginalia-mode))

;; which key
;; (use-package which-key
;;   :disabled
;;   :custom
;;   (which-key-side-window-location 'bottom)
;;   (which-key-separator " → ")
;;   :init
;;   (which-key-mode))

(use-package vc
  :ensure nil
  :custom
  ;; I only use Git.  If I ever need another, I will include it here.
  ;; This may have an effect on performance, as Emacs will not try to
  ;; check for a bunch of backends.
  (vc-handled-backends '(Git))

  :config
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view))

;; Magit
(use-package magit
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  (magit-define-global-key-bindings nil)
  (magit-section-visibility-indicator '("⮧")))

(use-package dired
  :ensure nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (dired-make-directory-clickable t) ; Emacs 29.1
  (dired-free-space nil) ; Emacs 29.1
  (dired-mouse-drag-files t) ; Emacs 29.1
  (dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
        '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
          ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
		  (".*" "xdg-open")))
  :hook ((dired-mode-hook . dired-hide-details-mode)
	 (dired-mode-hook . hl-line-mode)))

;; Outline
(use-package outline
  :ensure nil
  :bind (:map outline-minor-mode-map
	      ("<tab>"   . outline-cycle)
	      ("S-<tab>" . outline-cycle-buffer)
	      ("M-j"     . outline-move-subtree-down)
	      ("M-k"     . outline-move-subtree-up)
	      ("M-h"     . outline-promote)
	      ("M-l"     . outline-demote))
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (setq-local outline-level #'outline-level)
	      (setq-local outline-regexp ";;;\\(;* \\)")
	      (setq-local outline-heading-alist
			  '((";;; " . 1)
			    (";;;; " . 2)
			    (";;;;; " . 3)
			    (";;;;;; " . 4)
			    (";;;;;;; " . 5))))))

;; Treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt) ; Auto-install
  :config
  (global-treesit-auto-mode))

;; corfu
(use-package corfu
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; Enable auto completion
  (corfu-auto-prefix 3)		 ; Min length of prefix
  (corfu-separator ?\s)          ; Orderless field separator
  (corfu-quit-at-boundary nil)   ; Never quit at completion boundary
  (corfu-quit-no-match t)      ; quit, even if there is no match
  (corfu-preview-current 'insert)    ; current candidate preview
  (corfu-preselect 'prompt)      ; Preselect the prompt
  (corfu-on-exact-match nil)     ; Configure handling of exact matches
  (corfu-scroll-margin 5)        ; Use scroll margin

  :bind
  ;; For Tab and Go
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)))

;; cape
(use-package cape
  :defer t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; eglot
(use-package eglot
  :ensure nil
  ;; key-bindings
  :bind (("C-c l l" . eglot)
	 ("C-c l a" . eglot-code-actions)
	 ("C-c l r" . eglot-rename)
	 ("C-c l f" . eglot-format))
  :custom
  (eglot-autoshutdown t "Automatically shutdown")
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-sync-connect nil)
  (eldoc-message-function #'message)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider)
   "Ignore some functionalities"))

;; Function to bundle backends
(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     #'cape-file))))

(setq completion-category-overrides '((eglot (styles orderless))))
(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

(use-package lua-mode)

(use-package markdown-mode)

(use-package conda
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-autoactivate-mode t))

;; project
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '("requirements.txt" "Gemfile" "autogen.sh" "pom.xml" "package.json" ".git" ".project")))

(require 'latex)
(setq TeX-engine-alist '((default
                          "Tectonic"
                          "tectonic -X compile -f plain %T"
                          "tectonic -X watch"
                          nil)))
(setq LaTeX-command-style '(("" "%(latex)")))
(setq TeX-process-asynchronous t
      TeX-check-TeX nil
      TeX-engine 'default)
(let ((tex-list (assoc "TeX" TeX-command-list))
      (latex-list (assoc "LaTeX" TeX-command-list)))
  (setf (cadr tex-list) "%(tex)"
        (cadr latex-list) "%l"))

;; Org
(require 'indian-holidays)		; Indian Holidays
(require 'org-tempo)			; Templates for Org

(use-package org
  :custom
                                        ; Org documents
  (org-directory "~/proj/mtech/org")
  (org-ellipsis " …" "Nicer ellipsis")
  (org-tags-column 1 "Tags next to header")
  (org-cycle-separator-lines 2  "empty lines between sections")
  (org-fontify-quote-and-verse-blocks t "face for quote and verse")
  (org-indent-indentation-per-level 2 "Indentation per level")
  (org-image-actual-width nil "Resize image to window width")
  (org-outline-path-complete-in-steps nil "No steps in path display")

                                        ; Interaction
  (org-link-use-indirect-buffer-for-internals t "links")
  (org-return-follows-link nil "Follow links with return")
  (org-indirect-buffer-display 'other-window "Tab to expand in a window")
  ;; (org-latex-create-formula-image-program 'dvisvgm "Better Latex preview")
  (org-use-tag-inheritance nil "Tags inheritence")
  (org-use-property-inheritance t "Properties inheritence")
  (org-latex-with-hyperref nil)

                                        ; Agenda
  (org-agenda-todo-keyword-format "%-4s" "Spacious format")
  (org-agenda-files '("~/proj/mtech/org/agenda.org"))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
  (org-log-done 'time) ; log the time of completion
  (org-agenda-start-with-log-mode t)
  (org-log-into-drawer t)
  (org-refile-targets
   '(("~/proj/mtech/org/archive.org" :maxlevel . 1)))
  (calendar-holidays holiday-indian-holidays)
  (org-agenda-custom-commands	       ; Configure custom agenda views
   `(("d" "Dashboard"
      ((agenda "" ((org-agenda-span 1)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-highlight-todo)
                   (org-scheduled-past-days 2)
                   (org-agenda-format-date "")
                   (org-agenda-overriding-header "Today's Agenda")))

       (agenda "" ((org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "+1d")
                   (org-agenda-span 3)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "\nNext three days")))

       (agenda "" ((org-agenda-time-grid nil)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day "+4d")
                   (org-agenda-span 14)
                   (org-agenda-show-all-dates nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "\nUpcoming deadlines (+14d)")))))))
  (org-capture-templates
   `(("t" "Tasks / Projects")
     ("tw" "Task" entry (file+headline "~/proj/mtech/org/agenda.org" "WORK")
      "** TODO %^{task} :work:\nSCHEDULED: %^t\n%^{desc}%?\n" :empty-lines 1)
     ("tp" "Task" entry (file+headline "~/proj/mtech/org/agenda.org" "PERS")
      "** TODO %^{task} :per:\nSCHEDULED: %^t\n%^{desc}%?\n" :empty-lines 1)
     ))

                                        ; Export
  (org-src-fontify-natively t "Fontify code in code blocks")
  (org-adapt-indentation nil "Adaptive indentation")
  (org-src-tab-acts-natively t "Tab acts as in source editing")
  (org-confirm-babel-evaluate nil "No confirmation before executing code")
  (org-edit-src-content-indentation 0 "No relative indentation for code blocks")
  (org-fontify-whole-block-delimiter-line t "Fontify whole block")
  :config
  ;; save org buffers after refilling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  )

(require 'ox-latex)
;; Org latex classes
(add-to-list 'org-latex-classes
	     '("IEEEtran"
	       "\\documentclass[11pt]{IEEEtran}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	    '("beamer"
		"\\documentclass[presentation]{beamer}"
			"[DEFAULT-PACKAGES]"
			"[PACKAGES]"
			"[EXTRA]\n"
		("\\section{%s}" . "\\section*{%s}")
		("\\subsection{%s}" . "\\subsection*{%s}")
		("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(setq org-latex-pdf-process '("tectonic %f"))
(setq org-latex-caption-above nil)	; caption position

(require 'ox-beamer)

(use-package org-auto-tangle		; Auto tangle after saving
  :hook (org-mode . org-auto-tangle-mode)
  :custom
  (org-auto-tangle-default t))


(use-package org-modern			; Modern UI for Org
  :custom
  (org-modern-hide-stars t))

;; Welcome message
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))
(setq initial-scratch-message "")

;; Some basic settings
(setq frame-title-format '("%b")
      ring-bell-function 'ignore
      use-short-answers t
      native-compile-prune-cache t
      make-backup-files nil
      backup-inhibited nil
      create-lockfiles nil
      display-line-numbers-type 'relative
      custom-file (make-temp-file "emacs-custom-")
      completion-cycle-threshold 3
      tab-always-indent 'complete
      doc-view-resolution 200)

;; keep folders clean
(setq backup-directory-alist `(("." ., (expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
;; disable lock files
(setq create-lockfiles nil)

;; Line number and Line-wrap
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)

;; Highlight Current line
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Enable/Disable features
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))
(dolist (c '(eshell project-eshell overwrite-mode iconify-frame diary))
  (put c 'disabled t))

;; Recent files
(require 'recentf)
(recentf-mode 1)

;; Evil Window bindings
(winner-mode 1)
(nvmap :prefix "SPC"
       ;; Window splits
       "w c"   '(evil-window-delete :which-key "Close window")
       "w n"   '(evil-window-new :which-key "New window")
       "w s"   '(evil-window-split :which-key "Horizontal split window")
       "w v"   '(evil-window-vsplit :which-key "Vertical split window")
       ;; Window motions
       "w h"   '(evil-window-left :which-key "Window left")
       "w j"   '(evil-window-down :which-key "Window down")
       "w k"   '(evil-window-up :which-key "Window up")
       "w l"   '(evil-window-right :which-key "Window right")
       "w w"   '(evil-window-next :which-key "Goto next window")
       ;; winner mode
       "w <left>"  '(winner-undo :which-key "Winner undo")
       "w <right>" '(winner-redo :which-key "Winner redo")
       ;; Theme toggle
       "t t" '(ef-themes-toggle :which-key "Toggle Theme")
       "g g" '(magit-status :which-key "Get Magit status")
)

;; Good Key Bindings
(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-c a" #'org-agenda)
(bind-key "C-c c c" #'org-capture)
(bind-key "C-c r" #'recentf)

;;; init.el ends here
