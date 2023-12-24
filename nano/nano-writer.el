;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers 
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
(require 'org)
(require 'org-indent)
(require 'org-element)
(require 'nano-base-colors)
(require 'nano-faces)

;; 
;;             prefix  
;;           |<------>|

;; border -> |<------>| * Headline level 1  # Unnumbered
;;           |<------>| 1 Headline level 1  # Numbered
;;
;;           |<----->| ** Headline level 2  # Unnumbered
;;           |<---->| 1.1 Headline level 2  # Numbered
;;
;;           |<---->| *** Headline level 3  # Unumbered
;;           |<-->| 1.1.1 Headline level 3  # Numbered
;; etc.
;;
;; This works if the number of sections at a given level is < 10.


(defun writer-mode--num-format (numbering)
  "Alternative numbering format for org-num.

First level: 1 | xxx
Second level: 1.1 — xxx
Third level: 1.1.1 - xxx
etc.
"""
  (if (= (length numbering) 1)
      (propertize (concat (mapconcat
                           #'number-to-string
                           numbering ".") " | " )
                  'face `(:family "Roboto Condensed"
                          :height 250
                          :foreground ,nano-color-faded))
    (propertize (concat (mapconcat
                         #'number-to-string
                         numbering ".") " — " )
                'face `(:family "Roboto Condensed"
                        :foreground ,nano-color-faded))))

;; Specific face for headline stars
(font-lock-add-keywords 'writer-mode
             '(("^*+ " 0 `(:family "Roboto Mono"
                           :height 140
                           :foreground ,nano-color-faded) prepend)
                ) 'append)

(defun writer-mode--compute-prefixes ()
  "Compute prefix strings for regular text and headlines."

  (setq org-indent--heading-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--inlinetask-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))

  (let* ((min-indent 5)
         (indent (+ 1 (seq-max 
                       (org-element-map
                           (org-element-parse-buffer) 'headline
                         #'(lambda (item)
                             (org-element-property :level item))))))
         (indent (max indent min-indent)))

    (dotimes (n org-indent--deepest-level)
      (aset org-indent--heading-line-prefixes n
            (make-string
             (min indent (max 0 (- indent 1 n))) ?\s))
      (aset org-indent--inlinetask-line-prefixes n
            (make-string indent ?\s))
      (aset org-indent--text-line-prefixes n
            (make-string indent ?\s)))))


;;;###autoload
(defvar writer-mode nil
  "Variable to store the current state of writer mode.")

(defun toggle-writer-mode ()
  "Toggle writer mode."
  (interactive)
  (if writer-mode
      (writer-mode-off)
    (writer-mode-on)))

(defun writer-mode-on ()
  "Turn on writer mode."
  (setq writer-mode t)
  (writer-mode)
(message "Writer mode is ON"))

(defun writer-mode-off ()
  "Turn off writer mode."
  (setq writer-mode nil)
  (setq
    ;; Org styling, hide markup etc.
    org-hide-emphasis-markers nil
    org-pretty-entities nil)
  ;; (org-indent-mode -1)
  (org-modern-mode -1)
  (org-mode)
(message "Writer mode is OFF"))

(defun setup-writer-mode-keybindings ()
  "Set up keybindings for writer mode."
  (local-set-key (kbd "C-c w") 'toggle-writer-mode))

(define-derived-mode writer-mode org-mode "NΛNO writer"
  ;; Faces
  (face-remap-add-relative 'org-level-1
                           :overline nano-color-subtle
                           :family "Roboto" :height 180)
  (face-remap-add-relative 'org-level-2
                           :family "Roboto" :height 160)
  (face-remap-add-relative 'org-level-3
                           :family "Roboto" :height 150)
  (face-remap-add-relative 'org-document-info
                           :inherit 'nano-face-faded)
  (face-remap-add-relative 'org-document-title
                           :foreground nano-color-critical
                           :family "Roboto Slab"
                           :height 200
                           :weight 'medium)

  ;; Header line
  (setq header-line-format nil)

  ;; Layout
  (setq fill-column 80)
  (setq-local line-spacing 1)

  ;; settings
  (setq
    ;; Edit settings
    ; org-auto-align-tags nil
    ; org-tags-column 0
    ; org-catch-invisible-edits 'show-and-error
    ; org-special-ctrl-a/e t
    org-insert-heading-respect-content t

    ;; Org styling, hide markup etc.
    org-hide-emphasis-markers t
    org-pretty-entities t)

  (setup-writer-mode-keybindings)
  ;; (org-indent-mode 1)
  (org-modern-mode)
  )

(add-hook 'org-mode-hook 'setup-writer-mode-keybindings)
(provide 'nano-writer)
