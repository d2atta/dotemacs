;;; Writer.el --- Writing mode for Org -*- lexical-binding: t -*-
;;; Copyright (c) 2020-2023  Debarghya Datta <info@devildev.me>

;; Author: Debarghya Datta <info@devildev.me>
;; Version: 0.0.1
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
;; Major mode for viewing Org documents with
;; custom fonts and icons.

;;; Code:
(require 'org)
(require 'org-indent)
(require 'org-element)
(require 'ef-themes)

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
  (setq header-line-format nil)
(message "Writer mode is ON"))

(defun writer-mode-off ()
  "Turn off writer mode."
  (setq writer-mode nil)
  (setq
    ;; Org styling, hide markup etc.
    org-hide-emphasis-markers nil
    org-pretty-entities nil)
  (org-indent-mode -1)
  (org-modern-mode -1)
  (org-mode)
(message "Writer mode is OFF"))

(defun setup-writer-mode-keybindings ()
  "Set up keybindings for writer mode."
  (local-set-key (kbd "C-c w") 'toggle-writer-mode))

(define-derived-mode writer-mode org-mode "Writer"
  ;; Faces
  (face-remap-add-relative 'org-level-1
			   :height 180
			   :overline (ef-themes-get-color-value 'rainbow-1))
  (face-remap-add-relative 'org-level-2
			   :height 160)
  (face-remap-add-relative 'org-level-3
			   :height 150)
  (face-remap-add-relative 'org-document-title
			   :height 200
			   :weight 'medium)

  ;; Header line
  (setq header-line-format nil)

  ;; Layout
  (setq fill-column 80)
  (setq-local line-spacing 1)

  ;; settings
  (setq
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-pretty-entities t)


  (setup-writer-mode-keybindings)
  (org-indent-mode 1)
  (org-modern-mode))

(add-hook 'org-mode-hook 'setup-writer-mode-keybindings)
(provide 'writer)

;;; writer.el ends here
