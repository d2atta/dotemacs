;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
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

(setq-default frame-resize-pixelwise t
	      frame-inhibit-implied-resize t
	      inhibit-splash-screen t	; splash screen
	      inhibit-startup-screen t	; startup screen
	      inhibit-x-resources t	; load x-resources
	      inhibit-startup-buffer-menu t
	      select-enable-clipboard t ; merge clipboard
	      help-window-select t	; focus help menu
	      scroll-conservatively 101 ; scroll recentering
	      scroll-margin 2 ; Add a margin when scrolling vertically
	      recenter-positions '(5 bottom)) ; re-centering positions

(setq-default show-help-function nil	; No help text
	      use-file-dialog nil       ; No file dialog
	      use-dialog-box nil        ; No dialog box
	      pop-up-windows nil)       ; No popup windows
(menu-bar-mode -1)			; No menu bar
(tooltip-mode -1)			; No tooltips
(scroll-bar-mode -1)			; No scroll bars
(tool-bar-mode -1)			; No toolbar

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))

;; Garbage collection
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil
      vc-handled-backends nil
      package-enable-at-startup t)
;;; early-init.el ends here
