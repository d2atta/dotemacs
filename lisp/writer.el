(require 'org)
(require 'org-indent)
(require 'org-element)

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

(define-derived-mode writer-mode org-mode "WriteR"
  ;; Faces
  (face-remap-add-relative 'org-level-1 :height 180)
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
