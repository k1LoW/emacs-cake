;;; historyf.el --- file history like browser
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2010 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 0.0.1
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;;; Commentary:

;;; TODO
;; historyf-forward-same-mode-history

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup historyf nil
  "file history like browser"
  :group 'lisp
  :prefix "historyf-")

(defvar historyf-history nil
  "File history.")

(defcustom historyf-major-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    c-mode cc-mode c++-mode java-mode
    perl-mode cperl-mode python-mode ruby-mode
    ecmascript-mode javascript-mode js2-mode php-mode css-mode
    makefile-mode sh-mode fortran-mode f90-mode ada-mode
    xml-mode sgml-mode)
  "Target major-mode."
  :type 'list
  :group 'historyf)

(defvar historyf-minor-modes
  '(cake)
  "Target minor-mode."
  :type 'list
  :group 'historyf)

(defvar historyf-mark nil)

(defvar historyf-forward-temp nil)

(defadvice switch-to-buffer (before historyf-switch-to-buffer activate)
  (historyf-push-history))

(defun historyf-push-history ()
  "Push file history."
  (let ((active-modes (historyf-active-mode-list)))
    (unless (not active-modes)
      (historyf-clear-head)
      (push (random) active-modes)      ; set random
      (push (cons active-modes (expand-file-name (buffer-file-name))) historyf-history))))

(defun historyf-clear-head ()
  "Clear head history."
  (unless (not historyf-mark)
    (setq historyf-history (cdr (memq historyf-mark historyf-history)))
    (setq historyf-mark nil)))

(defun historyf-back (&optional mode-list)
  "Back file history."
  (interactive)
  (let ((temp-hist))
    (if (not mode-list)
        (if historyf-mark
            (setq temp-hist (cadr (memq historyf-mark historyf-history)))
          (setq temp-hist (car historyf-history)))
      (setq hist (cdr (if historyf-mark
                          (memq historyf-mark historyf-history)
                        historyf-history)))
      (mapc (lambda (h)
              (if (and (intersection (car h) mode-list)
                       (not temp-hist))
                  (setq temp-hist h)))
            hist))
    (unless (not temp-hist)
      (setq historyf-mark nil)
      (find-file (cdr temp-hist))
      (setq historyf-mark temp-hist)
      (setq historyf-forward-temp (pop historyf-history)))))

(defun historyf-back-same-mode-history ()
  "Back same mode file history."
  (interactive)
  (let ((active-modes (historyf-active-mode-list)))
    (historyf-back active-modes)))

(defun historyf-forward ()
  "Forward file history."
  (interactive)
  (let* ((hist-count (length historyf-history))
         (forward-count (unless (not historyf-mark)
                          (- hist-count (length (memq historyf-mark historyf-history))))))
    (unless (not forward-count)
      (setq historyf-mark nil)
      (if (not (equal forward-count 0))
          (find-file (cdr (nth (- forward-count 1) historyf-history)))
        (find-file (cdr historyf-forward-temp))
        (setq historyf-forward-temp nil))
      (pop historyf-history)            ; pop self
      (setq historyf-mark (nth (- forward-count 1) historyf-history)))))

(defun historyf-clear-history ()
  "Clear file history."
  (interactive)
  (setq historyf-history nil)
  (setq historyf-mark nil)
  (setq historyf-forward-temp nil))

(defun historyf-active-mode-list ()
  "Active mode list."
  (let ((active-major-mode (historyf-active-major-mode))
        (active-minor-modes (historyf-active-minor-mode-list))
        (active-modes))
    (if active-major-mode
        (push active-major-mode active-modes))
    (if active-minor-modes
        (setq active-modes (union active-minor-modes active-modes)))
    active-modes))

(defun historyf-active-major-mode ()
  "Active major-mode"
  (if (and (buffer-file-name)
           (memq major-mode historyf-major-modes))
      major-mode
    nil))

(defun historyf-active-minor-mode-list ()
  "Active minor-mode list"
  (let ((active-minor-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-minor-modes mode))
                           (error nil) ))
          minor-mode-list)
    (if (and (buffer-file-name)
             (intersection active-minor-modes historyf-minor-modes))
        (intersection active-minor-modes historyf-minor-modes)
      nil)))

(provide 'historyf)
;;; historyf.el ends here