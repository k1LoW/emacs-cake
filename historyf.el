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

;; Version: 0.0.3
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'historyf)
;;

;;; Commentary:

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `historyf-back'
;;    Back file history.
;;  `historyf-back-same-mode-history'
;;    Back same mode file history.
;;  `historyf-forward'
;;    Forward file history.
;;  `historyf-forward-same-mode-history'
;;    Forward same mode file history.
;;  `historyf-clear-history'
;;    Clear file history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `historyf-major-modes'
;;    Target major-mode.
;;    default = (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode ...))
;;  `historyf-minor-modes'
;;    Target minor-mode.
;;    default = (quote (cake))
;;  `historyf-limit'
;;    File history limit.
;;    default = 100

;;; TODO
;; Fix some bug historyf-forward

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup historyf nil
  "File history like browser"
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

(defcustom historyf-minor-modes
  '(cake)
  "Target minor-mode."
  :type 'list
  :group 'historyf)

(defcustom historyf-limit 100
  "File history limit."
  :type 'inter
  :group 'historyf)

(defvar historyf-mark nil)

(defvar historyf-forward-temp nil)

(defadvice switch-to-buffer (before historyf-switch-to-buffer activate)
  (historyf-push-history))

(defun historyf-push-history ()
  "Push file history."
  (let ((active-modes (historyf-active-mode-list))
        (file (buffer-file-name)))
    (unless (not active-modes)
      (unless (equal (expand-file-name file) (cdar historyf-history))
        (historyf-clear-head)
        (push (random) active-modes)
        (push (cons active-modes (expand-file-name (buffer-file-name))) historyf-history)
        (unless (< (length historyf-history) historyf-limit)
          (setq historyf-history (subseq historyf-history 0 (decf historyf-limit))))))))

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
        ;; no mode-list
        (if historyf-mark
            (setq temp-hist (cadr (memq historyf-mark historyf-history)))
          (setq temp-hist (car historyf-history)))
      ;; else
      (setq hist (if historyf-mark
                     (cdr (memq historyf-mark historyf-history))
                   historyf-history))
      (mapc (lambda (h)
              (if (and (intersection (car h) mode-list)
                       (not temp-hist))
                  (setq temp-hist h)))
            hist))
    (unless (not temp-hist)
      (setq historyf-mark nil)
      (find-file (cdr temp-hist))
      (setq historyf-mark temp-hist)
      (if historyf-forward-temp
          (pop historyf-history)
        (setq historyf-forward-temp (pop historyf-history))))))

(defun historyf-back-same-mode-history ()
  "Back same mode file history."
  (interactive)
  (let ((active-modes (historyf-active-mode-list)))
    (historyf-back active-modes)))

(defun historyf-forward (&optional mode-list)
  "Forward file history."
  (interactive)
  (let* ((temp-hist)
         (history-head (unless (not historyf-mark)
                         (subseq historyf-history 0 (position historyf-mark historyf-history)))))
    (if (not mode-list)
        ;; no mode-list
        (unless (not history-mark)
          (setq historyf-mark nil)
          (if history-head
              (find-file (cdar (reverse history-head)))
            (unless (not historyf-forward-temp)
              (find-file (cdr historyf-forward-temp))
              (setq historyf-forward-temp nil)))
          (pop historyf-history)
          (setq historyf-mark (car (reverse history-head))))
      ;; else
      (unless (not history-mark)
        (setq historyf-mark nil)
        (if history-head
            (progn
              (mapc (lambda (h)
                      (if (and (intersection (car h) mode-list)
                               (not temp-hist))
                          (setq temp-hist h)))
                    (reverse history-head))
              (unless (not temp-hist)
                (find-file (cdr temp-hist))
                (pop historyf-history)
                (setq historyf-mark temp-hist)
                ))
          (unless (not (and historyf-forward-temp
                            (intersection (car historyf-forward-temp) mode-list)))
            (find-file (cdr historyf-forward-temp))
            (setq historyf-forward-temp nil)
            (pop historyf-history)
            (setq historyf-mark nil)))))))

(defun historyf-forward-same-mode-history ()
  "Forward same mode file history."
  (interactive)
  (let ((active-modes (historyf-active-mode-list)))
    (historyf-forward active-modes)))

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
  "Active major-mode."
  (if (and (buffer-file-name)
           (memq major-mode historyf-major-modes))
      major-mode
    nil))

(defun historyf-active-minor-mode-list ()
  "Active minor-mode list."
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